package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"strings"

	"github.com/blanu/Dust/go/Dust"
	"github.com/blanu/Dust/go/Dust/procman"

	_ "github.com/blanu/Dust/go/DustModel/sillyHex"
)

const progName = "DustProxy"
const usageMessageRaw = `
Usage: DustProxy --incomplete PROXY-SPEC

Options:
  --incomplete
	Acknowledge that this executable does not implement
	the full Dust stack and is just a shell of its future
	self.  Required.

Proxy specification syntax:
  in IDENTITY-FILE PROGRAM ARGS...
	Using the address, port, and private key in IDENTITY-FILE,
	listen for Dust connections.  For each such connection,
	spawn a process running PROGRAM with ARGS and attach the
	connection to its stdin/stdout.
  out ADDR:PORT PARAMS...
	Connect to the Dust server at ADDR:PORT, using PARAMS as
	though from a torrc Bridge line.  Attach the connection to
	stdin/stdout of this process.

Models available:$models
`

var ourFlags *flag.FlagSet

func modelsReadable() string {
	result := ""
	for _, name := range Dust.ModelsAvailable() {
		result += "\n  " + name
	}
	return result
}

func usageMessage() string {
	template := strings.TrimLeft(usageMessageRaw, "\n")
	replacements := []string{
		"$models", modelsReadable(),
	}
	return strings.NewReplacer(replacements...).Replace(template)
}

func usageErrorf(detailFmt string, detailArgs ...interface{}) {
	detail := fmt.Sprintf(detailFmt, detailArgs...)
	fmt.Fprintf(os.Stderr, "%s: %s\n%s", progName, detail, usageMessage())
	os.Exit(64)
}

func exitError(err error) {
	fmt.Fprintf(os.Stderr, "%s: %s\n", progName, err.Error())
	os.Exit(1)
}

var argI int = 0

func nextArg(expected string) string {
	if !(argI < ourFlags.NArg()) {
		usageErrorf("not enough arguments; expected %s", expected)
	}
	arg := ourFlags.Arg(argI)
	argI++
	return arg
}

func remainingArgs() []string {
	slice := ourFlags.Args()[argI:]
	argI = ourFlags.NArg()
	return slice
}

func endOfArgs() {
	if argI < ourFlags.NArg() {
		usageErrorf("too many arguments at %d (\"%s\")", argI, ourFlags.Arg(argI))
	}
}

type ioPair struct {
	rd io.ReadCloser
	wr io.WriteCloser
}

func (pair ioPair) Read(p []byte) (n int, err error) {
	return pair.rd.Read(p)
}

func (pair ioPair) Write(p []byte) (n int, err error) {
	return pair.wr.Write(p)
}

func (pair ioPair) Close() error {
	err1 := pair.rd.Close()
	err2 := pair.wr.Close()
	if err1 != nil {
		return err1
	} else {
		return err2
	}
}

type (
	readerHalf struct {
		conn *net.TCPConn
	}
	writerHalf struct {
		conn *net.TCPConn
	}
)

func (r *readerHalf) Read(p []byte) (n int, err error) {
	return r.conn.Read(p)
}

func (r *readerHalf) Close() error {
	return r.conn.CloseRead()
}

func (w *writerHalf) Write(p []byte) (n int, err error) {
	return w.conn.Write(p)
}

func (w *writerHalf) Close() error {
	return w.conn.CloseWrite()
}

func stdioPair() ioPair {
	return ioPair{os.Stdin, os.Stdout}
}

func tcpPair(tcp *net.TCPConn) ioPair {
	return ioPair{&readerHalf{tcp}, &writerHalf{tcp}}
}

func spawnPair(prog string, progArgs []string) (result ioPair, err error) {
	var rd io.ReadCloser
	var wr io.WriteCloser
	defer func() {
		if err != nil {
			if rd != nil {
				_ = rd.Close()
			}
			if wr != nil {
				_ = wr.Close()
			}
		}
	}()

	cmd := exec.Command(prog, progArgs...)
	rd, err = cmd.StdoutPipe()
	if err != nil {
		return
	}
	wr, err = cmd.StdinPipe()
	if err != nil {
		return
	}

	err = cmd.Start()
	if err != nil {
		return
	}

	result = ioPair{rd, wr}
	return
}

func doProxy(dconn Dust.Connection, plainSide ioPair) error {
	// TODO: on client side, doesn't hang around for full duration
	defer dconn.Close()

	copyThunk := func(dst io.Writer, src io.Reader) func() error {
		return func() error {
			// TODO: this doesn't respect kill signaling
			_, err := io.Copy(dst, src)
			return err
		}
	}

	var inCopy, outCopy procman.Link
	inCopy.InitLink(copyThunk(plainSide.wr, dconn))
	outCopy.InitLink(copyThunk(dconn, plainSide.rd))
	defer inCopy.CloseDetach()
	defer outCopy.CloseDetach()
	inCopy.Spawn()
	outCopy.Spawn()

	select {
	case _, _ = <-inCopy.Exit:
	case _, _ = <-outCopy.Exit:
		//case _, _ = <-dconn.Exit:
	}

	// TODO: error propagation
	return nil
}

func incomingSpawn(conn *net.TCPConn, spriv *Dust.ServerPrivate, prog string, progArgs []string) error {
	commit := false
	defer func() {
		if !commit {
			conn.Close()
		}
	}()

	dconn, err := Dust.BeginServer(conn, spriv)
	if err != nil {
		return err
	}
	defer func() {
		if !commit {
			dconn.Close()
		}
	}()

	// TODO: clean up spawned processes properly
	plainSide, err := spawnPair(prog, progArgs)
	if err != nil {
		return err
	}

	commit = true
	go doProxy(dconn, plainSide)
	return nil
}

func listenAndSpawn(spriv *Dust.ServerPrivate, prog string, progArgs []string) error {
	listener, err := net.ListenTCP("tcp", spriv.ListenAddr())
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "Listening on %s\n", listener.Addr().String())
	defer listener.Close()

	for {
		conn, err := listener.AcceptTCP()
		if err != nil {
			return err
		}

		// TODO: be able to drop connections except from a specific remote address,
		// to limit visibility during testing

		_ = incomingSpawn(conn, spriv, prog, progArgs)
	}
}

func listenFromArgs() (func() error, error) {
	idPath := nextArg("IDENTITY-FILE")
	spriv, err := Dust.LoadServerPrivateFile(idPath)
	if err != nil {
		return nil, err
	}

	prog := nextArg("PROGRAM")
	progArgs := remainingArgs()
	return func() error { return listenAndSpawn(spriv, prog, progArgs) }, nil
}

func dialAndStdio(spub *Dust.ServerPublic) error {
	conn, err := net.DialTCP("tcp", nil, spub.DialAddr())
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "Connected to %s\n", conn.RemoteAddr().String())

	// TODO: the Dust side should probably have most of its initialization happen before the dial so that
	// there's less risk of dialing visibly and then getting hosed on some other part of initialization.

	dconn, err := Dust.BeginClient(conn, spub)
	if err != nil {
		return err
	}

	return doProxy(dconn, stdioPair())
}

func dialFromArgs() (func() error, error) {
	connString := nextArg("CONNECT-ADDRESS")

	params := make(map[string]string)
	for _, param := range remainingArgs() {
		pair := strings.SplitN(param, "=", 2)
		if len(pair) != 2 {
			return nil, errors.New("malformed bridge-like parameter (expected equals sign)")
		}

		params[pair[0]] = pair[1]
	}

	spub, err := Dust.LoadServerPublicBridgeLine(Dust.BridgeLine{connString, params})
	if err != nil {
		return nil, err
	}

	return func() error { return dialAndStdio(spub) }, nil
}

func main() {
	var err error
	ourFlags = flag.NewFlagSet(progName, flag.ContinueOnError)
	ourFlags.Usage = func() {
		io.WriteString(os.Stderr, usageMessage())
	}

	// Usage strings are hardcoded above.
	incompleteAck := ourFlags.Bool("incomplete", false, "")
	argErr := ourFlags.Parse(os.Args[1:])
	if argErr == flag.ErrHelp {
		io.WriteString(os.Stdout, usageMessage())
		os.Exit(0)
	} else if argErr != nil {
		usageErrorf("%s", argErr.Error())
	}

	var requestedCommand func() error
	dirArg := nextArg("DIRECTION")
	switch dirArg {
	default:
		usageErrorf("bad direction \"%s\"", dirArg)
	case "in":
		requestedCommand, err = listenFromArgs()
	case "out":
		requestedCommand, err = dialFromArgs()
	}

	if err != nil {
		exitError(err)
	}

	if !*incompleteAck {
		usageErrorf("this executable is incomplete; you must use --incomplete")
	}

	err = requestedCommand()
	if err != nil {
		exitError(err)
	}
}
