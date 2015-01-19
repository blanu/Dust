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
	"github.com/blanu/Dust/go/DustModel/sillyHex"
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
`

var ourFlags *flag.FlagSet

func usageMessage() string {
	return strings.TrimLeft(usageMessageRaw, "\n")
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

func copyThen(dst io.Writer, src io.Reader, errorOut *error, afterThunk func()) {
	defer func() {
		// TODO: move ReportExitTo somewhere reasonable (is there a stdlib equivalent for this?  Is it
		// bad?)
		Dust.ReportExitTo(errorOut)
		afterThunk()
	}()
	
	_, err := io.Copy(dst, src)
	*errorOut = err
}

func doProxy(cs *Dust.CryptoSession, dustSide ioPair, plainSide ioPair) error {
	codec := sillyHex.NewSillyHexCodec()
	
	shaper, err := Dust.NewShaper(cs, dustSide.rd, codec, dustSide.wr, codec)
	if err != nil {
		return err
	}

	exitChan := make(chan interface{}, 4)
	shaper.SpawnThen(func() { exitChan <- shaper })
	var inCopyError, outCopyError error
	go copyThen(plainSide.wr, cs, &inCopyError, func() { exitChan <- plainSide.wr })
	go copyThen(cs, plainSide.rd, &outCopyError, func() { exitChan <- plainSide.rd })

	for i := 0; i < 3; i++ {
		_ = <-exitChan
	}

	shaperError := shaper.Error()
	if shaperError != nil {
		return shaperError
	} else if inCopyError != nil {
		return inCopyError
	} else if outCopyError != nil {
		return outCopyError
	} else {
		return nil
	}
}

func incomingSpawn(conn *net.TCPConn, spriv *Dust.ServerPrivate, prog string, progArgs []string) error {
	commit := false
	defer func() {
		if !commit {
			conn.Close()
		}
	}()

	cs, err := Dust.BeginCryptoServer(spriv)
	if err != nil {
		return err
	}
		
	// TODO: clean up spawned processes properly
	dustSide := tcpPair(conn)
	plainSide, err := spawnPair(prog, progArgs)
	if err != nil {
		return err
	}

	commit = true
	go doProxy(cs, dustSide, plainSide)
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
	cs, err := Dust.BeginCryptoClient(spub)
	if err != nil {
		return err
	}

	conn, err := net.DialTCP("tcp", nil, spub.DialAddr())
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "Connected to %s\n", conn.RemoteAddr().String())
	
	dustSide := tcpPair(conn)
	plainSide := stdioPair()
	return doProxy(cs, dustSide, plainSide)
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
