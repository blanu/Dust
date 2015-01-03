package main

import (
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"strings"

	"caolta3iaejox3z4madc5wmp4z.uuid/Dust4go/Dust"
)

const progName = "DustProxy"
const usageMessageRaw = `
Usage: DustProxy --incomplete {in|out} {tcp HOST PORT | exec PROGRAM ARGS...}
Options:
  --incomplete
	Acknowledge that this executable does not implement
	the full Dust stack and is just a shell of its future
	self.  Required.

Connection directions:
  in	Convert between Dust on stdin/stdout and plain remote.
  out	Convert between plain stdin/stdout and Dust remote.

Remote types:
  tcp	Make a TCP connection to HOST:PORT.
  exec	Spawn PROGRAM with ARGS and use its stdin/stdout.

Examples with socat:
  Connecting side:
	socat tcp-listen:13686,bind=127.0.0.1,fork,reuseaddr \
	  exec:'DustProxy out tcp dusty-foo-service.example 13687'
  Listening side:
	socat tcp-listen:13687,fork,reuseaddr \
	  exec:'DustProxy in tcp localhost 13686'
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

type direction int

const (
	directionUnknown direction = iota
	directionIn
	directionOut
)

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

func parseTcpRemote(direction direction) func() (ioPair, error) {
	hostArg := nextArg("host")
	portArg := nextArg("port")
	network := "tcp"

	return func() (remote ioPair, err error) {
		var conn net.Conn
		var err_ error
		defer func() {
			if err != nil && conn != nil {
				_ = conn.Close()
			}
		}()
		
		// We combine the host and port early here because that might allow net.Dial to do smarter
		// resolution.  Similarly we assume it'll bomb if it finds the port to be invalid, so no
		// explicit strconv check above.
		if conn, err_ = net.Dial(network, net.JoinHostPort(hostArg, portArg)); err_ != nil {
			err = err_
			return
		}

		// Expect it to really be a TCPConn so we can CloseRead and CloseWrite on it.
		tcp, ok := conn.(*net.TCPConn)
		if !ok {
			panic("somehow, not a TCP connection")
		}

		// We have to wrap the connection because otherwise Close() will close the whole thing when we
		// want to be able to close only one side.  This leaks the socket FD even after both sides
		// have been shut down, but we'd exit soon anyway.  Don't do that in a longer-running process.
		remote = ioPair{&readerHalf{tcp}, &writerHalf{tcp}}
		return
	}
}

func parseExecRemote(direction direction) func() (ioPair, error) {
	prog := nextArg("program")
	progArgs := remainingArgs()

	return func() (remote ioPair, err error) {
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

		// This leaks a zombie subprocess, but that's okay because we'd exit soon anyway.  Don't do
		// that in a longer-running process.
		remote = ioPair{rd, wr}
		return
	}
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

func dustProxy(dustSide ioPair, plainSide ioPair, direction direction) error {
	var cs *Dust.CryptoSession
	
	switch direction {
	default:
		panic("weird direction")
		// TODO: put these cases in separate functions
	case directionIn:
		// INCOMPLETE: hardcoded
		path := strings.Join([]string{os.Getenv("HOME"), "/tmp/foo-key"}, "")
		spriv, err := Dust.LoadCryptoServerPrivateFile(net.IPv4zero, 0, path)
		if err != nil {
			return err
		}
		cs, err = Dust.BeginCryptoServer(spriv)
		if err != nil {
			return err
		}
	case directionOut:
		// INCOMPLETE: hardcoded
		bridgeParams := map[string]string{"p": "TS3JPXCKPOUE4BM4PQCDVZTHKOO3DUT5IQ4BTVBOJRRURMOWFU6Q"}
		sid, err := Dust.LoadCryptoServerIdentityBridgeLine(net.IPv4zero, 0, bridgeParams)
		if err != nil {
			return err
		}
		cs, err = Dust.BeginCryptoClient(sid)
		if err != nil {
			return err
		}
	}

	shaper, err := Dust.NewShaper(cs, dustSide.rd, dustSide.wr)
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

func main() {
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
	
	var direction direction
	dirArg := nextArg("direction")
	switch dirArg {
	default:
		usageErrorf("bad direction \"%s\"", dirArg)
	case "in":
		direction = directionIn
	case "out":
		direction = directionOut
	}

	var beginRemote func() (ioPair, error)
	typArg := nextArg("remote-type")
	switch typArg {
	default:
		usageErrorf("unknown remote-type \"%s\"", typArg)
	case "tcp":
		beginRemote = parseTcpRemote(direction)
	case "exec":
		beginRemote = parseExecRemote(direction)
	}

	endOfArgs()

	if !*incompleteAck {
		usageErrorf("this executable is incomplete; you must use --incomplete")
	}

	local := ioPair{os.Stdin, os.Stdout}
	remote, err := beginRemote()
	if err != nil {
		exitError(err)
	}

	var dustSide, plainSide ioPair
	switch direction {
	default:
		panic("weird direction")
	case directionIn:
		dustSide, plainSide = local, remote
	case directionOut:
		dustSide, plainSide = remote, local
	}

	if err := dustProxy(dustSide, plainSide, direction); err != nil {
		exitError(err)
	}
}
