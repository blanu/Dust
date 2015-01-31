package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"strings"

	"github.com/blanu/Dust/go/Dust"

	_ "github.com/blanu/Dust/go/DustModel/sillyHex"
)

const progName = "DustProxy"
const usageMessageRaw = `
Usage: DustProxy --incomplete OPTIONS DUST-SIDE

Options:
  --incomplete
	Acknowledge that this executable does not implement
	the full Dust stack and is just a shell of its future
	self.  Required.
  --listen HOST:PORT, -l HOST:PORT
	Listen for TCP connections on HOST:PORT.
  --connect HOST:PORT, -c HOST:PORT
	Connect outgoing TCP connections to HOST:PORT.
  --restrict HOST, -r HOST
	Only accept incoming TCP connections from HOST.

Dust side syntax:
  in IDENTITY-FILE
	Listen for Dust connections using the private key and
	parameters in IDENTITY-FILE.  --connect must be used,
	and will receive proxied clear-side connections.  If
	--listen is used, it overrides the physical address.
  out ADDR:PORT PARAMS...
	Connect to the Dust server at ADDR:PORT, using PARAMS as
	though from a bridge line.  --listen must be used, and
	will accept clear-side connections to be proxied.  If
	--connect is used, it overrides the physical address.

Models available:$models
`

var ipv4MappedPrefix = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff}

var ourFlags *flag.FlagSet
var userListenAddr, userDialAddr string
var userRestrictAddr string
var restrictIP *net.IPAddr

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

func tcpAddrEqual(a, b *net.TCPAddr) bool {
	return a.IP.Equal(b.IP) && a.Port == b.Port && a.Zone == b.Zone
}

func ipAddrEqual(a, b *net.IPAddr) bool {
	return a.IP.Equal(b.IP) && a.Zone == b.Zone
}

func parseRestrictAddr(relativeTo *net.TCPAddr) error {
	if userRestrictAddr == "" {
		return nil
	}

	realNet := "ip"
	switch len(relativeTo.IP) {
	default:
		// Someone's invented IPv8.  Just hope for the best; if we get an address that doesn't match
		// the listening family at all, then no connections will be accepted.
	case net.IPv4len:
		realNet = "ip4"
	case net.IPv6len:
		// Sometimes net.IP stores IPv4 addresses in 16-byte slices too.  Sigh.
		if bytes.HasPrefix(relativeTo.IP, ipv4MappedPrefix) {
			realNet = "ip4"
		} else {
			realNet = "ip6"
		}
	}

	// This does not propagate IPv6 scopes; the user must specify the same scope explicitly both times
	// if needed.
	ip, err := net.ResolveIPAddr(realNet, userRestrictAddr)
	if err != nil {
		return err
	}

	restrictIP = ip
	return nil
}

func dustProxy(dustSide Dust.Connection, plainSide *net.TCPConn) error {
	// TODO: re-add process management here when connection-close is handled more effectively.
	go io.Copy(plainSide, dustSide)
	go io.Copy(dustSide, plainSide)
	return nil
}

func listenOn(addr *net.TCPAddr, eachConn func(*net.TCPConn) error) error {
	listener, err := net.ListenTCP("tcp", addr)
	if err != nil {
		return err
	}
	defer listener.Close()

	for {
		conn, err := listener.AcceptTCP()
		if err != nil {
			return err
		}

		var remoteIP *net.IPAddr
		switch a := conn.RemoteAddr().(type) {
		case *net.TCPAddr:
			remoteIP = &net.IPAddr{
				IP:   a.IP,
				Zone: a.Zone,
			}
		}

		if restrictIP != nil && !(remoteIP != nil && ipAddrEqual(remoteIP, restrictIP)) {
			fmt.Fprintf(os.Stderr, "%s: rejecting connection from %s\n", progName, remoteIP.String())
			_ = conn.Close()
			continue
		}

		go eachConn(conn)
	}
}

func dustToPlain(listenAddr, dialAddr *net.TCPAddr, spriv *Dust.ServerPrivate) error {
	eachConn := func(in *net.TCPConn) error {
		dconn, err := Dust.BeginServer(in, spriv)
		if err != nil {
			return err
		}

		out, err := net.DialTCP("tcp", nil, dialAddr)
		if err != nil {
			return err
		}

		return dustProxy(dconn, out)
	}

	return listenOn(listenAddr, eachConn)
}

func dustToPlainFromArgs() (func() error, error) {
	var err error
	var warnings []string

	if userDialAddr == "" {
		usageErrorf("must specify address to connect to")
	}
	dialAddr, err := net.ResolveTCPAddr("tcp", userDialAddr)
	if err != nil {
		return nil, err
	}

	idPath := nextArg("IDENTITY-FILE")
	endOfArgs()

	spriv, err := Dust.LoadServerPrivateFile(idPath)
	if err != nil {
		return nil, err
	}

	listenAddr := spriv.ListenAddr()
	if userListenAddr != "" {
		// Overrides address in identity file.
		physListenAddr, err := net.ResolveTCPAddr("tcp", userListenAddr)
		if err != nil {
			return nil, err
		}

		if !tcpAddrEqual(physListenAddr, listenAddr) {
			warnings = append(warnings, fmt.Sprintf("listening on %s even though identity address is %s", physListenAddr.String(), listenAddr.String()))
		}
		listenAddr = physListenAddr
	}

	err = parseRestrictAddr(listenAddr)
	if err != nil {
		return nil, err
	}

	for _, warning := range warnings {
		fmt.Fprintf(os.Stderr, "%s: warning: %s\n", progName, warning)
	}

	return func() error {
		return dustToPlain(listenAddr, dialAddr, spriv)
	}, nil
}

func plainToDust(listenAddr, dialAddr *net.TCPAddr, spub *Dust.ServerPublic) error {
	eachConn := func(in *net.TCPConn) error {
		out, err := net.DialTCP("tcp", nil, dialAddr)
		if err != nil {
			return err
		}

		// TODO: the Dust side should probably have most of its initialization happen before the dial
		// so that there's less risk of dialing visibly and then getting hosed on some other part of
		// initialization.

		dconn, err := Dust.BeginClient(out, spub)
		if err != nil {
			return err
		}

		return dustProxy(dconn, in)
	}

	return listenOn(listenAddr, eachConn)
}

func plainToDustFromArgs() (func() error, error) {
	var warnings []string

	if userListenAddr == "" {
		usageErrorf("must specify address to listen on")
	}
	listenAddr, err := net.ResolveTCPAddr("tcp", userListenAddr)
	if err != nil {
		return nil, err
	}

	err = parseRestrictAddr(listenAddr)
	if err != nil {
		return nil, err
	}

	connString := nextArg("CONNECT-ADDRESS")

	params := make(map[string]string)
	for _, param := range remainingArgs() {
		equals := strings.IndexRune(param, '=')
		if equals < 0 {
			usageErrorf("malformed bridge-like parameter (expected equals sign)")
		}

		key := param[:equals]
		val := param[equals+1:]
		params[key] = val
	}

	bline := Dust.BridgeLine{
		// No need to set the nickname.
		Address: connString,
		Params:  params,
	}

	spub, err := Dust.LoadServerPublicBridgeLine(bline)
	if err != nil {
		return nil, err
	}

	dialAddr := spub.DialAddr()
	if userDialAddr != "" {
		// Overrides address in bridge line.
		physDialAddr, err := net.ResolveTCPAddr("tcp", userDialAddr)
		if err != nil {
			return nil, err
		}

		if !tcpAddrEqual(physDialAddr, dialAddr) {
			warnings = append(warnings, fmt.Sprintf("connecting to %s even though identity address is %s", physDialAddr.String(), dialAddr.String()))
		}
		dialAddr = physDialAddr
	}

	for _, warning := range warnings {
		fmt.Fprintf(os.Stderr, "%s: warning: %s\n", progName, warning)
	}

	return func() error {
		return plainToDust(listenAddr, dialAddr, spub)
	}, nil
}

type nullWriter struct{}

func (n *nullWriter) Write(p []byte) (int, error) {
	return len(p), nil
}

func main() {
	var err error
	ourFlags = flag.NewFlagSet(progName, flag.ContinueOnError)
	ourFlags.Usage = func() {}
	ourFlags.SetOutput(&nullWriter{})

	// Usage strings are hardcoded above.

	var incompleteAck bool
	ourFlags.BoolVar(&incompleteAck, "incomplete", false, "")
	ourFlags.StringVar(&userListenAddr, "listen", "", "")
	ourFlags.StringVar(&userListenAddr, "l", "", "")
	ourFlags.StringVar(&userDialAddr, "connect", "", "")
	ourFlags.StringVar(&userDialAddr, "c", "", "")
	ourFlags.StringVar(&userRestrictAddr, "restrict", "", "")
	ourFlags.StringVar(&userRestrictAddr, "r", "", "")

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
		requestedCommand, err = dustToPlainFromArgs()
	case "out":
		requestedCommand, err = plainToDustFromArgs()
	}

	if err != nil {
		exitError(err)
	}

	if !incompleteAck {
		usageErrorf("this executable is incomplete; you must use --incomplete")
	}

	err = requestedCommand()
	if err != nil {
		exitError(err)
	}
}
