package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/blanu/Dust/go/Dust"
)

const progName = "DustTool"
const usageMessageRaw = `
Usage: DustTool SUBCOMMAND...

Subcommands:
  newid -o FILE NICKNAME HOST:PORT PARAMS...
    Generate a new Dust server identity for a server named NICKNAME,
    to listen at HOST:PORT, communicating using PARAMS.  Each PARAM must
    be of the form KEY=VALUE.  Write private information to the new file
    FILE and output a full bridge line on standard output.

    Currently, PARAMS are not fully validated to make sure that Dust
    connections could reasonably be established with them.
`

type nullWriter struct{}

func (n *nullWriter) Write(p []byte) (int, error) {
	return len(p), nil
}

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

func formatBridgeLine(bline Dust.BridgeLine) string {
	parts := []string{
		bline.Nickname,
		bline.Address,
	}
	for k, v := range bline.Params {
		parts = append(parts, k+"="+v)
	}

	return strings.Join(parts, " ")
}

func newidToFile(path string, bline Dust.BridgeLine) error {
	var spriv *Dust.ServerPrivate
	var err error
	defer func() {
		if spriv != nil {
			// TODO: type fixup
			//spriv.DestroyPrivate()
		}
	}()

	spriv, err = Dust.NewServerPrivateBridgeLine(bline)
	if err != nil {
		return err
	}

	if err = spriv.SavePrivateFile(path); err != nil {
		return err
	}

	realBline := spriv.Public().BridgeLine()
	fmt.Fprintf(os.Stdout, "Bridge %s\n", formatBridgeLine(realBline))
	return nil
}

func newidFromArgs() (func() error, error) {
	// TODO: refactor subcommand-flags bit, and refactor this with DustProxy flag handling
	subFlags := flag.NewFlagSet(progName, flag.ContinueOnError)
	subFlags.Usage = func() {}
	subFlags.SetOutput(&nullWriter{})

	outputPathPtr := subFlags.String("o", "", "")

	argErr := subFlags.Parse(remainingArgs())
	if argErr == flag.ErrHelp {
		io.WriteString(os.Stdout, usageMessage())
		os.Exit(0)
	} else if argErr != nil {
		usageErrorf("%s", argErr.Error())
	}

	ourFlags = subFlags
	argI = 0
	nickname := nextArg("NICKNAME")
	addrString := nextArg("ADDR-STRING")
	paramArgs := remainingArgs()

	params := make(map[string]string)
	for _, pairArg := range paramArgs {
		equals := strings.IndexRune(pairArg, '=')
		if equals < 0 {
			usageErrorf("bridge parameter must be of the form KEY=VALUE")
		}

		key := pairArg[:equals]
		val := pairArg[equals+1:]
		params[key] = val
	}

	bline := Dust.BridgeLine{
		Nickname: nickname,
		Address:  addrString,
		Params:   params,
	}

	if *outputPathPtr == "" {
		usageErrorf("output file must be specified")
	}

	return func() error {
		return newidToFile(*outputPathPtr, bline)
	}, nil
}

func main() {
	var err error
	ourFlags = flag.NewFlagSet(progName, flag.ContinueOnError)
	ourFlags.Usage = func() {}
	ourFlags.SetOutput(&nullWriter{})

	argErr := ourFlags.Parse(os.Args[1:])
	if argErr == flag.ErrHelp {
		io.WriteString(os.Stdout, usageMessage())
		os.Exit(0)
	} else if argErr != nil {
		usageErrorf("%s", argErr.Error())
	}

	var requestedCommand func() error
	subcommandArg := nextArg("SUBCOMMAND")
	switch subcommandArg {
	default:
		usageErrorf("unrecognized subcommand \"%s\"", subcommandArg)
	case "newid":
		requestedCommand, err = newidFromArgs()
	}

	if err != nil {
		exitError(err)
	}

	err = requestedCommand()
	if err != nil {
		exitError(err)
	}
}
