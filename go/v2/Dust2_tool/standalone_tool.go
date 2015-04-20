// Copyright © 2015 Drake Wilson.  Copying, distribution, and modification of this software is governed by
// the MIT-style license in the file ../../LICENSE.md.

package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/blanu/Dust/go/v2/interface"

	_ "github.com/blanu/Dust/go/sillyHex"
)

const progName = "Dust2_tool"
const usageMessageRaw = `
Usage: Dust2_tool OPTIONS SUBCOMMAND...

Subcommands:
  newid -o FILE PARAMS...
	Generate a new Dust server identity for communicating using PARAMS.
	Each PARAM must be of the form KEY=VALUE.  Write private information
	to the new file FILE, and write a pseudo-bridge-line to standard
	output.

  params FILE
	Read a Dust server private identity from FILE and write its parameters
	to standard output in pseudo-bridge-line form.
`
// TODO: $models

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

func formatParams(p map[string]string) string {
	parts := []string{}
	for key, val := range p {
		parts = append(parts, key + "=" + val)
	}
	return strings.Join(parts, " ")
}

func showParams(spub *Dust.ServerPublic) {
	fmt.Fprintf(os.Stdout, "Bridge Dust2 ADDRESS %s\n", formatParams(spub.Unparse()))
}

func newidToFile(path string, unparsed map[string]string) (err error) {
	var spriv *Dust.ServerPrivate
	var ep *Dust.EndpointParams

	ep, err = Dust.ParseEndpointParams(unparsed)
	if err != nil {
		return
	}

	spriv, err = Dust.NewServerPrivate(ep)
	if err != nil {
		return
	}

	if err = spriv.SavePrivateFile(path); err != nil {
		return
	}

	showParams(spriv.Public())
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
	paramArgs := remainingArgs()

	unparsed := make(map[string]string)
	for _, arg := range paramArgs {
		equals := strings.IndexRune(arg, '=')
		if equals < 0 {
			usageErrorf("bridge parameter must be of the form KEY=VALUE")
		}

		key, val := arg[:equals], arg[equals+1:]
		unparsed[key] = val
	}

	if *outputPathPtr == "" {
		usageErrorf("output file must be specified")
	}

	return func() error {
		return newidToFile(*outputPathPtr, unparsed)
	}, nil
}

func showParamsFromFile(path string) error {
	spriv, err := Dust.LoadServerPrivateFile(path)
	if err != nil {
		return err
	}

	showParams(spriv.Public())
	return nil
}

func showParamsFromArgs() (func() error, error) {
	// TODO: in DustProxy, do the same sort of thing here (when this is all refactored) so that subcommands
	// can parse their own options.
	subFlags := flag.NewFlagSet(progName, flag.ContinueOnError)
	subFlags.Usage = func() {}
	subFlags.SetOutput(&nullWriter{})

	argErr := subFlags.Parse(remainingArgs())
	if argErr == flag.ErrHelp {
		io.WriteString(os.Stdout, usageMessage())
		os.Exit(0)
	} else if argErr != nil {
		usageErrorf("%s", argErr.Error())
	}

	ourFlags = subFlags
	argI = 0
	path := nextArg("FILE")
	endOfArgs()

	return func() error {
		return showParamsFromFile(path)
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
	case "params":
		requestedCommand, err = showParamsFromArgs()
	}

	if err != nil {
		exitError(err)
	}

	err = requestedCommand()
	if err != nil {
		exitError(err)
	}
}
