package main

import (
	"errors"
	"fmt"
	"os"
	"strings"
	
	"github.com/blanu/Dust/go/Dust"
)

const progName = "DustTool"

var ErrBadArgs = errors.New("bad arguments")

const usageMessageRaw = `
Usage: DustTool newid FILE HOST:PORT
`

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

func joinBridgeParams(params map[string]string) string {
	parts := make([]string, 0, len(params))
	for k, v := range params {
		parts = append(parts, fmt.Sprintf("%s=%s", k, v))
	}
	return strings.Join(parts, " ")
}

func writeNewIdentity(path string, addrString string) error {
	var spriv *Dust.CryptoServerPrivate
	var err error
	defer func() {
		if spriv != nil {
			spriv.DestroyPrivate()
		}
	}()

	spriv, err = Dust.NewCryptoServerPrivate(addrString)
	if err != nil {
		return err
	}

	if err = spriv.SavePrivateFile(path); err != nil {
		return err
	}
	fmt.Fprintf(os.Stdout, "Identity file saved to: %s\n", path)

	// TODO: clean up handling of address, name, other metadata here?
	realAddrString := spriv.ListenAddr().String()
	paramsString := joinBridgeParams(spriv.BridgeParams())
	fmt.Fprintf(os.Stdout, "Bridge x %s %s\n", realAddrString, paramsString)
	return nil
}

func main() {
	// TODO: proper arg parsing
	if len(os.Args) < 4 {
		usageErrorf("not enough arguments")
	}
	
	subcommand := os.Args[1]
	if subcommand != "newid" {
		usageErrorf("unrecognized subcommand \"%s\"", subcommand)
	}

	path := os.Args[2]
	addrString := os.Args[3]

	err := writeNewIdentity(path, addrString)
	if err != nil {
		exitError(err)
	}
}
