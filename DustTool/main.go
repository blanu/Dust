package main

import (
	"errors"
	"fmt"
	"net"
	"os"
	"strings"
	
	"caolta3iaejox3z4madc5wmp4z.uuid/Dust4go/Dust"
)

const progName = "DustTool"

var ErrBadArgs = errors.New("bad arguments")

func errorExit(err error) {
	exitCode := 1
	if err == ErrBadArgs {
		exitCode = 64
	}

	fmt.Fprintf(os.Stderr, "%s: %s\n", progName, err.Error())
	os.Exit(exitCode)
}

func doMake(path string) error {
	var spriv *Dust.CryptoServerPrivate
	var err error
	defer func() {
		if spriv != nil {
			spriv.DestroyPrivate()
		}
	}()
	
	spriv, err = Dust.NewCryptoServerPrivate(net.IPv4zero, 0)
	if err != nil {
		return err
	}

	if err = spriv.SavePrivateFile(path); err != nil {
		return err
	}
	fmt.Fprintf(os.Stdout, "Private key saved to: %s\n", path)

	// TODO: clean up handling of address, name, other metadata here?
	bridgeParams := spriv.BridgeParams()
	bridgeParts := make([]string, 0, 1)
	for k, v := range bridgeLine {
		bridgeParts = append(bridgeParts, fmt.Sprintf("%s=%s", k, v))
	}
	bridgeString := strings.Join(bridgeParts, " ")
	fmt.Fprintf(os.Stdout, "Bridge parameters: %s\n", bridgeString)
	return nil
}

func main() {
	// TODO: clean up arg parsing
	if len(os.Args) < 3 {
		errorExit(ErrBadArgs)
	}
	
	subcommand := os.Args[1]
	if subcommand != "genkey" {
		errorExit(ErrBadArgs)
	}

	path := os.Args[2]

	err := doMake(path)
	if err != nil {
		errorExit(err)
	}
}
