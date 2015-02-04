#!/bin/bash
sudo apt-get install mercurial
pushd ~/go
export GOPATH=~/go
go get code.google.com/p/go.crypto/curve25519
go get git.torproject.org/pluggable-transports/obfs4.git/common/csrand
go get github.com/dchest/skein
go get github.com/agl/ed25519/extra25519
go get github.com/op/go-logging
popd
