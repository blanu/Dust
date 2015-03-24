#!/bin/bash
export PROTOCOL=http
export DUSTIP=`ifconfig eth0 | grep "inet addr" | awk -F: '{print $2}' | awk '{print $1}'`
export DESTINATION=google.com:80

if [ ! -d "~/go/src/caolta3iaejox3z4madc5wmp4z.uuid" ]; then
  mkdir ~/go/src/caolta3iaejox3z4madc5wmp4z.uuid
fi

scripts/build.sh go ../go/models caolta3iaejox3z4madc5wmp4z.uuid/Dust_privmodels ~/go/src/caolta3iaejox3z4madc5wmp4z.uuid/Dust_privmodels 

pushd ~/go/src/github.com/blanu/Dust/go/cmd/DustTool
go build
go install
rm ~/DustProxy.invite
~/go/bin/DustTool newid -o ~/DustProxy.invite test $DUSTIP:16667 m=$PROTOCOL >~/DustProxy.bridge
export DUSTPROXYID=`cat ~/DustProxy.bridge | awk '{print $4}'`
popd

pushd ~/go/src/github.com/blanu/Dust/go/cmd/DustProxy
go install -tags privmodels
popd
