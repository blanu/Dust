export PROTOCOL=sillyHex
export DESTINATION=127.0.0.1:8090

./build.sh ../go/models caolta3iaejox3z4madc5wmp4z.uuid/Dust_privmodels ~/go/src/caolta3iaejox3z4madc5wmp4z.uuid/Dust_privmodels 

pushd ~/go/src/github.com/blanu/Dust/go/cmd/DustTool
go build
go install
rm ~/DustProxy.invite
~/go/bin/DustTool newid -o ~/DustProxy.invite test 127.0.0.1:16667 m=$PROTOCOL >~/DustProxy.bridge
export DUSTPROXYID=`cat ~/DustProxy.bridge | awk '{print $4}'`
popd

pushd ~/go/src/github.com/blanu/Dust/go/cmd/DustProxy
go install -tags privmodels
~/go/bin/DustProxy --incomplete -c $DESTINATION in ~/DustProxy.invite &
echo "Connecting to $DUSTPROXYID"
~/go/bin/DustProxy --incomplete -r 127.0.0.1 -l 127.0.0.1:16680 out 127.0.0.1:16667 $DUSTPROXYID m=$PROTOCOL
killall DustProxy
popd
