#!/bin/bash
echo "Connecting to $DUSTPROXYID"
~/go/bin/DustProxy --incomplete -r 127.0.0.1 -l 127.0.0.1:16680 out $1:16667 $DUSTPROXYID m=$PROTOCOL
