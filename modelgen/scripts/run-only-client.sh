#!/bin/bash
echo "Connecting to $1 $2 $3"
echo "Listening on localhost:16680"
~/go/bin/DustProxy -d --incomplete -r 127.0.0.1 -l 127.0.0.1:16680 out $1:16667 $2 m=$3
