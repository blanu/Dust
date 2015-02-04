#!/bin/bash
. scripts/run-base.sh
scripts/run-only-server.sh &
scripts/run-only-client.sh $DUSTIP $DUSTPROXYID $PROTOCOL
killall DustProxy
