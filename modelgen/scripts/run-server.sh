#!/bin/bash
. scripts/run-base.sh
echo "Launching $DUSTIP $DUSTPROXYID"
scripts/run-only-server.sh
