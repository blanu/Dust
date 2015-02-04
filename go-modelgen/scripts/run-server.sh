#!/bin/bash
. scripts/run-base.sh
echo "Launching $DUSTPROXYID $DUSTIP"
scripts/run-only-server.sh
