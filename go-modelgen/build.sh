#!/bin/sh
set -e
# compile-go.py [modelDir] [packageName] [packageDir]
python src/compile-go.py "$1" "$2" "$3"
cd "$3"
go install
go test
