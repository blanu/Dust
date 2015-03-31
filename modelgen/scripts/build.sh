#!/bin/bash
set -e
# compile-go.py [language] [modelDir] [packageName] [packageDir]
python src/compile.py "$1" "$2" "$3" "$4"
pushd "$4"
go install
#go test
popd
