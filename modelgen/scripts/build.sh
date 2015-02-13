#!/bin/bash
set -e
# compile-go.py [language] [modelDir] [packageName] [packageDir]
python src/compile.py "$1" "$2" "$3" "$4"
cd "$3"
go install
#go test
