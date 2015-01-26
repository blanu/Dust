#!/bin/sh
set -e
python src/compile-go.py "$1" "$2" "$3"
cd "$3"
go install
go test
