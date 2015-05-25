#!/bin/bash
set -e
# compile.py [language] [modelDir] [packageName] [packageDir]
python src/compile.py go ../go/models "git.torproject.org/pluggable-transports/obfs4.git/transports/Dust2/models" ~/go/src/git.torproject.org/pluggable-transports/obfs4.git/transports/Dust2/models
