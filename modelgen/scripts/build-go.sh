#!/bin/bash
set -e
# compile.py [language] [modelDir] [packageName] [packageDir]
python src/compile.py go ../go/models DustTestModels ~/go/DustTestModels
