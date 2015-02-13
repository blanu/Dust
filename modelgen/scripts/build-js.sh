#!/bin/bash
set -e
# compile-go.py [language] [modelDir] [packageName] [packageDir]
python src/compile.py js ../go/models DustTestModels ~/js/DustTestModels
