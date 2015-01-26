echo "Building Dust"
python src/compile-go.py $1 $2
pushd $2
go build $1
go install $1
go test $1
popd
