if [ ! -d src/github.com/gonum/stat/ ];
then
  echo "Building stat"
  go get github.com/gonum/stat
  go build github.com/gonum/stat
  go install github.com/gonum/stat
  go test github.com/gonum/stat
fi

echo "Building Dust"
go generate Dust
go build Dust
go install Dust
go test Dust
