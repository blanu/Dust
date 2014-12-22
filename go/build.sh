echo "Building stat"
go build github.com/gonum/stat
go install github.com/gonum/stat
go test github.com/gonum/stat

echo "Building Dust"
go generate Dust
go build Dust
go install Dust
go test Dust
