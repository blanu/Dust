echo "Building dist"
go build dist
go install dist

echo "Building Dust"
go generate Dust
go build Dust
go install Dust
go test Dust
