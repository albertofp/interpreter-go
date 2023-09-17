PHONY: test

test:
	gotest ./...

run:
	go run main.go

build:
	go build -o bin/interpreter main.go
