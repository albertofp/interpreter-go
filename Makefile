PHONY: test

test:
	go test ./...

run:
	go run main.go

build:
	go build -o bin/interpreter main.go
