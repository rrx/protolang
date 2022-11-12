default: build test doc

build:
	cargo build

doc:
	cargo doc --all --no-deps


test:
	cargo test -- --nocapture

functions:
	clang -c testfunction.c -o ./tmp/testfunction.o
