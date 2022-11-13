default: build test doc

build:
	cargo build

doc:
	cargo doc --all --no-deps


test:
	cargo test -- --nocapture

functions:
	clang -c link/testfiles/testfunction.c -o ./tmp/testfunction.o
	clang -c link/testfiles/simplefunction.c -o ./tmp/simplefunction.o
	clang -c link/testfiles/asdf.c -o ./tmp/asdf.o
	clang -c link/testfiles/live.c -o ./tmp/live.o
