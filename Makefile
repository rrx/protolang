default: build functions test doc

build:
	cargo build

doc:
	cargo doc --all --no-deps


test:
	cargo test -- --nocapture

CFLAGS=-fPIC -fno-direct-access-external-data
functions:
	clang ${CFLAGS} -c link/testfiles/testfunction.c -o ./tmp/testfunction.o
	clang ${CFLAGS} -c link/testfiles/simplefunction.c -o ./tmp/simplefunction.o
	clang ${CFLAGS} -c link/testfiles/asdf.c -o ./tmp/asdf.o
	clang ${CFLAGS} -c link/testfiles/segfault.c -o ./tmp/segfault.o
	clang ${CFLAGS} -c link/testfiles/link_shared.c -o ./tmp/link_shared.o
	clang ${CFLAGS} -c link/testfiles/live.c -o ./tmp/live.o
	clang ${CFLAGS} -c link/testfiles/empty_main.c -o ./tmp/empty_main.o
	#clang -c link/testfiles/live.c -o ./tmp/live.o
	clang ${CFLAGS} -shared link/testfiles/live.c -o ./tmp/live.so
	#gcc -nostdlib -m32 link/testfiles/start.c -o ./tmp/start.o
	clang -nostdlib link/testfiles/start.c -o ./tmp/start
	clang ${CFLAGS} -nostdlib link/testfiles/start.c -o ./tmp/start.o
	clang ${CFLAGS} -shared link/testfiles/live.c -o ./tmp/live.so

