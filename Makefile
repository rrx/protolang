default: build functions test doc

build:
	cargo build

doc:
	cargo doc --all --no-deps


test:
	cargo test -- --nocapture

CFLAGS=-fPIC -fno-direct-access-external-data ${NIX_CFLAGS_COMPILE}

functions:
	clang ${CFLAGS} -c link/testfiles/testfunction.c -o ./tmp/testfunction.o
	clang ${CFLAGS} -c link/testfiles/simplefunction.c -o ./tmp/simplefunction.o
	clang ${CFLAGS} -c link/testfiles/asdf.c -o ./tmp/asdf.o
	clang ${CFLAGS} -c link/testfiles/segfault.c -o ./tmp/segfault.o
	clang ${CFLAGS} -c link/testfiles/link_shared.c -o ./tmp/link_shared.o
	clang ${CFLAGS} -c link/testfiles/live.c -o ./tmp/live.o
	clang ${CFLAGS} -c link/testfiles/empty_main.c -o ./tmp/empty_main.o
	clang ${CFLAGS} -c link/testfiles/uvtest.c -o ./tmp/uvtest.o
	clang ${CFLAGS} -c link/testfiles/globals.c -o ./tmp/globals.o
	clang ${CFLAGS} -c link/testfiles/call_extern.c -o ./tmp/call_extern.o
	clang ${CFLAGS} -c link/testfiles/print_stuff.c -o ./tmp/print_stuff.o
	clang ${CFLAGS} -c link/testfiles/print_string.c -o ./tmp/print_string.o
	clang ${CFLAGS} -g link/testfiles/segfault_handle.c -o ./tmp/segfault_handle
	clang ${CFLAGS} -g link/testfiles/segfault_handle2.c -o ./tmp/segfault_handle2
	ar -rv tmp/liblive.a ./tmp/live.o ./tmp/globals.o
	#clang -fPIC -shared ./tmp/liblive.a -o ./tmp/live.so
	clang -shared -fpic -Wl,--no-undefined link/testfiles/live.c -o ./tmp/live.so


	#gcc -nostdlib -m32 link/testfiles/start.c -o ./tmp/start.o
	clang -nostdlib link/testfiles/start.c -o ./tmp/start
	clang ${CFLAGS} -nostdlib link/testfiles/start.c -o ./tmp/start.o
	clang ${CFLAGS} -shared link/testfiles/live.c -o ./tmp/live.so

	clang ${CFLAGS} -g link/testfiles/invoke_print.c -o ./tmp/invoke_print

