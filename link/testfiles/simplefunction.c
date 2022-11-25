#include <stdio.h>

long func() {
	return 10001;
}

long simple() {
	return 1 + 10 + func();
}

long print_stuff() {
	fprintf(stderr, "printing stuff");
	putc(0x34, stdout);
	fflush(stdout);
	putc(0x34, stderr);
	fflush(stderr);
}

