#include <stdio.h>

long func() {
	return 10001;
}

long simple() {
	return 1 + 10 + func();
}


// load globals from an external
char x = 0x34;
char *ptr = &x;
extern long global_int2;
long load_from_extern() {
	return 1 + x + global_int2;
}

long print_stuff() {
	fprintf(stderr, "printing stuff\n");
	fflush(stdout);
	putc(0x34, stdout);
	fflush(stdout);
}

