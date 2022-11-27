#include <stdio.h>

const char *g_str1 = "0000x\n";
const char *g_str2 = "33334x\n";
int v = 10;

int print_stuff(char *str, int val) {
	printf(g_str1);
	printf(g_str2);
	return v+1;
	int ret0 = printf(str, val);
	/*int ret1 = fprintf(stdout, "printing stuff\n");*/
	char *s = "1111\n";
	int ret1 = printf(s);
	int ret2 = printf("2222\n");
	putc(0x31, stdout);
	fputs("fffff", stderr);
	fflush(stdout);
	int len = strlen("asdf");
	int ret3 = putc(0x30, stderr);
	fflush(stderr);

	return ret1 + ret2 + len;

}


