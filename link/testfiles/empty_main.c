#include <stdio.h>
#include <stdlib.h>

int g=1;
int g2=0;
int g3=0;
int main() {
  puts("XXX1");
  printf("XXX2\n");
  fprintf(stdout, "XXX3\n");
  fprintf(stdout, "%p\n", g);
  fprintf(stderr, "%p\n", g);
  fprintf(stdout, "%p\n", fprintf);
  fflush(stdout);
  g = &fprintf;
	return 0;
}

