#include <stdio.h>
int g=1;
int g2=0;
int g3=0;
int main() {
  puts("XXX1");
  printf("XXX2\n");
  fprintf(stdout, "XXX3\n");
  fprintf(stdout, "%p\n", g);
  fflush(stdout);
  g = &fprintf;
  fprintf(stdout, "%p\n", fprintf);
	return 0;
}

