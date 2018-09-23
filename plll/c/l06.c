#include <stdio.h>
void print_l(long l) {
  printf("%ld\n",l);
}
int main() {
  long a[10];
  a[1L]=3L;
  print_l(a[1L]);
  return 0;
}
