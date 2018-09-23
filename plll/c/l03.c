#include <stdio.h>
long add(long a,long b) {
  return a+b;
}
long mul(long a,long b) {
  return a*b;
}
void print_l(long l) {
  printf("%ld\n",l);
}
int main() {
  print_l(1L);
  print_l(add(2L,3L));
  print_l(mul(add(2L,3L),2L));
  return 0;
}
