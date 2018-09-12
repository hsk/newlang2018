#include <stdio.h>

int add10(int a1, int a2, int a3, int a4, int a5,int a6, int a7,int a8, int a9, int a10) {
  return a1+a2+a3+a4+a5+a6+a7+a8+a9+a10;
}
void print_int(int a) {
  printf("%d\n",a);
}
int main() {
  int a = add10(1,2,3,4,5,6,7,8,9,10);
  print_int(a);
  return 0;
}