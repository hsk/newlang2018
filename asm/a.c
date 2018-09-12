#include <stdio.h>

int add(int a, int b) {
  return a+b;
}
void print_int(int a) {
  printf("%d\n",a);
}
int main() {
  int a = add(1,2);
  print_int(a);
  return 0;
}