#include <stdio.h>
int a = 8000;
int c = 2;
int b() {return a/c; }
int d() {return a%c; }
int a1() { return a||c;}
int a300() { return a > c; }
int main() {
  printf("%d %d\n", b(),a1());
  return 0;
}
