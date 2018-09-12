#include <alloca.h>
static long a(){
  long *b = alloca(100);
  *b = 20;
  long *c = alloca(200);
  *c= 30;
  long *d = alloca(300);
  *d = *b + *c;
  return *d;
}

long main() {
  return a();
}