int main(){
  long a = 1;
  {
    long a;
    long b;
    //printf("&a=%p &b=%p\n",&a,&b);
    b=0;
    while(b++ < 3) {
      printf("a=%ld\n",a);
      a=b;
    }
  }
  {
    long a;
    long b;
    printf("b=%ld\n",b);
    b = 0;
    while(b++ < 3) {
      printf("a2=%ld\n",a);
      a=b;
    }
  }
}