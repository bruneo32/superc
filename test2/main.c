
void puts(const char *s);
// void printf(const char *__restrict, ...);

// void foo(int a)   __attribute__((symbol("foo_i"))) {
//   // printf("INT: %d\n", a);
//   puts("INT");
// }

void foo(long a)  __attribute__((symbol("foo_l"))) {
  // printf("INT: %d\n", a);
  puts("LONG");
}

void foo(float a) __attribute__((symbol("foo_f"))) {
  // printf("FLOAT: %f\n", a);
  puts("FLOAT");
}

void foo(short a) {
  // printf("INT: %d\n", a);
  puts("SHORT");
}

int main(void) {
  foo((short) 10);
  foo((float) 3.1415);
  //foo(10); // Who? short or long?
  return 0;
}
