
void puts(const char *s);

void foo(short a, long b) {
  puts(__FUNCTION__);
}

void foo(short a, short b) __attribute__((symbol("__foo2"))) {
  puts(symbolof((short, short).foo));
  (short, long).foo(a + 1, b + 1);
}

int bar(double a, long b) {
  puts(__FUNCTION__);
  return 0;
}

void bar(double a, double b) __attribute__((symbol("bar_d_d"))) {
  puts(__FUNCTION__);
}

void abc(int a) {
  puts(__FUNCTION__);
}

int main(void) {
  abc(10);
  foo((short)10, (short)20);
  bar((double)10, 20L); // long literal
  bar((double)10, 20.0);
  return 0;
}
