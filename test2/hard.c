
void puts(const char *s);

void abc(long a) {
  puts(__FUNCTION__);
}

void def(int a, short b) {
  puts(__FUNCTION__);
}

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

void (int a) foo() {
  puts(__FUNCTION__);
}
void (long a) foo() {
  puts(__FUNCTION__);
}
void (double a) foo() {
  puts(__FUNCTION__);
}

/* For operator overload */
struct A { int i; };

struct A (struct A a) __add__(int b) {
  puts(__FUNCTION__);
  a.i += b;
  return a;
}

struct A (struct A a) __add__(long b) {
  puts(__FUNCTION__);
  a.i += b;
  return a;
}

struct A (struct A a) __add__(double b) {
  puts(__FUNCTION__);
  a.i += (int)b;
  return a;
}

int main(void) {
  abc(10);
  def(10, 20);
  ((int)10).def(20);

  foo((short)10, (short)20);
  bar((double)10, 20L); // long literal
  bar((double)10, 20.0);

  ((int)10).foo();
  (10L).foo();
  (10.0).foo();

  ((short)10).foo((short)20);
  ((double)10).bar(20L);
  (10.0).bar(20.0);

  struct A a;
  a = a + (int)10;
  a = a + 10L;
  // TODO:
  // a = a + 10.0;

  return 0;
}
