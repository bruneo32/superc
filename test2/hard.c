
void puts(const char *s);

void foo(int a, long b) {
	puts(__FUNCTION__);
}

void foo(short a, int b) __attribute__((symbol("__foo2"))) {
	puts(symbolof((short, int).foo));
	(int, long).foo(a + 1, b + 1);
}

int main(void) {
  foo((short)10, (int)20);
  return 0;
}
