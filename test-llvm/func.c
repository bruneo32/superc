#include <stdio.h>
#include <stdarg.h>

struct nothing {} _nothing;

struct ReturnVal {
	int a;
	int b;
};

/** This is a shame for C compilers,
 * an inline function should be able
 * to be inlined without enforcing */
#define inline __attribute__((always_inline)) inline

static void foo() {}

inline int bar() {
	return 1;
}

static inline int foobar() {
	return 2;
}

inline static char *foobar2() {
	return __func__;
}

static struct ReturnVal foobar3() {
	struct ReturnVal rv;
	rv.a = 5;
	rv.b = 6;
	return rv;
}

// long sumall(int n, ...) {
// 	va_list ap;
// 	int sum = 0;
// 	va_start(ap, n);
// 	for (int i = 0; i < n; i++)
// 		sum += va_arg(ap, int);
// 	va_end(ap);
// 	return sum;
// }

int main() {
	foo();
	int a = bar();
	int b = foobar();
	int c = foobar2();
	// long d = sumall(3, 6, -5, 2);
	long d = 4;
	struct ReturnVal rv = foobar3();
	printf("OK, %d %d %d %ld\n", a, b, c, d);
	return 0;
}
