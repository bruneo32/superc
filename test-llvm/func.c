// #include <stdio.h>
#include <alloca.h>

struct nothing {} _nothing;

// struct ReturnVal {
// 	char  a;
// 	int   b;
// 	long  c;
// 	char  d;
// 	short e;
// } __attribute__((packed));

/** This is a shame for C compilers,
 * an inline function should be able
 * to be inlined without enforcing */
#define inline __attribute__((always_inline)) inline

static void foo() {}
static void bar(int a, ...) {}

// inline int bar() { return 1; }

// static int _33() { return 33; }

// static inline int foobar() { return 2; }

// inline static const char *foobar2() {
// 	return __func__;
// }

// static struct ReturnVal foobar3() {
// 	return (struct ReturnVal){
// 		.a = 1,
// 		.b = 2,
// 		.c = 3,
// 		.d = 4,
// 		.e = 5,
// 	};
// }

// long sumall(int n, ...) {
// 	va_list ap;
// 	int sum = 0;
// 	va_start(ap, n);
// 	for (int i = 0; i < n; i++)
// 		sum += va_arg(ap, int);
// 	va_end(ap);
// 	return sum;
// }

void label_test() {
first:{}
	return;
	goto first;
}

int main() {
	alloca(7);

// goto test_label;

	// foo();

	int a = 10;
	int b = 10 + (2 * 3);

test_label: {

	float f = (float)b;
	int c = (int)f;

	float ff = f + 9.0f;
	double d = f + 9.0;
	long double ld = d * 9.0;
}
	// int c = _33();
	// int d = _33() + 1;
	// printf("OK\n");
	return 0;
}

// int early() {
// 	static volatile int x;
// 	if (x & 1) {
// 		foo();
// 		goto earl_ret;
// 	}
// 	foo();
// 	earl_ret:
// 	return 0;
// }
