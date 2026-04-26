// #include <stdio.h>
#include <alloca.h>

int printf(char *fmt, ...);
struct _IO_FILE { char __x; };
typedef struct _IO_FILE FILE;
int putc(int, FILE *);

extern FILE *const stdout;

struct nothing {} _nothing;

struct ReturnVal {
	char  a;
	int   b;
	long  c;
	char  d;
	short e;
};

static void foo() {}

// inline int bar() { return 1; }

static int _33(int a, float b) {
	return 33 + a + (int)b;
}

static inline int foobar() { return 2; }

// inline static const char *foobar2() {
// 	return __func__;
// }

struct ReturnVal foobar3() {
	return (struct ReturnVal){
		.a = 1,
		.b = 2,
		.c = 3,
		.d = 4,
		.e = 5,
	};
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

void label_test() {
first:
	return;
	goto first;
}

int main(int argc, char *argv[]) {
	alloca(7);

	struct ReturnVal r = foobar3();
	putc(r.e + '0', stdout);
	putc('\n', stdout);

	goto test_label;

	foo();

	int a = 10;
	int b = 10 + (2 * 3);

test_label: ;

	float f = (float)b;
	int c = (int)f;

	float ff = f + 9.0f;
	double d = f + 9.0;
	long double ld = d * 9.0;

	int e = _33(3, 2.0f);
	int g = _33(0, 0.0f) + 1;
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
