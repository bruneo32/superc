#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <float.h>

// asm("movl $0, %eax");

_Bool g0;
bool g001 = true;
unsigned short g01;
unsigned long g02;
float g03;
double g04 = 0.0;
long double g05 = 1.23456789;

#ifdef __SIZEOF_INT128__
__int128 g06;
#endif

#ifdef FLT16_MAX
_Float16 g07;
#endif

size_t g08;
ptrdiff_t g09;

void* g10 = &g01;

int _Alignas(512) g1;
static int _Alignas(256) __attribute__((used)) g2 = 5;

char *public_string = "public";
static char *__attribute__((used)) private_string = "private";

static short __attribute__((used)) bidimensional[][3] = {{1, 2, 3}, {4, 5, 6}};
short *bidim_ptr = bidimensional[1];

char l = "hello"[3];

typedef unsigned short char16_t;
typedef unsigned int char32_t;
typedef int wchar_t;

char16_t unich1 = u'日';
char32_t unich2 = U'本';
wchar_t  unich3 = L'語';

char     *unicode1 =  "日本語"; // UTF-8
char16_t *unicode2 = u"日本語"; // UTF-16
char32_t *unicode3 = U"日本語"; // UTF-32
wchar_t  *unicode4 = L"日本語"; // UTF-16 on Windows, UTF-32 on Linux

struct color {
	unsigned char r,g,b,a;
};

typedef struct Car Car;
struct Car {
	char *name;
	struct color col;
	int kms;
} mycar = {"car", {255, 0, 0, 255}, 3};

struct mypack {
	char a;
	long b;
} __attribute__((packed)) mypacked;

typedef uint8_t bit;

struct ByteField {
	bit _8 : 1;
	bit _7 : 1;
	bit _6 : 1;
	bit _5 : 1;
	bit _4 : 1;
	bit _3 : 1;
	bit _2 : 1;
	bit _1 : 1;
} bytefield = {
	._8 = 1,
	._7 = 0,
	._6 = 0,
	._5 = 0,
	._4 = 0,
	._3 = 0,
	._2 = 1,
	._1 = 0,
};
static char __attribute__((used)) _a = 'A';

struct ShortField {
	bit _padding0 : 7;
	bit _9 : 1;
	/* Byte */
	bit _8 : 1;
	bit _7 : 1;
	bit _6 : 1;
	bit _5 : 1;
	bit _4 : 1;
	bit _3 : 1;
	bit _2 : 1;
	bit _1 : 1;
} shortfield;

struct ComplexField {
	bit _padding0 : 7;
	bit _1 : 1;
	bit _2 : 1; // OVERFLOW
	uint16_t s;
	uint32_t i;
	bit _3 : 7; // OVERFLOW AGAIN
} complexfield;

typedef union number number;
union number {
	_Bool b;
	int i;
	float f;
} mynumber = {.f = 3.14, .i = 3}; // last prevails

union only_floats {
	float  f;
	double _Alignas(512) d;
} myfloat = {.f = 2.71};

union idk_big {
	int  a;
	long b;
	short _Alignas(512)  c;
	double _Alignas(512) d;
} mybig;

char semistr[4] = {48, 49, 50, 0};

int autoarray[] = {1, 2, 3, 4, 5};

int main() {
	struct inside {
		int a;
		int b;
	} __attribute__((packed)) abc;

	printf("anonymous, %c\n", '0' + g001);

	int array_vla[g2];

	// asm("movl $0, %eax");

	return 0;
}
