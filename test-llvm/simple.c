#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <float.h>

_Bool g0;
unsigned short g01;
unsigned long g02;
float g03;
double g04;
long double g05;

// __int128 g06;

#ifdef FLT16_MAX
_Float16 g07;
#endif

size_t g08;
ptrdiff_t g09;

void* g10;

int _Alignas(512) g1;
static int _Alignas(256) g2;

char *public_string = "public";
static char *private_string = "private";

struct color {
	unsigned char r,g,b,a;
};

typedef struct Car Car;
struct Car {
	char *name;
	struct color col;
	int kms;
} mycar = {0};

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
} bytefield = {1};

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
} mynumber;

union only_floats {
	float  f;
	double _Alignas(512) d;
} myfloat;

union idk_big {
	int  a;
	long b;
	short _Alignas(512)  c;
	double _Alignas(512) d;
} mybig;

int main() {
	struct inside {
		int a;
		int b;
	} __attribute__((packed)) abc;

	printf("anonymous\n");
	return 0;
}
