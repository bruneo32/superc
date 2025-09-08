
typedef _Bool i1;
typedef signed char i8;
typedef signed short i16;
typedef signed int i32;
typedef signed long i64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;
typedef long double x86_fp80;

int main() {
	i64 src = 123;
	u8 dst = src;
	// char *test_alloca = __builtin_alloca(16);
	return dst;
}
