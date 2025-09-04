// clang -S -emit-llvm -O0 -g simple.c

#include <stdio.h>

int main() {
	printf("Hello, world!\n");
	return 0;
}
