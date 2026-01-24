#include <stdio.h>
/* GNU overrides __attribute__ macro */
#undef __attribute__

int *(int *i) __iadd__(int *n) {
	// Just print
	printf("Adding %d to %d\n", *n, *i);
}

int main(int argc, char const *argv[]) {
	int _1 = 1;
	int _3 = 3;
	(&_1).__iadd__(&_3);
	&_3 += &_1;
	return 0;
}
