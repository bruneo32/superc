#include <stdio.h>
#include <string.h>
/* Silly GNU overrides __attribute__ macro */
#undef __attribute__

typedef char *string;

string (string s) concat(string o) __attribute__((symbol("strcat")));
string (string s) __iadd__(string o) __attribute__((symbol("strcat")));

char str_hello[100] = "Hello";

int main(int argc, char const *argv[]) {
	str_hello.concat(", ");
	str_hello += "World!";
	printf("%s\n", str_hello);
	return 0;
}
