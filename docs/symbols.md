---
title: Symbols & Aliases
layout: blog
---

# Symbol mangling
Allows the user to change the output symbol of a variable or function.
- New attribute `__attribute__((symbol("new_symbol_name")))`; changes the output symbol at assembly level of the variable or function.
- New keyword `symbolof(identifier)`; returns the expected symbol name of the variable or function as a string literal.

### Examples
```cpp
#include <stdio.h>

int foo __attribute__((symbol("bar"))) = 123;
int bar2 = 456; // No symbol change.

int sum(int a, int b) __attribute__((symbol("abc")));
int (int a) sum(int b) __attribute__((symbol("def"))); // was "sum$ii"

// void abc(); // Link error: redefinition of symbol 'abc'

asm(".global __asm_var__\n"
    ".data\n.type __asm_var__, @object\n"
    ".size __asm_var__, 8\n.align 8\n"
    "__asm_var__: .long 32\n");
extern long ext1 __attribute__((symbol("__asm_var__")));

void test_symbol(char *a, char *b) {
  printf("%s == %s -> %d\n", a, b, strcmp(a, b) == 0);
}

int main(void) {
  test_symbol(symbolof(foo),       "bar");
  test_symbol(symbolof(bar2),      "bar2");
  test_symbol(symbolof(sum),       "abc");
  test_symbol(symbolof((int).sum), "def");

  test_symbol(symbolof(ext1), "__asm_var__");
  printf("ext1 = %d\n", ext1);

  // bar == bar -> 1
  // bar2 == bar2 -> 1
  // abc == abc -> 1
  // def == def -> 1
  // __asm_var__ == __asm_var__ -> 1
  // ext1 = 32
  // OK
  printf("OK\n");
  return 0;
}
```

# Aliases
Symbol mangling can be used to create syntax aliases to existing variables and functions in the compiled binary.

Sure you can do `#define def abc`, but you cannot use preprocessor for [type methods](methods.md), so this is a useful feature of the symbol mangling.

### Examples
```cpp
#include <stdio.h>
#include <string.h>

int abc = 32;
/* def is an alias of abc, so this does not create a new variable */
extern int def __attribute__((symbol("abc")));

/* Use symbol mangling to create a function alias string.concat -> strcat */
char *(char *s1) concat(char *s2) __attribute__((symbol("strcat")));

int main(void) {
  char str_hello[100] = "Hello";
  str_hello.concat(" World").concat("!");
  printf("def: %d\n", def);
  printf("str_hello: %s\n", str_hello);
  // def: 32
  // str_hello: Hello World!
  return 0;
}
```
```cpp
#include <stdio.h>

/*
 * Question: How can I alias an inline function?
 * Answer: Inline the inliner
 */

inline int inline1(int n) { return n + 3; }
inline int (int i) inline2() { return inline1(i); }

int main(void) {
  /*
    binary result is exactly the same as
    printf("%d\n", 10 + 3);
    so this is an inline function alias
  */
  printf("%d\n", ((int)10).inline2());
  // 13
  return 0;
}
```
