#include "test.h"

int foo::var1 = 0;
int foo::var2 __attribute__((symbol("foo_bar2"))) = 1;
int foo::bar::var1 = 2;

/* Test that unknown pragmas are skipped over */
#pragma region SDL
typedef struct {
  int x;
  int y;
} SDL::Point;

SDL::Point (SDL::Point p) __add__(SDL::Point q) {
  p.x += q.x;
  p.y += q.y;
  return p;
}
#pragma endregion SDL

#pragma namespace Math
const double PI = 3.141592653;
double add(double a, double b) {
  return a + b;
}

// this shall be 'Math::Point'
typedef struct { double x, y; } Point;

/* Methods don't apply namespaces, so
 * this is not 'Math::__add__', it's just '__add__' */
Math::Point (Math::Point p) __add__(Math::Point q) {
  p.x += q.x;
  p.y += q.y;
  return p;
}

void test() {
  typedef int local_tydef; // this is not 'Math::local_tydef' because it's a local
  local_tydef localvar = 1;
  ASSERT(1, localvar);
}

#pragma namespace Math::Trigonometry
double cos(double) __attribute__((symbol("cos")));
float  cosf(float) __attribute__((symbol("cosf")));


#pragma namespace foobar
#pragma namespace_symbol "foobar__"
int num = 32;
int *ptr = &foobar::num;
int xyz::num = 64;
int baz() {
  return foobar::xyz::num;
}


// Include std* without namespace to load typedefs
#pragma namespace
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#undef _STDIO_H
#undef _STDLIB_H

// Now wrap std headers in a namespace
#pragma namespace libc
#pragma namespace_symbol ""
#include <stdio.h>
#include <stdlib.h>
#pragma namespace // end namespace

int main() {
  SDL::Point  sp = {1, 2};
  Math::Point mp = {1.0, 2.0};

  ASSERT(0, strcmp(symbolof(foo::var1),      "foo$$var1"));
  ASSERT(0, strcmp(symbolof(foo::var2),      "foo_bar2"));
  ASSERT(0, strcmp(symbolof(foo::bar::var1), "foo$$bar$$var1"));

  ASSERT(0, strcmp(symbolof(foobar::num), "foobar__num"));
  ASSERT(0, strcmp(symbolof(foobar::xyz::num), "foobar__xyz$$num"));
  ASSERT(0, strcmp(symbolof(foobar::baz), "foobar__baz"));

  ASSERT(0, strcmp(symbolof(Math::PI),  "Math$$PI"));
  ASSERT(0, strcmp(symbolof(Math::add), "Math$$add"));
  ASSERT(0, strcmp(symbolof(Math::Trigonometry::cos),  "cos"));
  ASSERT(0, strcmp(symbolof(Math::Trigonometry::cosf), "cosf"));

  ASSERT(6.0, ({ Math::Point p = mp + (Math::Point){3.0, 4.0}; p.y; }));

  ASSERT(0, strcmp(symbolof(libc::printf), "printf"));
  ASSERT(0, strcmp(symbolof(libc::exit),   "exit"));

  libc::printf("OK\n");
  libc::exit(0);
}
