#include "test.h"

typedef struct myint myint;
struct myint {
  long _pad1;
  int x;
  long _pad2;
};

// Arithmetic operators
myint (myint a) __add__(int b) {
  a.x += b;
  return a;
}

myint (myint a) __sub__(int b) {
  a.x -= b;
  return a;
}

myint (myint a) __mul__(int b) {
  a.x *= b;
  return a;
}

myint (myint a) __div__(int b) {
  a.x /= b;
  return a;
}

myint (myint a) __mod__(int b) {
  a.x %= b;
  return a;
}

// Assigment operators
myint *(myint *a) __iadd__(myint b) {
  a->x += b.x;
  return a;
}

myint *(myint *a) __isub__(myint b) {
  a->x -= b.x;
  return a;
}

myint *(myint *a) __imul__(myint b) {
  a->x *= b.x;
  return a;
}

myint *(myint *a) __idiv__(myint b) {
  a->x /= b.x;
  return a;
}

myint *(myint *a) __imod__(myint b) {
  a->x %= b.x;
  return a;
}

// Comparison operators
_Bool (myint a) __eq__(int b) {
  return a.x == b;
}

_Bool (myint a) __ne__(int b) {
  return !(a == b);
}

_Bool (myint a) __lt__(int b) {
  return a.x < b;
}

_Bool (myint a) __gt__(int b) {
  return a.x > b;
}

_Bool (myint a) __le__(int b) {
  return a.x <= b;
}

_Bool (myint a) __ge__(int b) {
  return a.x >= b;
}

// Unary operators
myint (myint a) __neg__() {
  a.x = -a.x;
  return a;
}

myint (myint a) __pos__() {
  if (a < 0) // call __lt__ here
    a = -a;  // call __neg__ here
  return a;
}

myint (myint a) __del__() {
  a.x = ~a.x;
  return a;
}

int main() {
  myint a = { .x = 1 };
  myint b = { .x = 5 };

  // Arithmetic operators
  ASSERT(3, ({ myint c = a + 2; c.x; }) );
  ASSERT(3, (a + 2).x ); // Same as above
  ASSERT(1, a.x); // a unmodified

  ASSERT(-1, ({ myint c = a - 2; c.x; }) );
  ASSERT(-1, (a - 2).x );
  ASSERT(1, a.x);

  a.x = 1;
  ASSERT(2, ({ myint c = a * 2; c.x; }) );
  ASSERT(2, (a * 2).x );
  ASSERT(1, a.x );
  a.x = 2;
  ASSERT(4, ({ myint c = a * 2; c.x; }) );
  ASSERT(4, (a * 2).x );
  ASSERT(2, a.x);

  a.x = 4;
  ASSERT(2, ({ myint c = a / 2; c.x; }) );
  ASSERT(2, (a / 2).x );
  ASSERT(4, a.x );
  a.x = 1;
  ASSERT(0, ({ myint c = a / 2; c.x; }) );
  ASSERT(0, (a / 2).x );
  ASSERT(1, a.x);

  // Assigment operators
  a.x = 1;
  ASSERT(&a, &a += b);
  ASSERT(6, a.x); // a modified

  ASSERT(&a, &a -= b);
  ASSERT(1, a.x);

  ASSERT(&a, &a *= b);
  ASSERT(5, a.x);
  ASSERT(&a, &a *= b);
  ASSERT(25, a.x);

  ASSERT(&a, &a /= b);
  ASSERT(5, a.x);

  ASSERT(&a, &a %= b);
  ASSERT(0, a.x);

  // Comparison operators
  a.x = -4;
  ASSERT(0, a == 1);
  ASSERT(1, a == -4);
  ASSERT(1, a != 100);
  ASSERT(0, a != -4);
  ASSERT(0, a < -5);
  ASSERT(0, a < -4);
  ASSERT(1, a < -3);
  ASSERT(1, a > -5);
  ASSERT(0, a > -4);
  ASSERT(0, a > -3);
  ASSERT(0, a <= -5);
  ASSERT(1, a <= -4);
  ASSERT(1, a <= -3);
  ASSERT(1, a >= -5);
  ASSERT(1, a >= -4);
  ASSERT(0, a >= -3);

  // Unary operators
  ASSERT(4, -a.x);   // Unary negation of the member
  ASSERT(4, (-a).x); // Unary negation of the struct
  ASSERT(-4, +a.x);  // Unary plus of member, C just returns the member as-is
  ASSERT(4, (+a).x); // Unary plus of struct, overload says return positive value
  ASSERT(~(-4), ~a.x);
  ASSERT(~(-4), (~a).x);

  printf("OK\n");
  return 0;
}
