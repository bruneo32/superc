---
title: Switch ranges
layout: blog
---

# Switch ranges
> This is already implemented in [GNU C](<https://gcc.gnu.org/onlinedocs/gcc/Case-Ranges.html>){:target="_blank"}

Sometimes it's tedious to write a switch statement with a lot of cases for nearby values.

This can be done using ranges, which are written as `case low ... high`, it has the same effect as the proper number of individual case labels, one for each integer value from low to high, inclusive.

> Note: Space around the `1 ... 10` is important, otherwise you could get an error when parsing integer values.

### Examples
```cpp
#include <stdio.h>

int main() {
  int x = 10;
  switch (x) {
    // case 1:
    // case 2:
    // ...:
    // case 20:
    case 1 ... 20:
      printf("x is between 1 and 20\n");
      break;
    case 21 ... 30:
      printf("x is between 21 and 30\n");
      break;
  }

  char letter = 'a';
  switch (letter) {
    case 'A' ... 'Z':
      printf("letter '%c' is uppercase\n", letter);
      break;
    case 'a' ... 'z':
      printf("letter '%c' is lowercase\n", letter);
      break;
    default:
      printf("'%c' is not a letter\n", letter);
  }

  // x is between 1 and 20
  // letter 'a' is lowercase
  return 0;
}
```
<br>

## Wait? Characters are numbers?
**Yes!** Take a look at [asciitable.com](<https://www.asciitable.com>){:target="_blank"}.

When you state `int x = 'A'`, the compiler just translates it to `int x = 65`.

|  @  | Decimal | Hexadecimal |
| --- | ------- | ----------- |
| ... |   ...   |     ...     |
|  A  |   65    |    0x41     |
|  B  |   66    |    0x42     |
|  C  |   67    |    0x43     |
| ... |   ...   |     ...     |
|  X  |   88    |    0x58     |
|  Y  |   89    |    0x59     |
|  Z  |   90    |    0x5A     |
| ... |   ...   |     ...     |
|  a  |   97    |    0x61     |
|  b  |   98    |    0x62     |
|  c  |   99    |    0x63     |
| ... |   ...   |     ...     |
|  x  |   120   |    0x78     |
|  y  |   121   |    0x79     |
|  z  |   122   |    0x7A     |
| ... |   ...   |     ...     |
