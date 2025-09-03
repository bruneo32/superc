#include "test.h"

int main() {
  int i, j;
  for (j = 0; j < 10; j++) {
    for (i = 0; i < 10; i++) {
      if (i == 5 && j == 2)
        break 2; /* Break both for loops */
    }
  }

  ASSERT(5, i);
  ASSERT(2, j);

  while (i) {
    i--;
    while (i < 20) {
      i += 2;
      while (1) {
        switch (j) {
          case 1: break 4;
          default:
            j--;
            break 3;
        }
      }
    }
  }

  ASSERT(7, i);

  printf("OK\n");
  return 0;
}
