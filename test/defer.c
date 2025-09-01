#include "test.h"

int main() {
  defer printf("Function defer 1\n");
  defer {
    printf("Function defer 2\n");
    printf("Function defer 3\n");
  }
  defer printf("Function defer 4\n");

  int i = 0;

  for (; i < 3; i++) {
    defer printf("This will run when the function is over\n");
    defer break printf("[%d] This will run right before loop break\n", i);
    defer continue printf("[%d] This will run right before loop increment\n", i);
  }

  while (i < 12) {
    defer continue {
      printf("[%d]++\n", i);
      i++;
    }

    printf("> %d\n", i);

    continue;
    printf("unreachable\n");
  }

  do {
    defer break i = 0;
    if (i == 15)
      break;
    i++;
  } while (i < 16);

  if (i == 0) {
    printf("OK\n");
    return 0;
  }

  defer printf("Unreachable defer\n");
  return 1;
}
