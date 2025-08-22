#include "test.h"

int main() {
  defer { printf("Deferred 1\n"); };
  defer { printf("Deferred 2\n"); };
  defer { printf("Deferred 3\n"); };

  if (0) {
    /* No "OK" */
    return 1;
  }

  printf("OK\n");
  return 0;
}
