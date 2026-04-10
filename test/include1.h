#include "include2.h"

char *include1_filename = __FILE__;
int include1_line = __LINE__;

int include1 = 5;

// Namespace should be ended when include ends,
// so this namespace should be dropped after
// including include3.h
// if not, it will break the test
#pragma namespace foobar
