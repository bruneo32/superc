---
title: (DRAFT) become
layout: blog
---

> ⚠️ This is a **PROPOSAL DRAFT**. Become is currently **not implemented**. Syntax may change<br>

# The `become` keyword
When a function is called **recursively**, a *stack frame* is allocated each time the function is called. So if you call a function **5** times, you will have **5** *stack frames* allocated, but if you call a function ***millions*** of times, you end up with an ***error*** because you run out of usable **RAM**.

The **solution** was invented decades ago, it's **[tail call](<https://en.wikipedia.org/wiki/Tail_call>){:target="_blank"}**.

Instead of creating a new *stack frame*, you can reuse the previous one, so **5** recursive calls will only use **1** *stack frame*, and ***millions*** of recursive calls will use only **1** *stack frame*.

- `become` is a keyword that forces the function to perform a **tail call** instead of a *normal function call*.
- Hence, `become` is a specific keyword, typically used in recursion scenarios, but not limited to them (see the following use cases).

# Use Case 1: Recursion
The following program seems fine, but it will crash because `sum` overflows the stack with a million of recursive calls.

You can see how it's solved in the **SuperC** version.

{% tabs become1 %}
{% tab become1 Problem %}
```c
#include <stdio.h>

/**
 * sum(n) is the sum of all integers from 1 to n
 * - sum(5) = 5 + 4 + 3 + 2 + 1 = 15
 * - sum(100) = 100 + 99 + 98 + ... + 3 + 2 + 1 = 5050
 */
size_t sum(size_t n) {
  if (!n)
    return 0;
  return n + sum(n - 1);
}

int main() {
  printf("sum(5) = %zu\n", sum(5));
  printf("sum(100) = %zu\n", sum(100));
  printf("sum(1000000) = %zu\n", sum(1000000)); // stack overflow due to recursion
  // sum(5) = 15
  // sum(100) = 5050
  // Segmentation fault
  return 0;
}
```
{% endtab %}

{% tab become1 SuperC %}
```cpp
#include <stdio.h>

/**
 * sum(n) is the sum of all integers from 1 to n
 * - sum(5) = 5 + 4 + 3 + 2 + 1 = 15
 * - sum(100) = 100 + 99 + 98 + ... + 3 + 2 + 1 = 5050
 */
size_t sum(size_t n, size_t acc = 0) {
  // We cannot use `become n+sum...` because the become keyword
  // strictly requires a function call.
  // So we have to perform the addition prior to calling the function.
  if (n == 0)
    return acc;
  become sum(n - 1, acc + n);
}

int main() {
  printf("sum(5) = %zu\n", sum(5));
  printf("sum(100) = %zu\n", sum(100));
  printf("sum(1000000) = %zu\n", sum(1000000));
  // sum(5) = 15
  // sum(100) = 5050
  // sum(1000000) = 500000500000
  return 0;
}
```
{% endtab %}

{% tab become1 LLVM IR %}
```llvm
...

@.str = private unnamed_addr constant [14 x i8] c"sum(5) = %zu\0A\00", align 1
@.str.1 = private unnamed_addr constant [16 x i8] c"sum(100) = %zu\0A\00", align 1
@.str.2 = private unnamed_addr constant [20 x i8] c"sum(1000000) = %zu\0A\00", align 1

define dso_local i64 @sum(i64 noundef %0, i64 noundef %1) {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, ptr %4, align 8
  store i64 %1, ptr %5, align 8
  %6 = load i64, ptr %4, align 8
  %7 = icmp eq i64 %6, 0
  br i1 %7, label %8, label %10

8:
  %9 = load i64, ptr %5, align 8
  store i64 %9, ptr %3, align 8
  br label %17

10:
  %11 = load i64, ptr %4, align 8
  %12 = sub i64 %11, 1
  %13 = load i64, ptr %5, align 8
  %14 = load i64, ptr %4, align 8
  %15 = add i64 %13, %14
  %16 = musttail call i64 @sum(i64 noundef %12, i64 noundef %15)
  ret i64 %16

17:
  %18 = load i64, ptr %3, align 8
  ret i64 %18
}

define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  %2 = call i64 @sum(i64 noundef 5, i64 noundef 0)
  %3 = call i32 (ptr, ...) @printf(ptr noundef @.str, i64 noundef %2)
  %4 = call i64 @sum(i64 noundef 100, i64 noundef 0)
  %5 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i64 noundef %4)
  %6 = call i64 @sum(i64 noundef 1000000, i64 noundef 0)
  %7 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i64 noundef %6)
  ret i32 0
}

declare i32 @printf(ptr noundef, ...) #1

...
```
{% endtab %}
{% endtabs %}

# Use Case 2: Function Forwarding (Dispatchers)
In complex systems like network stacks (e.g., TCP connection parsing) or video game logic (e.g., AI behaviors), developers can cleanly encapsulate every discrete state into its own function. become removes the penalty for doing so.

{% tabs become2 %}
{% tab become2 SuperC %}
```cpp
#include <stdio.h>

int handle_get(int user_id) {
  // Do heavy lifting...
  return 200;
}

int handle_post(int user_id) {
  // Do heavy lifting...
  return 201;
}

// The dispatcher inspects the request, then uses `become`.
// The stack frame is replaced by the handler.
// If the handler crashes, `dispatch_request` won't even
// appear in the debugger's stack trace!
int dispatch_request(int method, int user_id) {
  if (method == 0) {
    become handle_get(user_id);
  } else if (method == 1) {
    become handle_post(user_id);
  }
  return 405;
}
```
{% endtab %}
{% endtabs %}


# Use Case 3: Finite State Machines
Because the compiler guarantees that the current stack frame is obsolete when become is executed, arguments passed to the next function are simply shifted into the correct CPU registers. Memory access is entirely bypassed during the state transition.

{% tabs become3 %}
{% tab become3 SuperC %}
```cpp
// Forward declarations for the states
int state_reading(const char* stream, int index);
int state_error(const char* stream, int index);
int state_success(void);

int state_reading(const char* stream, int index) {
  char current = stream[index];

  if (current == '\0')
    become state_success();

  if (current == 'X')
    become state_error(stream, index);

  // Continue reading next character
  become state_reading(stream, index + 1);
}

int state_error(const char* stream, int index) {
  printf("Parse error at index %d\n", index);
  return -1;
}

int state_success(void) {
  printf("Parsing finished successfully.\n");
  return 0;
}

int parse_data(const char* stream) {
  // Bootstrapping the state machine
  become state_reading(stream, 0);
}
```
{% endtab %}
{% endtabs %}

# Notes
- Probably `defer` statements should be called before `become` statements.
- Other edge cases must be covered.
