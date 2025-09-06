#include "superc.h"

#define GP_MAX 6
#define FP_MAX 8

static FILE *output_file;
static count_t depth;
static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg16[] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
static Obj *current_fn;
static Node *current_loop;

#define NO_DEFER ((count_t)-1)
static count_t current_defer_fn = NO_DEFER;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

static void gen_defers_brk(void);
static void gen_defers_cont(void);

#define emitc(ch) ({ putc((ch), output_file); })
#define emitln    emitc('\n')

__attribute__((format(printf, 1, 2)))
static void emitf(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
}

__attribute__((format(printf, 1, 2)))
static void emitfln(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  emitln; // same as emitf but a newline at the end
}

static count_t count(void) {
  static count_t i = 1;
  return i++;
}

static void push(void) {
  emitfln("  push %%rax");
  depth++;
}

static void pop(char *arg) {
  emitfln("  pop %s", arg);
  depth--;
}

static void pushf(void) {
  emitfln("  sub $8, %%rsp");
  emitfln("  movsd %%xmm0, (%%rsp)");
  depth++;
}

static void popf(int reg) {
  emitfln("  movsd (%%rsp), %%xmm%d", reg);
  emitfln("  add $8, %%rsp");
  depth--;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static char *reg_dx(int sz) {
  switch (sz) {
  case 1: return "%dl";
  case 2: return "%dx";
  case 4: return "%edx";
  case 8: return "%rdx";
  }
  unreachable();
}

static char *reg_ax(int sz) {
  switch (sz) {
  case 1: return "%al";
  case 2: return "%ax";
  case 4: return "%eax";
  case 8: return "%rax";
  }
  unreachable();
}

static char* get_symbol(Obj *var) {
  if (var->symbol)
    return var->symbol;
  return var->name;
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR: {
    const char *symbol = get_symbol(node->var);
    // Variable-length array, which is always local.
    if (node->var->ty->kind == TY_VLA) {
      emitfln("  mov %d(%%rbp), %%rax", node->var->offset);
      return;
    }

    // Local variable
    if (node->var->is_local) {
      emitfln("  lea %d(%%rbp), %%rax", node->var->offset);
      return;
    }

    if (opt_fpic) {
      // Thread-local variable
      if (node->var->is_tls) {
        emitfln("  data16 lea %s@tlsgd(%%rip), %%rdi", symbol);
        emitfln("  .value 0x6666");
        emitfln("  rex64");
        emitfln("  call __tls_get_addr@PLT");
        return;
      }

      // Function or global variable
      emitfln("  mov %s@GOTPCREL(%%rip), %%rax", symbol);
      return;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      emitfln("  mov %%fs:0, %%rax");
      emitfln("  add $%s@tpoff, %%rax", symbol);
      return;
    }

    // Here, we generate an absolute address of a function or a global
    // variable. Even though they exist at a certain address at runtime,
    // their addresses are not known at link-time for the following
    // two reasons.
    //
    //  - Address randomization: Executables are loaded to memory as a
    //    whole but it is not known what address they are loaded to.
    //    Therefore, at link-time, relative address in the same
    //    exectuable (i.e. the distance between two functions in the
    //    same executable) is known, but the absolute address is not
    //    known.
    //
    //  - Dynamic linking: Dynamic shared objects (DSOs) or .so files
    //    are loaded to memory alongside an executable at runtime and
    //    linked by the runtime loader in memory. We know nothing
    //    about addresses of global stuff that may be defined by DSOs
    //    until the runtime relocation is complete.
    //
    // In order to deal with the former case, we use RIP-relative
    // addressing, denoted by `(%rip)`. For the latter, we obtain an
    // address of a stuff that may be in a shared object file from the
    // Global Offset Table using `@GOTPCREL(%rip)` notation.

    // Function
    if (node->ty->kind == TY_FUNC) {
      if (node->var->is_definition)
        emitfln("  lea %s(%%rip), %%rax", symbol);
      else
        emitfln("  mov %s@GOTPCREL(%%rip), %%rax", symbol);
      return;
    }

    // Global variable
    emitfln("  lea %s(%%rip), %%rax", symbol);
  } return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    emitfln("  add $%d, %%rax", node->member->offset);
    return;
  case ND_FUNCALL:
    if (node->ret_buffer) {
      gen_expr(node);
      return;
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      gen_expr(node);
      return;
    }
    break;
  case ND_VLA_PTR:
    emitfln("  lea %d(%%rbp), %%rax", node->var->offset);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

// Load a value from where %rax is pointing to.
static void load(Type *ty) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA:
    // If it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    // This is where "array is automatically converted to a pointer to
    // the first element of the array in C" occurs.
    return;
  case TY_FLOAT:
    emitfln("  movss (%%rax), %%xmm0");
    return;
  case TY_DOUBLE:
    emitfln("  movsd (%%rax), %%xmm0");
    return;
  case TY_LDOUBLE:
    emitfln("  fldt (%%rax)");
    return;
  }

  char *insn = ty->is_unsigned ? "movz" : "movs";

  // When we load a char or a short value to a register, we always
  // extend them to the size of int, so we can assume the lower half of
  // a register always contains a valid value. The upper half of a
  // register for char, short and int may contain garbage. When we load
  // a long value to a register, it simply occupies the entire register.
  if (ty->size == 1)
    emitfln("  %sbl (%%rax), %%eax", insn);
  else if (ty->size == 2)
    emitfln("  %swl (%%rax), %%eax", insn);
  else if (ty->size == 4)
    emitfln("  movsxd (%%rax), %%rax");
  else
    emitfln("  mov (%%rax), %%rax");
}

// Store %rax to an address that the stack top is pointing to.
static void store(Type *ty) {
  assert(ty != NULL);

  pop("%rdi");

  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    for (int i = 0; i < ty->size; i++) {
      emitfln("  mov %d(%%rax), %%r8b", i);
      emitfln("  mov %%r8b, %d(%%rdi)", i);
    }
    return;
  case TY_FLOAT:
    emitfln("  movss %%xmm0, (%%rdi)");
    return;
  case TY_DOUBLE:
    emitfln("  movsd %%xmm0, (%%rdi)");
    return;
  case TY_LDOUBLE:
    emitfln("  fstpt (%%rdi)");
    return;
  }

  if (ty->size == 1)
    emitfln("  mov %%al, (%%rdi)");
  else if (ty->size == 2)
    emitfln("  mov %%ax, (%%rdi)");
  else if (ty->size == 4)
    emitfln("  mov %%eax, (%%rdi)");
  else
    emitfln("  mov %%rax, (%%rdi)");
}

static void cmp_zero(Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT:
    emitfln("  xorps %%xmm1, %%xmm1");
    emitfln("  ucomiss %%xmm1, %%xmm0");
    return;
  case TY_DOUBLE:
    emitfln("  xorpd %%xmm1, %%xmm1");
    emitfln("  ucomisd %%xmm1, %%xmm0");
    return;
  case TY_LDOUBLE:
    emitfln("  fldz");
    emitfln("  fucomip");
    emitfln("  fstp %%st(0)");
    return;
  }

  if (is_integer(ty) && ty->size <= 4)
    emitfln("  cmp $0, %%eax");
  else
    emitfln("  cmp $0, %%rax");
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
    return ty->is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  case TY_LDOUBLE:
    return F80;
  }
  return U64;
}

// The table for type casts
static char i32i8[] = "movsbl %al, %eax";
static char i32u8[] = "movzbl %al, %eax";
static char i32i16[] = "movswl %ax, %eax";
static char i32u16[] = "movzwl %ax, %eax";
static char i32f32[] = "cvtsi2ssl %eax, %xmm0";
static char i32i64[] = "movsxd %eax, %rax";
static char i32f64[] = "cvtsi2sdl %eax, %xmm0";
static char i32f80[] = "mov %eax, -4(%rsp); fildl -4(%rsp)";

static char u32f32[] = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
static char u32i64[] = "mov %eax, %eax";
static char u32f64[] = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";
static char u32f80[] = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)";

static char i64f32[] = "cvtsi2ssq %rax, %xmm0";
static char i64f64[] = "cvtsi2sdq %rax, %xmm0";
static char i64f80[] = "movq %rax, -8(%rsp); fildll -8(%rsp)";

static char u64f32[] = "cvtsi2ssq %rax, %xmm0";
static char u64f64[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; "
  "or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";
static char u64f80[] =
  "mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;"
  "mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:";

static char f32i8[] = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
static char f32u8[] = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
static char f32i16[] = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
static char f32u16[] = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
static char f32i32[] = "cvttss2sil %xmm0, %eax";
static char f32u32[] = "cvttss2siq %xmm0, %rax";
static char f32i64[] = "cvttss2siq %xmm0, %rax";
static char f32u64[] = "cvttss2siq %xmm0, %rax";
static char f32f64[] = "cvtss2sd %xmm0, %xmm0";
static char f32f80[] = "movss %xmm0, -4(%rsp); flds -4(%rsp)";

static char f64i8[] = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
static char f64u8[] = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
static char f64i16[] = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
static char f64u16[] = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
static char f64i32[] = "cvttsd2sil %xmm0, %eax";
static char f64u32[] = "cvttsd2siq %xmm0, %rax";
static char f64i64[] = "cvttsd2siq %xmm0, %rax";
static char f64u64[] = "cvttsd2siq %xmm0, %rax";
static char f64f32[] = "cvtsd2ss %xmm0, %xmm0";
static char f64f80[] = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)";

#define FROM_F80_1                                           \
  "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; " \
  "mov %ax, -12(%rsp); fldcw -12(%rsp); "

#define FROM_F80_2 " -24(%rsp); fldcw -10(%rsp); "

static char f80i8[] = FROM_F80_1 "fistps" FROM_F80_2 "movsbl -24(%rsp), %eax";
static char f80u8[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80i16[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80u16[] = FROM_F80_1 "fistpl" FROM_F80_2 "movswl -24(%rsp), %eax";
static char f80i32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80u32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80i64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80u64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80f32[] = "fstps -8(%rsp); movss -8(%rsp), %xmm0";
static char f80f64[] = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0";

static char *cast_table[][11] = {
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    emitfln("  setne %%al");
    emitfln("  movzx %%al, %%eax");
    return;
  }

  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2])
    emitfln("  %s", cast_table[t1][t2]);
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
static bool has_flonum(Type *ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

static bool has_flonum1(Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}

static void push_struct(Type *ty) {
  int sz = align_to(ty->size, 8);
  emitfln("  sub $%d, %%rsp", sz);
  depth += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    emitfln("  mov %d(%%rax), %%r10b", i);
    emitfln("  mov %%r10b, %d(%%rsp)", i);
  }
}

static void push_args2(Node *args, bool first_pass) {
  if (!args)
    return;
  push_args2(args->next, first_pass);

  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr(args);

  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    push_struct(args->ty);
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    pushf();
    break;
  case TY_LDOUBLE:
    emitfln("  sub $16, %%rsp");
    emitfln("  fstpt (%%rsp)");
    depth += 2;
    break;
  default:
    push();
  }
}

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
// - Up to 6 arguments of integral type are passed using RDI, RSI,
//   RDX, RCX, R8 and R9.
//
// - Up to 8 arguments of floating-point type are passed using XMM0 to
//   XMM7.
//
// - If all registers of an appropriate type are already used, push an
//   argument to the stack in the right-to-left order.
//
// - Each argument passed on the stack takes 8 bytes, and the end of
//   the argument area must be aligned to a 16 byte boundary.
//
// - If a function is variadic, set the number of floating-point type
//   arguments to RAX.
static int push_args(Node *node) {
  int stack = 0, gp = 0, fp = 0;

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16)
    gp++;

  // Load as many arguments to the registers as possible.
  for (Node *arg = node->args; arg; arg = arg->next) {
    Type *ty = arg->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (ty->size > 16) {
        arg->pass_by_stack = true;
        stack += align_to(ty->size, 8) / 8;
      } else {
        bool fp1 = has_flonum1(ty);
        bool fp2 = has_flonum2(ty);

        if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
          fp = fp + fp1 + fp2;
          gp = gp + !fp1 + !fp2;
        } else {
          arg->pass_by_stack = true;
          stack += align_to(ty->size, 8) / 8;
        }
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      if (fp++ >= FP_MAX) {
        arg->pass_by_stack = true;
        stack++;
      }
      break;
    case TY_LDOUBLE:
      arg->pass_by_stack = true;
      stack += 2;
      break;
    default:
      if (gp++ >= GP_MAX) {
        arg->pass_by_stack = true;
        stack++;
      }
    }
  }

  if ((depth + stack) % 2 == 1) {
    emitfln("  sub $8, %%rsp");
    depth++;
    stack++;
  }

  push_args2(node->args, true);
  push_args2(node->args, false);

  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (node->ret_buffer && node->ty->size > 16) {
    emitfln("  lea %d(%%rbp), %%rax", node->ret_buffer->offset);
    push();
  }

  return stack;
}

static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      emitfln("  movss %%xmm0, %d(%%rbp)", var->offset);
    else
      emitfln("  movsd %%xmm0, %d(%%rbp)", var->offset);
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      emitfln("  mov %%al, %d(%%rbp)", var->offset + i);
      emitfln("  shr $8, %%rax");
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12)
        emitfln("  movss %%xmm%d, %d(%%rbp)", fp, var->offset + 8);
      else
        emitfln("  movsd %%xmm%d, %d(%%rbp)", fp, var->offset + 8);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      for (int i = 8; i < MIN(16, ty->size); i++) {
        emitfln("  mov %s, %d(%%rbp)", reg1, var->offset + i);
        emitfln("  shr $8, %s", reg2);
      }
    }
  }
}

static void copy_struct_reg(void) {
  Type *ty = current_fn->ty->return_ty;
  int gp = 0, fp = 0;

  emitfln("  mov %%rax, %%rdi");

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      emitfln("  movss (%%rdi), %%xmm0");
    else
      emitfln("  movsd (%%rdi), %%xmm0");
    fp++;
  } else {
    emitfln("  mov $0, %%rax");
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      emitfln("  shl $8, %%rax");
      emitfln("  mov %d(%%rdi), %%al", i);
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4)
        emitfln("  movss 8(%%rdi), %%xmm%d", fp);
      else
        emitfln("  movsd 8(%%rdi), %%xmm%d", fp);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      emitfln("  mov $0, %s", reg2);
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        emitfln("  shl $8, %s", reg2);
        emitfln("  mov %d(%%rdi), %s", i, reg1);
      }
    }
  }
}

static void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;
  Obj *var = current_fn->params;

  emitfln("  mov %d(%%rbp), %%rdi", var->offset);

  for (int i = 0; i < ty->size; i++) {
    emitfln("  mov %d(%%rax), %%dl", i);
    emitfln("  mov %%dl, %d(%%rdi)", i);
  }
}

static void builtin_alloca(void) {
  // Align size to 16 bytes.
  emitfln("  add $15, %%rdi");
  emitfln("  and $0xfffffff0, %%edi");

  // Shift the temporary area by %rdi.
  emitfln("  mov %d(%%rbp), %%rcx", current_fn->alloca_bottom->offset);
  emitfln("  sub %%rsp, %%rcx");
  emitfln("  mov %%rsp, %%rax");
  emitfln("  sub %%rdi, %%rsp");
  emitfln("  mov %%rsp, %%rdx");
  emitfln("1:");
  emitfln("  cmp $0, %%rcx");
  emitfln("  je 2f");
  emitfln("  mov (%%rax), %%r8b");
  emitfln("  mov %%r8b, (%%rdx)");
  emitfln("  inc %%rdx");
  emitfln("  inc %%rax");
  emitfln("  dec %%rcx");
  emitfln("  jmp 1b");
  emitfln("2:");

  // Move alloca_bottom pointer.
  emitfln("  mov %d(%%rbp), %%rax", current_fn->alloca_bottom->offset);
  emitfln("  sub %%rdi, %%rax");
  emitfln("  mov %%rax, %d(%%rbp)", current_fn->alloca_bottom->offset);
}

// Generate code for a given node.
static void gen_expr(Node *node) {
  emitfln("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM: {
    switch (node->ty->kind) {
    case TY_FLOAT: {
      union { float f32; uint32_t u32; } u = { node->fval };
      emitfln("  mov $%u, %%eax  # float %Lf", u.u32, node->fval);
      emitfln("  movq %%rax, %%xmm0");
      return;
    }
    case TY_DOUBLE: {
      union { double f64; uint64_t u64; } u = { node->fval };
      emitfln("  mov $%lu, %%rax  # double %Lf", u.u64, node->fval);
      emitfln("  movq %%rax, %%xmm0");
      return;
    }
    case TY_LDOUBLE: {
      union { flt_number f80; uint64_t u64[2]; } u;
      memset(&u, 0, sizeof(u));
      u.f80 = node->fval;
      emitfln("  mov $%lu, %%rax  # long double %Lf", u.u64[0], node->fval);
      emitfln("  mov %%rax, -16(%%rsp)");
      emitfln("  mov $%lu, %%rax", u.u64[1]);
      emitfln("  mov %%rax, -8(%%rsp)");
      emitfln("  fldt -16(%%rsp)");
      return;
    }
    }

    emitfln("  mov $%ld, %%rax", node->val);
    return;
  }
  case ND_NEG:
    gen_expr(node->lhs);

    switch (node->ty->kind) {
    case TY_FLOAT:
      emitfln("  mov $1, %%rax");
      emitfln("  shl $31, %%rax");
      emitfln("  movq %%rax, %%xmm1");
      emitfln("  xorps %%xmm1, %%xmm0");
      return;
    case TY_DOUBLE:
      emitfln("  mov $1, %%rax");
      emitfln("  shl $63, %%rax");
      emitfln("  movq %%rax, %%xmm1");
      emitfln("  xorpd %%xmm1, %%xmm0");
      return;
    case TY_LDOUBLE:
      emitfln("  fchs");
      return;
    }

    emitfln("  neg %%rax");
    return;
  case ND_VAR:
    gen_addr(node);
    load(node->ty);
    return;
  case ND_MEMBER: {
    gen_addr(node);
    load(node->ty);

    Member *mem = node->member;
    if (mem->is_bitfield) {
      emitfln("  shl $%d, %%rax", 64 - mem->bit_width - mem->bit_offset);
      if (mem->ty->is_unsigned)
        emitfln("  shr $%d, %%rax", 64 - mem->bit_width);
      else
        emitfln("  sar $%d, %%rax", 64 - mem->bit_width);
    }
    return;
  }
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);

    if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
      emitfln("  mov %%rax, %%r8");

      // If the lhs is a bitfield, we need to read the current value
      // from memory and merge it with a new value.
      Member *mem = node->lhs->member;
      emitfln("  mov %%rax, %%rdi");
      emitfln("  and $%ld, %%rdi", (1L << mem->bit_width) - 1);
      emitfln("  shl $%d, %%rdi", mem->bit_offset);

      emitfln("  mov (%%rsp), %%rax");
      load(mem->ty);

      long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
      emitfln("  mov $%ld, %%r9", ~mask);
      emitfln("  and %%r9, %%rax");
      emitfln("  or %%rdi, %%rax");
      store(node->ty);
      emitfln("  mov %%r8, %%rax");
      return;
    }

    store(node->ty);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_expr(node->lhs);
    cast(node->lhs->ty, node->ty);
    return;
  case ND_MEMZERO:
    // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
    emitfln("  mov $%d, %%rcx", node->var->ty->size);
    emitfln("  lea %d(%%rbp), %%rdi", node->var->offset);
    emitfln("  mov $0, %%al");
    emitfln("  rep stosb");
    return;
  case ND_COND: {
    count_t c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    emitfln("  je .L.else.%lu", c);
    gen_expr(node->then);
    emitfln("  jmp .L.end.%lu", c);
    emitfln(".L.else.%lu:", c);
    gen_expr(node->els);
    emitfln(".L.end.%lu:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    emitfln("  sete %%al");
    emitfln("  movzx %%al, %%rax");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    emitfln("  not %%rax");
    return;
  case ND_LOGAND: {
    count_t c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    emitfln("  je .L.false.%lu", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    emitfln("  je .L.false.%lu", c);
    emitfln("  mov $1, %%rax");
    emitfln("  jmp .L.end.%lu", c);
    emitfln(".L.false.%lu:", c);
    emitfln("  mov $0, %%rax");
    emitfln(".L.end.%lu:", c);
    return;
  }
  case ND_LOGOR: {
    count_t c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    emitfln("  jne .L.true.%lu", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    emitfln("  jne .L.true.%lu", c);
    emitfln("  mov $0, %%rax");
    emitfln("  jmp .L.end.%lu", c);
    emitfln(".L.true.%lu:", c);
    emitfln("  mov $1, %%rax");
    emitfln(".L.end.%lu:", c);
    return;
  }
  case ND_FUNCALL: {
    if (node->lhs->kind == ND_VAR && !strcmp(get_symbol(node->lhs->var), "alloca")) {
      gen_expr(node->args);
      emitfln("  mov %%rax, %%rdi");
      builtin_alloca();
      return;
    }

    int stack_args = push_args(node);
    gen_expr(node->lhs);

    int gp = 0, fp = 0;

    // If the return type is a large struct/union, the caller passes
    // a pointer to a buffer as if it were the first argument.
    if (node->ret_buffer && node->ty->size > 16)
      pop(argreg64[gp++]);

    for (Node *arg = node->args; arg; arg = arg->next) {
      Type *ty = arg->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size > 16)
          continue;

        bool fp1 = has_flonum1(ty);
        bool fp2 = has_flonum2(ty);

        if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
          if (fp1)
            popf(fp++);
          else
            pop(argreg64[gp++]);

          if (ty->size > 8) {
            if (fp2)
              popf(fp++);
            else
              pop(argreg64[gp++]);
          }
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp < FP_MAX)
          popf(fp++);
        break;
      case TY_LDOUBLE:
        break;
      default:
        if (gp < GP_MAX)
          pop(argreg64[gp++]);
      }
    }

    emitfln("  mov %%rax, %%r10");
    emitfln("  mov $%d, %%rax", fp);
    emitfln("  call *%%r10");
    emitfln("  add $%d, %%rsp", stack_args * 8);

    depth -= stack_args;

    // It looks like the most significant 48 or 56 bits in RAX may
    // contain garbage if a function return type is short or bool/char,
    // respectively. We clear the upper bits here.
    switch (node->ty->kind) {
    case TY_BOOL:
      emitfln("  movzx %%al, %%eax");
      return;
    case TY_CHAR:
      if (node->ty->is_unsigned)
        emitfln("  movzbl %%al, %%eax");
      else
        emitfln("  movsbl %%al, %%eax");
      return;
    case TY_SHORT:
      if (node->ty->is_unsigned)
        emitfln("  movzwl %%ax, %%eax");
      else
        emitfln("  movswl %%ax, %%eax");
      return;
    }

    // If the return type is a small struct, a value is returned
    // using up to two registers.
    if (node->ret_buffer && node->ty->size <= 16) {
      copy_ret_buffer(node->ret_buffer);
      emitfln("  lea %d(%%rbp), %%rax", node->ret_buffer->offset);
    }

    return;
  }
  case ND_LABEL_VAL:
    emitfln("  lea %s(%%rip), %%rax", node->unique_label);
    return;
  case ND_CAS: {
    gen_expr(node->cas_addr);
    push();
    gen_expr(node->cas_new);
    push();
    gen_expr(node->cas_old);
    emitfln("  mov %%rax, %%r8");
    load(node->cas_old->ty->base);
    pop("%rdx"); // new
    pop("%rdi"); // addr

    int sz = node->cas_addr->ty->base->size;
    emitfln("  lock cmpxchg %s, (%%rdi)", reg_dx(sz));
    emitfln("  sete %%cl");
    emitfln("  je 1f");
    emitfln("  mov %s, (%%r8)", reg_ax(sz));
    emitfln("1:");
    emitfln("  movzbl %%cl, %%eax");
    return;
  }
  case ND_EXCH: {
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    pop("%rdi");

    int sz = node->lhs->ty->base->size;
    emitfln("  xchg %s, (%%rdi)", reg_ax(sz));
    return;
  }
  }

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE: {
    gen_expr(node->rhs);
    pushf();
    gen_expr(node->lhs);
    popf(1);

    char *sz = (node->lhs->ty->kind == TY_FLOAT) ? "ss" : "sd";

    switch (node->kind) {
    case ND_ADD:
      emitfln("  add%s %%xmm1, %%xmm0", sz);
      return;
    case ND_SUB:
      emitfln("  sub%s %%xmm1, %%xmm0", sz);
      return;
    case ND_MUL:
      emitfln("  mul%s %%xmm1, %%xmm0", sz);
      return;
    case ND_DIV:
      emitfln("  div%s %%xmm1, %%xmm0", sz);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      emitfln("  ucomi%s %%xmm0, %%xmm1", sz);

      if (node->kind == ND_EQ) {
        emitfln("  sete %%al");
        emitfln("  setnp %%dl");
        emitfln("  and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        emitfln("  setne %%al");
        emitfln("  setp %%dl");
        emitfln("  or %%dl, %%al");
      } else if (node->kind == ND_LT) {
        emitfln("  seta %%al");
      } else {
        emitfln("  setae %%al");
      }

      emitfln("  and $1, %%al");
      emitfln("  movzb %%al, %%rax");
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  case TY_LDOUBLE: {
    gen_expr(node->lhs);
    gen_expr(node->rhs);

    switch (node->kind) {
    case ND_ADD:
      emitfln("  faddp");
      return;
    case ND_SUB:
      emitfln("  fsubrp");
      return;
    case ND_MUL:
      emitfln("  fmulp");
      return;
    case ND_DIV:
      emitfln("  fdivrp");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      emitfln("  fcomip");
      emitfln("  fstp %%st(0)");

      if (node->kind == ND_EQ)
        emitfln("  sete %%al");
      else if (node->kind == ND_NE)
        emitfln("  setne %%al");
      else if (node->kind == ND_LT)
        emitfln("  seta %%al");
      else
        emitfln("  setae %%al");

      emitfln("  movzb %%al, %%rax");
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("%rdi");

  char *ax, *di, *dx;

  if (node->lhs->ty->kind == TY_LONG || node->lhs->ty->base) {
    ax = "%rax";
    di = "%rdi";
    dx = "%rdx";
  } else {
    ax = "%eax";
    di = "%edi";
    dx = "%edx";
  }

  switch (node->kind) {
  case ND_ADD:
    emitfln("  add %s, %s", di, ax);
    return;
  case ND_SUB:
    emitfln("  sub %s, %s", di, ax);
    return;
  case ND_MUL:
    emitfln("  imul %s, %s", di, ax);
    return;
  case ND_DIV:
  case ND_MOD:
    if (node->ty->is_unsigned) {
      emitfln("  mov $0, %s", dx);
      emitfln("  div %s", di);
    } else {
      if (node->lhs->ty->size == 8)
        emitfln("  cqo");
      else
        emitfln("  cdq");
      emitfln("  idiv %s", di);
    }

    if (node->kind == ND_MOD)
      emitfln("  mov %%rdx, %%rax");
    return;
  case ND_BITAND:
    emitfln("  and %s, %s", di, ax);
    return;
  case ND_BITOR:
    emitfln("  or %s, %s", di, ax);
    return;
  case ND_BITXOR:
    emitfln("  xor %s, %s", di, ax);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    emitfln("  cmp %s, %s", di, ax);

    if (node->kind == ND_EQ) {
      emitfln("  sete %%al");
    } else if (node->kind == ND_NE) {
      emitfln("  setne %%al");
    } else if (node->kind == ND_LT) {
      if (node->lhs->ty->is_unsigned)
        emitfln("  setb %%al");
      else
        emitfln("  setl %%al");
    } else if (node->kind == ND_LE) {
      if (node->lhs->ty->is_unsigned)
        emitfln("  setbe %%al");
      else
        emitfln("  setle %%al");
    }

    emitfln("  movzb %%al, %%rax");
    return;
  case ND_SHL:
    emitfln("  mov %%rdi, %%rcx");
    emitfln("  shl %%cl, %s", ax);
    return;
  case ND_SHR:
    emitfln("  mov %%rdi, %%rcx");
    if (node->lhs->ty->is_unsigned)
      emitfln("  shr %%cl, %s", ax);
    else
      emitfln("  sar %%cl, %s", ax);
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  emitfln("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_NOP:
    return;
  case ND_IF: {
    count_t c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    emitfln("  je  .L.else.%lu", c);
    gen_stmt(node->then);
    emitfln("  jmp .L.end.%lu", c);
    emitfln(".L.else.%lu:", c);
    if (node->els)
      gen_stmt(node->els);
    emitfln(".L.end.%lu:", c);
    return;
  }
  case ND_FOR: {
    count_t c = count();

    /* Dive into the loop */
    Node *prev_loop = current_loop;
    current_loop = node;

    // Reset defers
    node->brk_defers_count  = NO_DEFER;
    node->cont_defers_count = NO_DEFER;

    if (node->init)
      gen_stmt(node->init);
    emitfln(".L.begin.%lu:", c);
    if (node->cond) {
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      emitfln("  je %s", node->brk_label);
    }
    gen_stmt(node->then);
    emitfln("%s:", node->cont_label);
    gen_defers_cont();
    if (node->inc)
      gen_expr(node->inc);
    emitfln("  jmp .L.begin.%lu", c);
    emitfln("%s:", node->brk_label);
    gen_defers_brk();
    current_loop = prev_loop;
    return;
  }
  case ND_DO: {
    count_t c = count();

    /* Dive into the loop */
    Node *prev_loop = current_loop;
    current_loop = node;

    // Reset defers
    node->brk_defers_count  = NO_DEFER;
    node->cont_defers_count = NO_DEFER;

    emitfln(".L.begin.%lu:", c);
    gen_stmt(node->then);
    emitfln("%s:", node->cont_label);
    gen_defers_cont();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    emitfln("  jne .L.begin.%lu", c);
    emitfln("%s:", node->brk_label);
    gen_defers_brk();
    current_loop = prev_loop;
    return;
  }
  case ND_SWITCH:
    gen_expr(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
      char *ax = (node->cond->ty->size == 8) ? "%rax" : "%eax";
      char *di = (node->cond->ty->size == 8) ? "%rdi" : "%edi";

      if (n->begin == n->end) {
        emitfln("  cmp $%ld, %s", n->begin, ax);
        emitfln("  je %s", n->label);
        continue;
      }

      // [GNU] Case ranges
      emitfln("  mov %s, %s", ax, di);
      emitfln("  sub $%ld, %s", n->begin, di);
      emitfln("  cmp $%ld, %s", n->end - n->begin, di);
      emitfln("  jbe %s", n->label);
    }

    if (node->default_case)
      emitfln("  jmp %s", node->default_case->label);

    emitfln("  jmp %s", node->brk_label);
    gen_stmt(node->then);
    emitfln("%s:", node->brk_label);
    return;
  case ND_CASE:
    emitfln("%s:", node->label);
    gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    emitfln("  jmp %s", node->unique_label);
    return;
  case ND_BREAK:
    /* It can be a switch statement */
    if (!current_loop || current_loop->brk_defers_count == NO_DEFER)
      emitfln("  jmp %s", node->unique_label);
    else
      emitfln("  jmp %s.defer$brk.%lu", node->unique_label, current_loop->brk_defers_count);
    return;
  case ND_CONTINUE:
    if (!current_loop || current_loop->cont_defers_count == NO_DEFER)
      emitfln("  jmp %s", node->unique_label);
    else
      emitfln("  jmp %s.defer$cont.%lu", node->unique_label, current_loop->cont_defers_count);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    emitfln("  jmp *%%rax");
    return;
  case ND_LABEL:
    emitfln("%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN:
    if (node->lhs) {
      gen_expr(node->lhs);
      Type *ty = node->lhs->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size <= 16)
          copy_struct_reg();
        else
          copy_struct_mem();
        break;
      }
    }

    /* Jump to return label, this might be a defer statement to run before return */
    char *symbol = get_symbol(current_fn);
    if (current_defer_fn == NO_DEFER)
      emitfln("  jmp .L.return.%s", symbol);
    else
      emitfln("  jmp .L.defer.%s.%lu", symbol, current_defer_fn);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  case ND_ASM:
    emitfln("  %s", node->asm_str);
    return;
  /* SuperC */
  case ND_DEFER:
    switch (node->val) {
    case DK_FUNCTION:
      current_defer_fn++;
      break;
    case DK_BREAK:
      current_loop->brk_defers_count++;
      break;
    case DK_CONTINUE:
      current_loop->cont_defers_count++;
      break;
    }
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static void gen_defers_brk() {
  for (Node *defer_node = current_loop->brk_defers; defer_node; defer_node = defer_node->next) {
    count_t saved_depth = depth; /* Prevent stack overflow */
    emitfln("%s.defer$brk.%lu:", current_loop->brk_label, current_loop->brk_defers_count--);
    gen_stmt(defer_node);
    depth = saved_depth;
  }
}

static void gen_defers_cont() {
  for (Node *defer_node = current_loop->cont_defers; defer_node; defer_node = defer_node->next) {
    count_t saved_depth = depth; /* Prevent stack overflow */
    emitfln("%s.defer$cont.%lu:", current_loop->cont_label, current_loop->cont_defers_count--);
    gen_stmt(defer_node);
    depth = saved_depth;
  }
}

static const char *llvm_type_for_size(int bytes, bool is_flo) {
  if (bytes <= 1)      return "i8";
  else if (bytes <= 2) return "i16";
  else if (bytes <= 4) return !is_flo ? "i32" : "float";
  else if (bytes <= 8) return !is_flo ? "i64" : "double";
  else return format("[%d x i8]", bytes);
}

static Type *list_structs = &(Type){0};

#define _push_struct(ty) ({  \
  Type *ty_ = copy_type((ty)); \
  ty_->next = list_structs;  \
  list_structs = ty_;        \
})

static void detect_member_types(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (!var->is_definition)
      continue;

    /* Seek global variables */
    if (var->ty->kind == TY_STRUCT || var->ty->kind == TY_UNION){
      _push_struct(var->ty);
      for (Member *mem = var->ty->members; mem; mem = mem->next) {
        if (mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION)
          _push_struct(mem->ty);
      }
      continue;
    }

    if (!var->is_function)
      continue;

    /* Seek function parameters */
    for (Obj *param = var->params; param; param = param->next) {
      if (param->ty->kind == TY_STRUCT || param->ty->kind == TY_UNION)
        _push_struct(param->ty);
    }

    if (var->is_definition) {
      /* Seek function local variables */
      for (Obj *local = var->locals; local; local = local->next) {
        if (local->ty->kind == TY_STRUCT || local->ty->kind == TY_UNION)
          _push_struct(local->ty);
      }
    }
  }
}

#undef _push_struct

static void emit_member_types() {
  for (Type *ty = list_structs; ty; ty = ty->next) {
    emitf("%s = type ", llvm_type(ty));

    /* == Unions == */
    if (ty->kind == TY_UNION) {
      // %union.numbers = type { i64 }
      // %union.idk_bigf = type { double, [504 x i8] } ; _Alignas(512)
      Member *mem = ty->members;
      int max_size = mem->align;
      bool is_flo = is_flonum(mem->ty);

      while (mem->next) {
        Member *mem_next = mem->next;
        if (mem_next->align > max_size) {
          max_size = mem_next->align;
          is_flo = is_flonum(mem_next->ty);
        }
        mem = mem_next;
      }

      if (max_size <= 8)
        emitfln("{ %s }", llvm_type_for_size(max_size, is_flo));
      else
        emitfln("{ %s, %s }", !is_flo ? "i64" : "double", llvm_type_for_size(max_size - 8, is_flo));
      continue;
    }

    /* == Structs == */

    // %struct.color = type { i8, i8, i8, i8 }
    // %struct.Car = type { i8*, %struct.color, i32 }
    // %struct.mypacked = type <{ i8, i64 }>

    if (ty->is_packed)
      emitc('<');
    emitc('{');

    bool first = true;
    for (Member *mem = ty->members; mem; mem = mem->next) {
      if (!first)
        emitc(',');
      first = false;

      if (!mem->is_bitfield) {
        emitf(" %s", llvm_type(mem->ty));
        continue;
      }

      /* Handle bitfields */
      unsigned int bits = mem->bit_width;
      while (mem->next && mem->next->is_bitfield) {
        Member *mem_next = mem->next;
        bits += mem_next->bit_width;
        mem = mem_next;
      }

      /* Select bytespan */
      int bytes = ((bits % 8) == 0)
                    ? bits / 8
                    : (bits / 8) + 1;

      emitf(" %s", llvm_type_for_size(bytes, false));

      if (!mem) break; /* Avoid crashing the for loop */
    }

    emitf(" }");
    if (ty->is_packed)
      emitc('>');

    emitln;
  }
}

static void emit_float_lit(TypeKind kind, flt_number fval) {
  switch(kind) {
    case TY_FLOAT:
    case TY_DOUBLE:
      emitf("%.6e", (double)fval);
      break;
    case TY_LDOUBLE: {
      /* FIXME: x86_fp80 is not well emitted */
      unsigned char bytes[10];
      memcpy(bytes, &fval, 10);
      emitf("0xK");
      for (int i = 9; i >= 0; i--)
        emitf("%02X", bytes[i]);
    } break;
  }
}

static void emit_gep(char *symbol, bool is_sym_global, Type *lty,
                     size_t indices_len, int64_t *indices) {
  emitf("getelementptr%s (%s, %s* %c%s",
    indices ? " inbounds" : "",
    llvm_type(lty), llvm_type(lty),
    is_sym_global ? '@' : '%', symbol);

  for (size_t i = 0; i < indices_len; i++)
    emitf(", i32 %ld", indices[i]);

  emitc(')');
}

static void emit_initializer(Initializer *init) {
  if (!init) {
    /* Zero initializer (BSS) */
    emitf("zeroinitializer");
    return;
  }

  /* Basic initializer */
  switch (init->ty->kind) {
    case TY_BOOL:
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
    case TY_LONG: {
      emitf("%ld", init->expr->val);
    } break;

    case TY_FLOAT:
    case TY_DOUBLE:
    case TY_LDOUBLE: {
      emit_float_lit(init->ty->kind, init->expr->fval);
    } break;

    case TY_STRUCT: {
      emitc('{');
      bool first = true;
      for (Member *mem = init->ty->members; mem; mem = mem->next) {
        Initializer *child = init->children[mem->idx];

        /* Bitfields can be merged previously, so skip */
        if (!child->expr && !child->children) continue;

        if (!first) emitc(',');
        first = false;
        emitf(" %s ", llvm_type(child->ty));
        emit_initializer(child);
      }
      emitf(" }");
    } break;

    case TY_UNION: {
      emitc('{');

      /* Get the last initializer */
      Initializer *valid;
      for (Member *mem = init->ty->members; mem && mem->next; mem = mem->next) {
        Initializer *child = init->children[mem->idx];
        /* Skip invalid values */
        if (child->expr || child->children)
          valid = child;
      }

      emitf(" %s ", llvm_type(valid->ty));
      emit_initializer(valid);

      emitf(" }");
    } break;

    case TY_ARRAY: {
      /* Special case for strings */
      if (init->ty->base->kind == TY_CHAR &&
          init->ty->base->size == 1 &&
          init->ty->base->align == 1) {
        emitc('c');
        emitc('"');

        for (size_t i = 0; i < init->ty->array_len; ++i) {
          Initializer *child = init->children[i];
          unsigned char ch = child->expr->val;

          /* Emit normal letter character */
          if (ch >= ' ' && ch < 127)
            emitf("%c", ch);
          else
            emitf("\\%02X", ch);
        }

        emitc('"');
        break;
      }

      emitc('[');
      bool first = true;
      for (size_t i = 0; i < init->ty->array_len; ++i) {
        Initializer *child = init->children[i];
        if (!first) emitf(", ");
        first = false;
        emitf("%s ", llvm_type(child->ty));
        emit_initializer(child);
      }
      emitf("]");
    } break;

    case TY_PTR: {
      switch (init->expr->kind) {
        case ND_ADDR: {
          /* Resolve pointer */
          Obj *rel = init->expr->lhs->var;
          emitf("bitcast (%s @%s to %s)", llvm_type(init->expr->ty), get_symbol(rel), llvm_type(init->ty));
        } break;

        case ND_VAR: {
          char *symbol = get_symbol(init->expr->var);
          int64_t indices[] = { 0, 0 };
          emit_gep(symbol, true, init->expr->ty, 2, indices);
        } break;

        case ND_DEREF: {
          char **label;
          uint64_t val = eval2(init->expr->lhs, &label);

          // FIXME: Get real value
          int64_t indices[] = { 0, val };

          emit_gep(label[0], true, init->expr->ty, 2, indices);
        } break;
      }
    } break;
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (var->body && var->body->kind == ND_ASM) {
      /* Global assembly statement */
      emitfln("%s", var->body->asm_str);
      continue;
    }

    const char *symbol = get_symbol(var);
    const char *llty = llvm_type(var->ty);

    emitf("@%s = ", symbol);

    if (var->is_puc_addr) {
      emitf("private unnamed_addr constant ");
    } else {
      if (var->is_static)
        emitf("internal ");
      else
        emitf("dso_local ");
      emitf("global ");
    }

    emitf("%s ", llty);
    emit_initializer(var->init);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    emitf(", align %d", align);
    emitln;
  }
}

static void store_fp(int r, int offset, int sz) {
  switch (sz) {
  case 4:
    emitfln("  movss %%xmm%d, %d(%%rbp)", r, offset);
    return;
  case 8:
    emitfln("  movsd %%xmm%d, %d(%%rbp)", r, offset);
    return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
  case 1:
    emitfln("  mov %s, %d(%%rbp)", argreg8[r], offset);
    return;
  case 2:
    emitfln("  mov %s, %d(%%rbp)", argreg16[r], offset);
    return;
  case 4:
    emitfln("  mov %s, %d(%%rbp)", argreg32[r], offset);
    return;
  case 8:
    emitfln("  mov %s, %d(%%rbp)", argreg64[r], offset);
    return;
  default:
    for (int i = 0; i < sz; i++) {
      emitfln("  mov %s, %d(%%rbp)", argreg8[r], offset + i);
      emitfln("  shr $8, %s", argreg64[r]);
    }
    return;
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    const char *symbol = get_symbol(fn);
    current_defer_fn = NO_DEFER; // Reset defers

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    if (fn->is_static)
      emitfln("  .local %s", symbol);
    else
      emitfln("  .globl %s", symbol);

    emitfln("  .text");
    emitfln("  .type %s, @function", symbol);
    emitfln("%s:", symbol);
    current_fn = fn;

    // Prologue
    emitfln("  push %%rbp");
    emitfln("  mov %%rsp, %%rbp");
    emitfln("  sub $%d, %%rsp", fn->stack_size);
    emitfln("  mov %%rsp, %d(%%rbp)", fn->alloca_bottom->offset);

    // Save arg registers if function is variadic
    if (fn->va_area) {
      int gp = 0, fp = 0;
      for (Obj *var = fn->params; var; var = var->next) {
        if (is_flonum(var->ty))
          fp++;
        else
          gp++;
      }

      int off = fn->va_area->offset;

      // va_elem
      emitfln("  movl $%d, %d(%%rbp)", gp * 8, off);          // gp_offset
      emitfln("  movl $%d, %d(%%rbp)", fp * 8 + 48, off + 4); // fp_offset
      emitfln("  movq %%rbp, %d(%%rbp)", off + 8);            // overflow_arg_area
      emitfln("  addq $16, %d(%%rbp)", off + 8);
      emitfln("  movq %%rbp, %d(%%rbp)", off + 16);           // reg_save_area
      emitfln("  addq $%d, %d(%%rbp)", off + 24, off + 16);

      // __reg_save_area__
      emitfln("  movq %%rdi, %d(%%rbp)", off + 24);
      emitfln("  movq %%rsi, %d(%%rbp)", off + 32);
      emitfln("  movq %%rdx, %d(%%rbp)", off + 40);
      emitfln("  movq %%rcx, %d(%%rbp)", off + 48);
      emitfln("  movq %%r8, %d(%%rbp)", off + 56);
      emitfln("  movq %%r9, %d(%%rbp)", off + 64);
      emitfln("  movsd %%xmm0, %d(%%rbp)", off + 72);
      emitfln("  movsd %%xmm1, %d(%%rbp)", off + 80);
      emitfln("  movsd %%xmm2, %d(%%rbp)", off + 88);
      emitfln("  movsd %%xmm3, %d(%%rbp)", off + 96);
      emitfln("  movsd %%xmm4, %d(%%rbp)", off + 104);
      emitfln("  movsd %%xmm5, %d(%%rbp)", off + 112);
      emitfln("  movsd %%xmm6, %d(%%rbp)", off + 120);
      emitfln("  movsd %%xmm7, %d(%%rbp)", off + 128);
    }

    // Save passed-by-register arguments to the stack
    int gp = 0, fp = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->offset > 0)
        continue;

      Type *ty = var->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        assert(ty->size <= 16);
        if (has_flonum(ty, 0, 8, 0))
          store_fp(fp++, var->offset, MIN(8, ty->size));
        else
          store_gp(gp++, var->offset, MIN(8, ty->size));

        if (ty->size > 8) {
          if (has_flonum(ty, 8, 16, 0))
            store_fp(fp++, var->offset + 8, ty->size - 8);
          else
            store_gp(gp++, var->offset + 8, ty->size - 8);
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        store_fp(fp++, var->offset, ty->size);
        break;
      default:
        store_gp(gp++, var->offset, ty->size);
      }
    }

    // Emit code
    gen_stmt(fn->body);
    assert(depth == 0);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(symbol, "main") == 0)
      emitfln("  mov $0, %%rax");

    // Epilogue

    if (fn->defers) {
      push(); /* Save the return value (for if it's destroyed) */

      /* Emit deferred statements */
      for (Node *defer_node = fn->defers; defer_node; defer_node = defer_node->next) {
        count_t save_depth = depth; /* Prevent stack overflow */
        emitfln(".L.defer.%s.%lu:", symbol, current_defer_fn--);
        gen_stmt(defer_node);
        depth = save_depth;
      }

      pop("%rax"); /* Recover the return value */
    }

    emitfln(".L.return.%s:", symbol);
    emitfln("  mov %%rbp, %%rsp");
    emitfln("  pop %%rbp");
    emitfln("  ret");
  }
}

void codegen(Obj *prog, FILE *out) {
  output_file = out;

  char *mtriple = "x86_64-pc-linux-gnu";
  char *dlayout_mangle = "m:e";
  char *dlayout_processor = "p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128";

  /* LLVM TEST */
  File **files = get_input_files();
  assert(files[0]); // Must have at least one input file
  char *filename = files[0]->name;
  emitfln("; ModuleID = '%s'", filename);
  emitfln("source_filename = \"%s\"", filename);
  emitfln("target datalayout = \"e-%s-%s\"", dlayout_mangle, dlayout_processor);
  emitfln("target triple = \"%s\"", mtriple);
  emitln;

  detect_member_types(prog);
  emit_member_types();
  emitln;

  emit_data(prog);
  emitln;

  // emit_text(prog);
}
