#include "superc.h"

static FILE *output_file;
static Obj  *current_fn;
static Node *current_loop;

static LLVM   *gen_expr(Node *node);
static void    gen_stmt(Node *node);
static count_t emit_llvm(LLVM *llvm);

LLVM *fn_retval_ll = NULL;
Label fn_label_ret = {};

static count_t ssa_id = 1;
#define new_ssa ssa_id++

static LLVM *_emit_cur;

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

const char *get_symbol(Obj *var) {
  if (var->symbol)
    return var->symbol;
  return var->name;
}

enum { I1, I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };
#define USIZE U64

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_BOOL:
    return I1;
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
  return USIZE;
}

static const char *llvm_type_for_size(int bytes, bool is_flo) {
  if (bytes <= 1)      return "i8";
  else if (bytes <= 2) return "i16";
  else if (bytes <= 4) return !is_flo ? "i32" : "float";
  else if (bytes <= 8) return !is_flo ? "i64" : "double";
  else return format("[%d x i8]", bytes);
}

static const char *get_float_lit(TypeKind kind, flt_number fval) {
  switch(kind) {
    case TY_FLOAT:
    case TY_DOUBLE:
      return format("%.6e", (double)fval);
    case TY_LDOUBLE: {
      /* FIXME: x86_fp80 is not well emitted */
      unsigned char bytes[10];
      memcpy(bytes, &fval, 10);
      StringBuilder sb;
      sb_init(&sb);
      sb_append(&sb, "0xK");
      for (int i = 9; i >= 0; i--)
        sb_appendf(&sb, "%02X", bytes[i]);

      const char *res = strndup(sb.buf, sb.len);
      sb_free(&sb);
      return res;
    }
  }
  unreachable();
}

static const char *get_symvar(LLVM *llvm) {
  switch (llvm->kind) {
  case LL_NUM:
    return format("%ld", llvm->val);
  case LL_NUMF:
    return get_float_lit(llvm->ty->kind, llvm->fval);
  case LL_VAR:
    Obj *var = llvm->var;
    if (var->is_local)
      return format("%%%ld", var->llvm->ssa);
    else
      return format("@%s", get_symbol(var));
  default:
    return format("%%%ld", llvm->ssa);
  }
}

static inline bool is_assignable_ll(LLKind kind) {
  switch (kind) {
  /* List of instructions that don't emit an SSA */
  case LL_JMP:
  case LL_STORE:
    return false;
  default:
    return true;
  }
}

static LLVM *gen_jmp(Label *label) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_JMP;
  llvm->label = label;
  _emit_cur = _emit_cur->next = llvm;
  return llvm;
}

static LLVM *gen_addr(Node *node) {
  assert(node->kind == ND_VAR);
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_VAR;
  llvm->ty = node->ty;
  llvm->var = node->var;
  return llvm;
}

static LLVM *gen_num(Type *ty, int64_t val) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_NUM;
  llvm->ty = ty;
  llvm->val = val;
  return llvm;
};

static LLVM *gen_numf(Type *ty, flt_number val) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_NUM;
  llvm->ty = ty;
  llvm->fval = val;
  return llvm;
}

static LLVM *gen_alloca(Type *ty) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_ALLOCA;
  llvm->ty = ty;

  _emit_cur = _emit_cur->next = llvm;
  return llvm;
}

static LLVM *builtint_alloca(count_t size) {
  Type *ty = copy_type(ty_char);
  ty->align = size;
  return gen_alloca(ty);
}

static LLVM *gen_load(Type *ty, LLVM *src) {
  // assert(src);

  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_LOAD;
  llvm->ty = ty;
  llvm->src = src;

  _emit_cur = _emit_cur->next = llvm;
  return llvm;
}

static LLVM *gen_store(Type *ty, LLVM *src, LLVM *dst) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_STORE;
  llvm->ty = ty;
  llvm->src = src;
  llvm->dst = dst;

  _emit_cur = _emit_cur->next = llvm;
  return llvm;
}

static LLKind cast_table[][12] = {
// i1     i8       i16      i32      i64      u8       u16      u32      u64      f32      f64      f80
{LL_NOOP, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_UI_F, LL_UI_F, LL_UI_F}, // i1
{LL_TRUN, LL_NOOP, LL_SEXT, LL_SEXT, LL_SEXT, LL_NOOP, LL_SEXT, LL_SEXT, LL_SEXT, LL_SI_F, LL_SI_F, LL_SI_F}, // i8
{LL_TRUN, LL_TRUN, LL_NOOP, LL_SEXT, LL_SEXT, LL_TRUN, LL_NOOP, LL_SEXT, LL_SEXT, LL_SI_F, LL_SI_F, LL_SI_F}, // i16
{LL_TRUN, LL_TRUN, LL_TRUN, LL_NOOP, LL_SEXT, LL_TRUN, LL_TRUN, LL_NOOP, LL_SEXT, LL_SI_F, LL_SI_F, LL_SI_F}, // i32
{LL_TRUN, LL_TRUN, LL_TRUN, LL_TRUN, LL_NOOP, LL_TRUN, LL_TRUN, LL_TRUN, LL_NOOP, LL_SI_F, LL_SI_F, LL_SI_F}, // i64
{LL_TRUN, LL_NOOP, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_NOOP, LL_ZEXT, LL_ZEXT, LL_ZEXT, LL_SI_F, LL_SI_F, LL_SI_F}, // u8
{LL_TRUN, LL_TRUN, LL_NOOP, LL_ZEXT, LL_ZEXT, LL_TRUN, LL_NOOP, LL_ZEXT, LL_ZEXT, LL_SI_F, LL_SI_F, LL_SI_F}, // u16
{LL_TRUN, LL_TRUN, LL_TRUN, LL_NOOP, LL_ZEXT, LL_TRUN, LL_TRUN, LL_NOOP, LL_ZEXT, LL_SI_F, LL_SI_F, LL_SI_F}, // u32
{LL_TRUN, LL_TRUN, LL_TRUN, LL_TRUN, LL_NOOP, LL_TRUN, LL_TRUN, LL_TRUN, LL_NOOP, LL_SI_F, LL_SI_F, LL_SI_F}, // u64
{LL_F_UI, LL_F_SI, LL_F_SI, LL_F_SI, LL_F_SI, LL_F_UI, LL_F_UI, LL_F_UI, LL_F_UI, LL_NOOP, LL_FEXT, LL_FEXT}, // f32
{LL_F_UI, LL_F_SI, LL_F_SI, LL_F_SI, LL_F_SI, LL_F_UI, LL_F_UI, LL_F_UI, LL_F_UI, LL_FTRN, LL_NOOP, LL_FEXT}, // f64
{LL_F_UI, LL_F_SI, LL_F_SI, LL_F_SI, LL_F_SI, LL_F_UI, LL_F_UI, LL_F_UI, LL_F_UI, LL_FTRN, LL_FTRN, LL_NOOP}, // f80
};

static LLVM *gen_cast(Type *from, Type *to, LLVM *ref) {
  LLVM *llvm = calloc(1, sizeof(LLVM));

  switch (from->kind) {
  case TY_PTR:
    llvm->kind = LL_BITCAST;
    break;
  case TY_BOOL:
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
    // Use table lookup for now
    int t1 = getTypeId(from);
    int t2 = getTypeId(to);
    llvm->kind = cast_table[t1][t2];
    break;
  default:
    unreachable();
  }

  llvm->ty = to;
  llvm->src = ref;

  _emit_cur = _emit_cur->next = llvm;
  return llvm;
}

static LLVM *gen_expr(Node *node) {
  switch (node->kind) {
    case ND_NULL_EXPR:
      return NULL;
    case ND_COMMA:
      gen_expr(node->lhs);
      gen_expr(node->rhs);
      return NULL;
    case ND_STMT_EXPR:
      for (Node *n = node->body; n; n = n->next)
        gen_stmt(n);
      return NULL;
    case ND_NUM: {
      if (is_flonum(node->ty))
        return gen_numf(node->ty, node->fval);
      return gen_num(node->ty, node->val);
    }
    case ND_VAR: {
      // rvalue of a var = load from its address
      return gen_load(node->ty, gen_addr(node));
    }
    case ND_CAST: {
      return gen_cast(node->lhs->ty, node->ty, gen_expr(node->lhs));
    }
    case ND_ASSIGN: {
      // lhs must be an lvalue
      LLVM *ptr = gen_addr(node->lhs);
      LLVM *rhs = gen_expr(node->rhs);
      return gen_store(node->ty, rhs, ptr);
    }
    case ND_FUNCALL: {
      if (node->lhs->kind == ND_VAR &&
        !strcmp(get_symbol(node->lhs->var), "__builtin_alloca")) {
        // gen_expr(node->args);
        int64_t sz = eval2(node->args, NULL);
        return builtint_alloca(sz);
      }
      error_tok(node->tok, "unsupported funcall");
    }
    default:
      error_tok(node->tok, "unsupported rvalue kind in minimal IR");
  }
  unreachable();
}

static void gen_stmt(Node *node) {
  switch (node->kind) {
    case ND_BLOCK:
      for (Node *m = node->body; m; m = m->next)
        gen_stmt(m);
      break;

    case ND_EXPR_STMT:
      gen_expr(node->lhs);
      break;

    case ND_RETURN: {
      if (node->lhs)
        gen_store(current_fn->ty->return_ty, gen_expr(node->lhs), fn_retval_ll);
      gen_jmp(&fn_label_ret);
      break;
    }

    default:
      error_tok(node->tok, "unsupported stmt in minimal IR");
  }

}

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

static count_t emit_alloca(count_t ssa, LLVM *ll) {
  assert(ll);
  if (!ssa) ssa = ll->ssa;
  emitfln("  %%%ld = alloca %s, align %d", ssa, llvm_type(ll->ty), ll->ty->align);
  return ssa;
}

static count_t emit_load(count_t ssa, LLVM *ll) {
  assert(ll);
  assert(ll->src);

  if (!ssa) ssa = ll->ssa;
  const char *llty = llvm_type(ll->ty);
  emitfln("  %%%ld = load %s, %s* %s, align %d", ssa,
          llty, llty, get_symvar(ll->src), ll->ty->align);

  return ssa;
}

static count_t emit_label(Label *label) {
  assert(label);
  emitfln("%ld:", label->ssa);
  return label->ssa;
}

static count_t emit_store(LLVM *src, LLVM *dst) {
  assert(src);
  assert(dst);
  emitfln("  store %s %s, %s* %s, align %d",
          llvm_type(src->ty), get_symvar(src),
          llvm_type(dst->ty), get_symvar(dst),
          dst->ty->align);
  return 0;
}

static count_t emit_llvm(LLVM *llvm) {
  switch (llvm->kind) {
  case LL_JMP:
    emitfln("  br label %%%ld", llvm->label->ssa);
    return llvm->ssa;
  case LL_ALLOCA:
    return emit_alloca(0, llvm);
  case LL_LOAD:
    return emit_load(0, llvm);
  case LL_STORE:
    return emit_store(llvm->src, llvm->dst);
  case LL_LABEL:
    return emit_label(llvm->label);
  case LL_TRUN:
    emitfln("  %%%ld = trunc %s %s to %s", llvm->ssa,
            llvm_type(llvm->src->ty), get_symvar(llvm->src),
            llvm_type(llvm->ty));
    return llvm->ssa;
  case LL_ZEXT:
    emitfln("  %%%ld = zext %s %s to %s", llvm->ssa,
            llvm_type(llvm->src->ty), get_symvar(llvm->src),
            llvm_type(llvm->ty));
    return llvm->ssa;
  case LL_SEXT:
    emitfln("  %%%ld = sext %s %s to %s", llvm->ssa,
            llvm_type(llvm->src->ty), get_symvar(llvm->src),
            llvm_type(llvm->ty));
    return llvm->ssa;
  }
  return 0;
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    /* Reset LLVM cursor */
    LLVM ll_head;
    memset(&ll_head, 0, sizeof(LLVM));
    _emit_cur = &ll_head;

    ssa_id = 1; // Reset SSA indexes

    current_fn = fn;
    const char *symbol = get_symbol(fn);

    Type *ret_ty = fn->ty->return_ty;
    const char *ll_ret_ty = llvm_type(ret_ty);

    /* ==== Function header ==== */
    emitf("define");
    if (fn->is_static)
      emitf(" internal");
    else
      emitf(" dso_local");

    count_t attr_num = 0; // TODO: emit attributes
    emitfln(" %s @%s() #%ld {", ll_ret_ty, symbol, attr_num);

    /* ==== Prologue ==== */

    /* Allocate return value */
    if (ret_ty->kind != TY_VOID) {
      fn_retval_ll = gen_alloca(ret_ty);
    } else
     fn_retval_ll = NULL;

    /* Allocate local variables */
    for (Obj *local = fn->locals; local; local = local->next) {
      LLVM * ref = gen_alloca(local->ty);
      /* Create cross-reference so it can be addressed */
      local->llvm = ref;
    }

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(symbol, "main") == 0)
      gen_store(ret_ty, gen_num(ret_ty, 0), fn_retval_ll);

    /* ==== BODY ==== */

    /* Generate LLVM IR AST */
    gen_stmt(fn->body);
    LLVM *ll_body = ll_head.next; /* Skip dummy head */

    /* Enumerate instructions before emitting */
    for (LLVM *ll = ll_body; ll; ll = ll->next) {
      if (is_assignable_ll(ll->kind))
        ll->ssa = new_ssa;
    }
    /* Return label receive the last SSA */
    fn_label_ret.ssa = new_ssa;

    /* Emit LLVM IR*/
    while (ll_body) {
      emit_llvm(ll_body);
      ll_body = ll_body->next;
    }

    /* ==== Epilogue ==== */
    // TODO: Defers
    emit_label(&fn_label_ret);

    if (ret_ty->kind == TY_VOID)
      emitf("  ret void");
    else
      emitf("  ret %s %%%ld", llvm_type(ret_ty),
            emit_load(new_ssa, gen_load(ret_ty, fn_retval_ll)));

    emitln;
    emitc('}');
    emitln;
    emitln;
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

  // detect_member_types(prog);
  // emit_member_types();
  // emitln;

  // emit_data(prog);
  // emitln;

  emit_text(prog);
}
