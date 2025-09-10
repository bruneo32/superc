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

int align_down(int n, int align) {
  return align_to(n - align + 1, align);
}

const char *get_symbol(Obj *var) {
  if (var->symbol)
    return var->symbol;
  return var->name;
}

static inline bool ty_is_pointer(Type *ty) {
  return ty->kind == TY_PTR || ty->kind == TY_ARRAY;
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

static inline bool is_float_tid(int tid) {
  return tid == F32 || tid == F64 || tid == F80;
}

static const char *llvm_type_for_size(int bytes, bool is_flo) {
  if (bytes <= 1)      return "i8";
  else if (bytes <= 2) return "i16";
  else if (bytes <= 4) return !is_flo ? "i32" : "float";
  else if (bytes <= 8) return !is_flo ? "i64" : "double";
  else return format("[%d x i8]", bytes);
}

static const char *llvmty_usize;

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

static LLVM *gen_inum(Type *ty, int64_t val) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_NUM;
  llvm->ty = ty;
  llvm->val = val;
  return llvm;
};

static LLVM *gen_fnum(Type *ty, flt_number val) {
  LLVM *llvm = calloc(1, sizeof(LLVM));
  llvm->kind = LL_NUM;
  llvm->ty = ty;
  llvm->fval = val;
  return llvm;
}

static inline LLVM *gen_num(Type *ty, Node *node) {
  if (is_flonum(ty))
    return gen_fnum(ty, node->fval);
  return gen_inum(ty, node->val);
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
  assert(src);

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

  if (llvm->kind == LL_NOOP)
    return ref;

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
      return gen_num(node->ty, node);
    }
    case ND_VAR: {
      // rvalue of a var = load from its address
      return gen_load(node->ty, gen_addr(node));
    }
    case ND_CAST: {
      /* If lhs is a primitive, don't need to cast,
       * just return it as the other kind */
      if (node->lhs->kind == ND_NUM) {
        int t1 = getTypeId(node->ty);
        int t2 = getTypeId(node->lhs->ty);
        /* But, if one is float and another is int, then we need to cast it */
        if (t1 != I1 && t2 != I1 &&
            is_float_tid(t1) == is_float_tid(t2))
          return gen_num(node->ty, node->lhs);
      }
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

static void emitd_initializer(Initializer *init);

static Node *get_const_expr_di(Node *node, Type *cast_ty) {
  node->ty = cast_ty;
  switch (node->kind) {
    case ND_NUM:
    case ND_VAR:
      return node;
    case ND_CAST: {
      /* If lhs is a primitive, don't need to cast,
       * just return it as the other kind */
      if (node->lhs->kind == ND_NUM)
        return node->lhs;
      return node;
    }
    default:
      error_tok(node->tok, "expression is not constant");
  }
  unreachable();
}

static void emitd_array_comptime_elem(Node *expr) {
  /* Get index & symbol */
  Obj *var;
  uint64_t idx = eval2(expr->lhs, &var);

  /* Get initializer */
  Initializer *vinit = var->init;

  if (!vinit || vinit->ty->kind != TY_ARRAY)
    error_tok(expr->tok,
      "invalid initializer for compile time dereferencing");

  if (idx >= vinit->ty->array_len)
    error_tok(expr->tok,
      "index out of bounds for compile time dereferencing");

  /* Emit initializer */
  emitd_initializer(vinit->children[idx]);
  return;
}

static void emitd_initializer(Initializer *init) {
  if (!init || init->is_flexible) {
    emitf("zeroinitializer");
    return;
  }

  // Rare case like `char l = "hello"[3];`
  // We cannot dereference the expression,
  // so make a copy of the element
  if (init->ty->kind != TY_PTR && init->expr && init->expr->kind == ND_DEREF) {
    emitd_array_comptime_elem(init->expr);
    return;
  }

  switch (init->ty->kind) {
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
      return;
    }
    /* Regular arrays */
    emitc('[');
    for (int i = 0; i < init->ty->array_len; i++) {
      if (i > 0)
        emitf(", ");
      Initializer *child = init->children[i];
      emitf("%s ", llvm_type(child->ty));
      emitd_initializer(child);
    }
    emitc(']');
  } break;

  case TY_STRUCT: {
    emitc('{');
    bool first = true;
    for (Member *mem = init->ty->members; mem; ) {
      if (!first)
        emitf(", ");
      first = false;

      /* Skip bitfields */
      if (mem->is_bitfield) {
        size_t nbytes = init->ty->size;
        int8_t bits[nbytes];
        memset(bits, 0, nbytes);

        while (mem && mem->is_bitfield) {
          if (!mem->bit_width) {
            mem = mem->next;
            continue;
          }

          Initializer *child = init->children[mem->idx];

          /* Evaluate initializer to an unsigned 64-bit value and mask to width.
           * Casting to uint64_t preserves two's-complement low bits for negatives. */
          uint64_t raw = (uint64_t)eval2(child->expr, NULL);
          uint64_t mask = (mem->bit_width >= 64) ? ~0ULL : ((1ULL << mem->bit_width) - 1);
          uint64_t val = raw & mask;

          /* Absolute start bit from beginning of struct: byte-offset * 8 + bit_offset */
          size_t start_bit = (size_t)mem->offset * 8 + (size_t)mem->bit_offset;

          for (size_t i = 0; i < (size_t)mem->bit_width; ++i) {
            size_t bit_index = start_bit + i;
            size_t byte_index = bit_index / 8;
            size_t bit_in_byte = bit_index % 8;
            bits[byte_index] |= (unsigned char)(((val >> i) & 1ULL) << bit_in_byte);
          }

          mem = mem->next;
        }

        /* Emit the packed bytes as i8 constants */
        for (size_t i = 0; i < nbytes; ++i)
          emitf("%si8 %d", (i != 0) ? ", " : "", bits[i]);

        /* continue with next (non-bitfield) member */
        continue;
      }

      /* Emit normal struct member */
      Initializer *child = init->children[mem->idx];
      emitf("%s ", llvm_type(child->ty));
      emitd_initializer(child);
      mem = mem->next;
    }
    emitc('}');
  } break;

  case TY_UNION: {
    if (init->mem)
      emitd_initializer(init->children[0]);
    else
      emitf("zeroinitializer");
  } break;

  default:
    switch (init->expr->kind)
    {
    case ND_NUM: {
      if (is_flonum(init->expr->ty)) {
        emitf("%s", get_float_lit(init->expr->ty->kind, init->expr->fval));
        return;
      }
      int64_t val = eval2(init->expr, NULL);
      emitf("%ld", val);
    } break;

    case ND_CAST:
    case ND_ADDR: {
      Obj *rel = init->expr->lhs->var;
      emitf("bitcast (%s* @%s to %s)",
            llvm_type(init->expr->lhs->ty),
            get_symbol(rel),
            llvm_type(init->ty));
    } break;

    case ND_VAR: {
      bool is_arr = init->expr->ty->kind == TY_ARRAY;
      uint64_t offset = 0;
      const char *llty = llvm_type(init->expr->ty);
      emitf("getelementptr %s(%s, %s* ", is_arr ? "inbounds " : "", llty, llty);
      emitf("@%s", get_symbol(init->expr->var));
      if (is_arr) emitf(", %s 0", llvmty_usize);
      emitf(", %s %ld)", llvmty_usize, offset);
    } break;

    case ND_DEREF: {
      uint64_t offset = 0;

      bool do_bitcast = init->ty->kind != TY_PTR || init->ty->base->kind != TY_CHAR;

      if (do_bitcast)
        emitf("bitcast (i8* ");

      emitf("getelementptr (i8, i8* ");

      if (init->expr->lhs->kind == ND_ADD) {
        Node *nd_add = init->expr->lhs;
        offset = eval2(nd_add->rhs, NULL);

        Type* type_to = pointer_to(ty_char);

        Initializer *ninit = calloc(1, sizeof(Initializer));
        ninit->expr = get_const_expr_di(nd_add->lhs, type_to);
        ninit->ty = ninit->expr->ty;

        emitd_initializer(ninit);
        free(ninit);
        free(type_to);
      }

      emitf(", %s %ld)", llvmty_usize, offset);

      if (do_bitcast)
        emitf(" to %s)", llvm_type(init->ty));
    } break;

    default:
      error_tok(init->tok, "Unsupported yet");
    }
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
    emitd_initializer(var->init);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    emitf(", align %d", align);
    emitln;
  }
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
      gen_store(ret_ty, gen_inum(ret_ty, 0), fn_retval_ll);

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

  llvmty_usize = llvm_type(ty_usize);

  // detect_member_types(prog);
  // emit_member_types();
  // emitln;

  emit_data(prog);
  emitln;

  emit_text(prog);
}
