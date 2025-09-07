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

static count_t instr_id = 1;

static void emit_initializer(Initializer *init);

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

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

char* get_symbol(Obj *var) {
  if (var->symbol)
    return var->symbol;
  return var->name;
}

enum { I1, I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

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
  return U64;
}

static const char *llvm_type_for_size(int bytes, bool is_flo) {
  if (bytes <= 1)      return "i8";
  else if (bytes <= 2) return "i16";
  else if (bytes <= 4) return !is_flo ? "i32" : "float";
  else if (bytes <= 8) return !is_flo ? "i64" : "double";
  else return format("[%d x i8]", bytes);
}

static Type *list_structs = &(Type){0};

#define _push_struct(ty) ({    \
  Type *ty_ = copy_type((ty)); \
  ty_->next = list_structs;    \
  list_structs = ty_;          \
})

static void detect_member_types(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (!var->is_definition)
      continue;

    /* Seek global variables */
    if (var->ty->kind == TY_STRUCT || var->ty->kind == TY_UNION && var->ty->tagname) {
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

static inline bool ty_is_pointer(Type *ty) {
  return ty->kind == TY_PTR || ty->kind == TY_ARRAY;
}

static void emit_gep(char *symbol, bool is_sym_global, Type *lty, Type *rty,
                     bool is_puc_addr, int64_t index) {
  /* Simplify pointers to base */
  while (lty->kind == TY_PTR && lty->base)
    lty = lty->base;
  while (rty->kind == TY_PTR && rty->base)
    rty = rty->base;

  const char *ll_lty = llvm_type(lty);
  const char *ll_rty = llvm_type(rty);

  emitf("getelementptr%s (%s, %s* ",
    is_puc_addr ? " inbounds" : "",
    ll_lty, ll_lty);

  bool do_bitcast = (!ty_is_pointer(lty) && ty_is_pointer(rty) && rty->base->kind != lty->kind) ||
                    (!ty_is_pointer(rty) && ty_is_pointer(lty) && lty->base->kind != rty->kind);

  /* Bitcast if necesary */
  if (do_bitcast)
    emitf("bitcast (%s* ", ll_rty);

  emitf("%c%s", is_sym_global ? '@' : '%', symbol);

  if (do_bitcast)
    emitf(" to %s*)", ll_lty);

  if (is_puc_addr)
    emitf(", i32 0");
  emitf(", i32 %ld", index);

  emitc(')');
}

static void emit_deref(Node *expr, Type *type_to) {
  assert(expr->lhs);
  Obj *var;
  uint64_t val = eval2(expr->lhs, &var);

  bool do_bitcast = type_to->kind != TY_PTR || type_to->base->kind != TY_CHAR;

  if (do_bitcast)
    emitf("bitcast (i8* ");

  emit_gep(get_symbol(var), true, ty_char, var->ty, var->is_puc_addr, val);

  if (do_bitcast)
    emitf(" to %s)",
          llvm_type(type_to));
}

static void emit_array_comptime_elem(Node *expr) {
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
  emit_initializer(vinit->children[idx]);
  return;
}

static void emit_initializer(Initializer *init) {
  if (!init) {
    /* Zero initializer (BSS) */
    emitf("zeroinitializer");
    return;
  }

  // Rare case like `char l = "hello"[3];`
  // We cannot dereference the expression,
  // so make a copy of the element
  if (init->ty->kind != TY_PTR && init->expr && init->expr->kind == ND_DEREF) {
    emit_array_comptime_elem(init->expr);
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
    } return;

    case TY_FLOAT:
    case TY_DOUBLE:
    case TY_LDOUBLE: {
      emit_float_lit(init->ty->kind, init->expr->fval);
    } return;

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
    } return;

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
    } return;

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
    } return;

    case TY_PTR: {
      switch (init->expr->kind) {
        case ND_ADDR: {
          /* Resolve pointer */
          Obj *rel = init->expr->lhs->var;
          emitf("bitcast (%s @%s to %s)",
                llvm_type(init->expr->ty),
                get_symbol(rel),
                llvm_type(init->ty));
        } return;

        case ND_VAR: {
          char *symbol = get_symbol(init->expr->var);
          emit_gep(symbol, true, init->expr->ty, init->expr->ty, init->expr->var->is_puc_addr, 0);
        } return;

        case ND_DEREF:
          emit_deref(init->expr, init->ty);
          return;
      }
    } break;
  }

  unreachable();
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

static void emit_stmt(Node *stmt) {

}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    current_fn = fn;
    const char *symbol = get_symbol(fn);

    current_defer_fn = NO_DEFER; // Reset defers
    instr_id = 1; // Reset instruction id

    Type *ret_ty = fn->ty->return_ty;
    const char *ll_ret_ty = llvm_type(ret_ty);

    emitf("define");
    if (fn->is_static)
      emitf(" internal");
    else
      emitf(" dso_local");

    count_t attr_num = 0;
    /* TODO: emit attributes */

    emitfln(" %s @%s() #%d {", ll_ret_ty, symbol, attr_num);

    // Prologue
    /* Allocate local variables */
    for (Obj *local = fn->locals; local; local = local->next) {
      count_t vid = instr_id++;
      const char *llty = llvm_type(local->ty);
      local->instr_id = vid;
      emitfln("  %%%d = alloca %s, align %d", vid, llty, local->align);
    }
    /* Initialize local variables */
    for (Obj *local = fn->locals; local; local = local->next) {
    }

    // Emit code
    emit_stmt(fn->body);

    // Epilogue
    emitf("  ret %s", ll_ret_ty);
    if (ret_ty->kind != TY_VOID)
      emitf(" 0");
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

  detect_member_types(prog);
  emit_member_types();
  emitln;

  emit_data(prog);
  emitln;

  emit_text(prog);
}
