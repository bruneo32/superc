#include "superc.h"

Type *ty_void = &(Type){TY_VOID, 1, 1};
Type *ty_bool = &(Type){TY_BOOL, 1, 1};

Type *ty_char = &(Type){TY_CHAR, 1, 1};
Type *ty_short = &(Type){TY_SHORT, 2, 2};
Type *ty_int = &(Type){TY_INT, 4, 4};
Type *ty_long = &(Type){TY_LONG, 8, 8};

Type *ty_uchar = &(Type){TY_CHAR, 1, 1, true};
Type *ty_ushort = &(Type){TY_SHORT, 2, 2, true};
Type *ty_uint = &(Type){TY_INT, 4, 4, true};
Type *ty_ulong = &(Type){TY_LONG, 8, 8, true};

Type *ty_float = &(Type){TY_FLOAT, 4, 4};
Type *ty_double = &(Type){TY_DOUBLE, 8, 8};
Type *ty_ldouble = &(Type){TY_LDOUBLE, 16, 16};

static Type *new_type(TypeKind kind, int size, int align) {
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = kind;
  ty->size = size;
  ty->align = align;
  return ty;
}

bool is_integer(Type *ty) {
  TypeKind k = ty->kind;
  return k == TY_BOOL || k == TY_CHAR || k == TY_SHORT ||
         k == TY_INT  || k == TY_LONG || k == TY_ENUM;
}

bool is_flonum(Type *ty) {
  return ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE ||
         ty->kind == TY_LDOUBLE;
}

bool is_numeric(Type *ty) {
  return is_integer(ty) || is_flonum(ty);
}

bool is_compatible(Type *t1, Type *t2) {
  if (t1 == t2)
    return true;

  if (t1->origin)
    return is_compatible(t1->origin, t2);

  if (t2->origin)
    return is_compatible(t1, t2->origin);

  if (t1->kind != t2->kind)
    return false;

  switch (t1->kind) {
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
    return t1->is_unsigned == t2->is_unsigned;
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return true;
  case TY_PTR:
    return is_compatible(t1->base, t2->base);
  case TY_FUNC: {
    if (!is_compatible(t1->return_ty, t2->return_ty))
      return false;
    if (t1->is_variadic != t2->is_variadic)
      return false;

    Type *p1 = t1->params;
    Type *p2 = t2->params;
    for (; p1 && p2; p1 = p1->next, p2 = p2->next)
      if (!is_compatible(p1, p2))
        return false;
    return p1 == NULL && p2 == NULL;
  }
  case TY_ARRAY:
    if (!is_compatible(t1->base, t2->base))
      return false;
    return t1->array_len < 0 && t2->array_len < 0 &&
           t1->array_len == t2->array_len;
  }
  return false;
}

Type *copy_type(Type *ty) {
  Type *ret = calloc(1, sizeof(Type));
  *ret = *ty;
  ret->origin = ty;
  return ret;
}

Type *pointer_to(Type *base) {
  Type *ty = new_type(TY_PTR, 8, 8);
  ty->base = base;
  ty->is_unsigned = true;
  return ty;
}

Type *func_type(Type *return_ty) {
  // The C spec disallows sizeof(<function type>), but
  // GCC allows that and the expression is evaluated to 1.
  Type *ty = new_type(TY_FUNC, 1, 1);
  ty->return_ty = return_ty;
  return ty;
}

Type *array_of(Type *base, int len) {
  Type *ty = new_type(TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->array_len = len;
  return ty;
}

Type *stringlit_of(char *str) {
  size_t len = strlen(str) + 1;
  Type *ty = new_type(TY_ARRAY, ty_char->size * len, ty_char->align);
  ty->base = ty_char;
  ty->array_len = len;
  return ty;
}

Type *vla_of(Type *base, Node *len) {
  Type *ty = new_type(TY_VLA, 8, 8);
  ty->base = base;
  ty->vla_len = len;
  return ty;
}

Type *enum_type(void) {
  return new_type(TY_ENUM, 4, 4);
}

Type *struct_type(void) {
  return new_type(TY_STRUCT, 0, 1);
}

static Type *get_common_type(Type *ty1, Type *ty2) {
  if (ty1->base)
    return pointer_to(ty1->base);

  if (ty1->kind == TY_FUNC)
    return pointer_to(ty1);
  if (ty2->kind == TY_FUNC)
    return pointer_to(ty2);

  if (ty1->kind == TY_LDOUBLE || ty2->kind == TY_LDOUBLE)
    return ty_ldouble;
  if (ty1->kind == TY_DOUBLE || ty2->kind == TY_DOUBLE)
    return ty_double;
  if (ty1->kind == TY_FLOAT || ty2->kind == TY_FLOAT)
    return ty_float;

  if (ty1->size < 4)
    ty1 = ty_int;
  if (ty2->size < 4)
    ty2 = ty_int;

  if (ty1->size != ty2->size)
    return (ty1->size < ty2->size) ? ty2 : ty1;

  if (ty2->is_unsigned)
    return ty2;
  return ty1;
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
static void usual_arith_conv(Node **lhs, Node **rhs) {
  Type *ty = get_common_type((*lhs)->ty, (*rhs)->ty);
  *lhs = new_cast(*lhs, ty);
  *rhs = new_cast(*rhs, ty);
}

void add_type(Node *node) {
  if (!node || node->ty)
    return;

  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->cond);
  add_type(node->then);
  add_type(node->els);
  add_type(node->init);
  add_type(node->inc);

  for (Node *n = node->body; n; n = n->next)
    add_type(n);
  for (Node *n = node->args; n; n = n->next)
    add_type(n);

  switch (node->kind) {
  case ND_NUM:
    node->ty = ty_int;
    return;
  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
  case ND_DIV:
  case ND_MOD:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    usual_arith_conv(&node->lhs, &node->rhs);
    node->ty = node->lhs->ty;
    return;
  case ND_NEG: {
    Type *ty = get_common_type(ty_int, node->lhs->ty);
    node->lhs = new_cast(node->lhs, ty);
    node->ty = ty;
    return;
  }
  case ND_ASSIGN:
    if (node->lhs->ty->kind == TY_ARRAY)
      error_tok(node->lhs->tok, "not an lvalue");
    if (node->lhs->ty->kind != TY_STRUCT)
      node->rhs = new_cast(node->rhs, node->lhs->ty);
    node->ty = node->lhs->ty;
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    usual_arith_conv(&node->lhs, &node->rhs);
    node->ty = ty_int;
    return;
  case ND_FUNCALL:
    node->ty = node->func_ty->return_ty;
    return;
  case ND_NOT:
  case ND_LOGOR:
  case ND_LOGAND:
    node->ty = ty_int;
    return;
  case ND_BITNOT:
  case ND_SHL:
  case ND_SHR:
    node->ty = node->lhs->ty;
    return;
  case ND_VAR:
  case ND_VLA_PTR:
    node->ty = node->var->ty;
    return;
  case ND_COND:
    if (node->then->ty->kind == TY_VOID || node->els->ty->kind == TY_VOID) {
      node->ty = ty_void;
    } else {
      usual_arith_conv(&node->then, &node->els);
      node->ty = node->then->ty;
    }
    return;
  case ND_COMMA:
    node->ty = node->rhs->ty;
    return;
  case ND_MEMBER:
    node->ty = node->member->ty;
    return;
  case ND_ADDR: {
    Type *ty = node->lhs->ty;
    if (ty->kind == TY_ARRAY)
      node->ty = pointer_to(ty->base);
    else
      node->ty = pointer_to(ty);
    return;
  }
  case ND_DEREF:
    if (!node->lhs->ty->base)
      error_tok(node->tok, "invalid pointer dereference");
    if (node->lhs->ty->base->kind == TY_VOID)
      error_tok(node->tok, "dereferencing a void pointer");

    node->ty = node->lhs->ty->base;
    return;
  case ND_STMT_EXPR:
    if (node->body) {
      Node *stmt = node->body;
      while (stmt->next)
        stmt = stmt->next;
      if (stmt->kind == ND_EXPR_STMT) {
        node->ty = stmt->lhs->ty;
        return;
      }
    }
    error_tok(node->tok, "statement expression returning void is not supported");
    return;
  case ND_LABEL_VAL:
    node->ty = pointer_to(ty_void);
    return;
  case ND_CAS:
    add_type(node->cas_addr);
    add_type(node->cas_old);
    add_type(node->cas_new);
    node->ty = ty_bool;

    if (node->cas_addr->ty->kind != TY_PTR)
      error_tok(node->cas_addr->tok, "pointer expected");
    if (node->cas_old->ty->kind != TY_PTR)
      error_tok(node->cas_old->tok, "pointer expected");
    return;
  case ND_EXCH:
    if (node->lhs->ty->kind != TY_PTR)
      error_tok(node->cas_addr->tok, "pointer expected");
    node->ty = node->lhs->ty->base;
    return;
  }
}

/** Check if two types are C compatible. */
bool same_type(Type *a, Type *b) {
  if (a == b)
    return true;
  if (a->kind != b->kind)
    return false;

  switch (a->kind) {
  case TY_VOID:
    return true;
  case TY_BOOL: case TY_CHAR: case TY_SHORT: case TY_INT: case TY_LONG:
  case TY_FLOAT: case TY_DOUBLE:
    // For primitive scalars: check size + signedness
    return a->size == b->size && a->is_unsigned == b->is_unsigned;

  case TY_PTR:
    return same_type(a->base, b->base);

  case TY_ARRAY:
    return a->array_len == b->array_len && same_type(a->base, b->base);

  case TY_FUNC:
    if (!same_type(a->return_ty, b->return_ty))
      return false;
    // Compare parameter lists
    Type *pa = a->params, *pb = b->params;
    for (; pa && pb; pa=pa->next, pb=pb->next)
      if (!same_type(pa, pb))
        return false;
    return pa == NULL && pb == NULL && a->is_variadic == b->is_variadic;

  case TY_STRUCT: case TY_UNION:
    // easiest: require them to be the *same tag* (same declaration symbol)
    return a->name && b->name && !strncmp(a->name->loc, b->name->loc, a->name->len);

  default:
    return false;
  }
}

char *type_to_string(Type *ty) {
  char buf[512];
  char *s = buf;
  #define BUFSIZE (sizeof(buf) - (s - buf))

  if (ty->is_unsigned)
    s += snprintf(s, BUFSIZE, "unsigned ");

  switch (ty->kind) {
  case TY_VOID:
    s += snprintf(s, BUFSIZE, "void");
    break;
  case TY_BOOL:
    s += snprintf(s, BUFSIZE, "bool");
    break;
  case TY_CHAR:
    s += snprintf(s, BUFSIZE, "char");
    break;
  case TY_SHORT:
    s += snprintf(s, BUFSIZE, "short");
    break;
  case TY_INT:
    s += snprintf(s, BUFSIZE, "int");
    break;
  case TY_LONG:
    s += snprintf(s, BUFSIZE, "long");
    break;
  case TY_FLOAT:
    s += snprintf(s, BUFSIZE, "float");
    break;
  case TY_DOUBLE:
    s += snprintf(s, BUFSIZE, "double");
    break;
  case TY_LDOUBLE:
    s += snprintf(s, BUFSIZE, "long double");
    break;

  case TY_PTR:
  case TY_VLA: {
    char *base = type_to_string(ty->base);
    s += snprintf(s, BUFSIZE, "%s*", base);
    free(base);
  } break;

  case TY_ARRAY: {
    char *base = type_to_string(ty->base);
    if (ty->array_len == -1)
      s += snprintf(s, BUFSIZE, "%s[]", base);
    else
      s += snprintf(s, BUFSIZE, "%s[%d]", base, ty->array_len);
    free(base);
  } break;

  case TY_ENUM: {
    s += snprintf(s, BUFSIZE, "enum");
    if (ty->tagname) {
      char name[ty->tagname->len + 1];
      strncpy(name, ty->tagname->loc, ty->tagname->len);
      name[ty->tagname->len] = '\0';
      s += snprintf(s, BUFSIZE, " %s", name);
    }
  } break;

  case TY_STRUCT: {
    s += snprintf(s, BUFSIZE, "struct");
    if (ty->tagname) {
      char name[ty->tagname->len + 1];
      strncpy(name, ty->tagname->loc, ty->tagname->len);
      name[ty->tagname->len] = '\0';
      s += snprintf(s, BUFSIZE, " %s", name);
    }
  } break;

  case TY_UNION: {
    s += snprintf(s, BUFSIZE, "union");
    if (ty->tagname) {
      char name[ty->tagname->len + 1];
      strncpy(name, ty->tagname->loc, ty->tagname->len);
      name[ty->tagname->len] = '\0';
      s += snprintf(s, BUFSIZE, " %s", name);
    }
  } break;

  case TY_FUNC: {
    char *ret = type_to_string(ty->return_ty);
    s += snprintf(s, BUFSIZE, "%s(", ret);
    free(ret);
    for (Type *p = ty->params; p; p = p->next) {
      if (p != ty->params) s += snprintf(s, BUFSIZE, ", ");
      char *pt = type_to_string(p);
      s += snprintf(s, BUFSIZE, "%s", pt);
      free(pt);
    }
    if (ty->is_variadic) s += snprintf(s, BUFSIZE, "%s...", ty->params ? ", " : "");
    s += snprintf(s, BUFSIZE, ")");
  } break;

  default:
    s += snprintf(s, BUFSIZE, "unknown");
  }

  #undef BUFSIZE

  return strdup(buf);
}

//
// assembly symbol mangle
//

static void append_name_mangle(Token *tok, StringBuilder *sb) {
  char *name = strndup(tok->loc, tok->len);
  sb_appendf(sb, "%d%s", tok->len, name);
  free(name);
}

static void append_type_mangle(Type *ty, StringBuilder *sb) {
  switch (ty->kind) {
  case TY_VOID:
    sb_append(sb, "v");
    break;
  case TY_BOOL:
    sb_append(sb, "b");
    break;
  case TY_CHAR:
    sb_appendf(sb, "%c", !ty->is_unsigned ? 'c' : 'h');
    break;
  case TY_SHORT:
    sb_appendf(sb, "%c", 's' + ty->is_unsigned);
    break;
  case TY_INT:
    sb_appendf(sb, "%c", 'i' + ty->is_unsigned);
    break;
  case TY_LONG:
    sb_appendf(sb, "%c", 'l' + ty->is_unsigned);
    break;
  case TY_FLOAT:
    sb_append(sb, "f");
    break;
  case TY_DOUBLE:
    sb_append(sb, "d");
    break;
  case TY_LDOUBLE:
    sb_append(sb, "e");
    break;

  case TY_PTR:
  case TY_VLA:
    sb_append(sb, "P");
    append_type_mangle(ty->base, sb);
    break;

  case TY_ARRAY:
    sb_appendf(sb, "A%d", ty->array_len);
    append_type_mangle(ty->base, sb);
    break;

  case TY_ENUM:
    sb_append(sb, "E");
    if (!ty->tagname)
      error_tok(ty->tagname, "Unnamed enums are not supported");
    append_name_mangle(ty->tagname, sb);
    break;

  case TY_UNION:
    sb_append(sb, "U");
    if (!ty->tagname)
      error_tok(ty->tagname, "Unnamed unions are not supported");
    append_name_mangle(ty->tagname, sb);
    break;

  case TY_STRUCT:
    sb_append(sb, "S");
    if (!ty->tagname)
      error_tok(ty->tagname, "Unnamed structs are not supported");
    append_name_mangle(ty->tagname, sb);
    break;

  case TY_FUNC:
    sb_append(sb, "F");
    if (ty->tagname)
      // error_tok(ty->tagname, "Unnamed functions are not supported");
      append_name_mangle(ty->tagname, sb);
    else {
      for (Type *p = ty->params; p; p = p->next)
        append_type_mangle(p, sb);
      if (ty->is_variadic) sb_append(sb, "Q");
      append_type_mangle(ty->return_ty, sb);
    }
    break;

  default:
    unreachable();
  }
}

/* return a string that can be used as an safe C identifier */
char *type_to_asmident(Type *ty) {
  StringBuilder sb;
  sb_init(&sb);
  append_type_mangle(ty, &sb);
  char *res = strndup(sb.buf, sb.len);
  sb_free(&sb);
  return res;
}

const char *llvm_type(Type *ty) {
  switch (ty->kind) {
  case TY_BOOL:
    return "i1";
  case TY_VOID:
  case TY_CHAR:    return "i8";
  case TY_SHORT:   return "i16";
  case TY_INT:     return "i32";
  case TY_LONG:    return "i64";
  case TY_FLOAT:   return "float";
  case TY_DOUBLE:  return "double";
  case TY_LDOUBLE: return "x86_fp80";
  // Arrays become [n x element_ty]
  case TY_ARRAY:
    return format("[%d x %s]", ty->array_len, llvm_type(ty->base));
  case TY_PTR:
    return format("%s*", llvm_type(ty->base));
  case TY_STRUCT: {
    char *tagname = strndup(ty->tagname->loc, ty->tagname->len);
    char *res = format("%%struct.%s", tagname);
    free(tagname);
    return res;
  }
  case TY_UNION: {
    char *tagname = strndup(ty->tagname->loc, ty->tagname->len);
    char *res = format("%%union.%s", tagname);
    free(tagname);
    return res;
  }
  default:
    unreachable();
  }
}
