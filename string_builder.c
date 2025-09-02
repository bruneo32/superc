#include "superc.h"

#define INIT_CAP 128

void sb_init(StringBuilder *sb) {
  sb->cap = INIT_CAP;
  sb->len = 0;
  sb->buf = malloc(sb->cap);
  sb->buf[0] = '\0';
}

void sb_free(StringBuilder *sb) {
  free(sb->buf);
  sb->buf = NULL;
  sb->len = sb->cap = 0;
}

void sb_grow(StringBuilder *sb, size_t more) {
  size_t need = sb->len + more + 1;
  if (need <= sb->cap) return;
  while (sb->cap < need) sb->cap <<= 1;
  sb->buf = realloc(sb->buf, sb->cap);
}

void sb_append(StringBuilder *sb, const char *s) {
  size_t n = strlen(s);
  sb_grow(sb, n);
  memcpy(sb->buf + sb->len, s, n);
  sb->len += n;
  sb->buf[sb->len] = '\0';
}

void sb_appendf(StringBuilder *sb, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  // format once to find needed size
  va_list ap2;
  va_copy(ap2, ap);
  size_t needed = vsnprintf(NULL, 0, fmt, ap2);
  va_end(ap2);

  if (needed > 0) {
    sb_grow(sb, (size_t)needed);
    vsnprintf(sb->buf + sb->len, sb->cap - sb->len, fmt, ap);
    sb->len += (size_t)needed;
  }

  va_end(ap);
}
