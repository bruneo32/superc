#include "superc.h"

void strarray_push(StringArray *arr, char *s) {
  if (!arr->data) {
    arr->data = calloc(8, sizeof(char *));
    arr->capacity = 8;
  }

  if (arr->capacity == arr->len) {
    arr->data = realloc(arr->data, sizeof(char *) * arr->capacity * 2);
    arr->capacity *= 2;
    for (int i = arr->len; i < arr->capacity; i++)
      arr->data[i] = NULL;
  }

  arr->data[arr->len++] = s;
}

// Takes a printf-style format string and returns a formatted string.
char *format(char *fmt, ...) {
  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(out, fmt, ap);
  va_end(ap);
  fclose(out);
  return buf;
}

#define SB_INIT_CAP 128

void sb_init(StringBuilder *sb) {
  sb->cap = SB_INIT_CAP;
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
