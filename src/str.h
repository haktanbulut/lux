#ifndef LUX_STR_H
#define LUX_STR_H

#include <stddef.h>

typedef struct {
    const char **entries;   /* array of interned strings */
    size_t count;
    size_t cap;
} InternTable;

void intern_init(InternTable *t);
void intern_free(InternTable *t);
const char *str_intern_range(InternTable *t, const char *start, size_t len);
const char *str_intern(InternTable *t, const char *s);

#endif
