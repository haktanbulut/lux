#ifndef LUX_VEC_H
#define LUX_VEC_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/*
 * Stretchy buffer / dynamic array via macros.
 * Usage:
 *   int *arr = NULL;
 *   vec_push(arr, 42);
 *   for (int i = 0; i < vec_len(arr); i++) printf("%d\n", arr[i]);
 *   vec_free(arr);
 *
 * Layout in memory: [size_t len][size_t cap][T data...]
 * The pointer the user holds points to data[].
 */

typedef struct {
    size_t len;
    size_t cap;
} VecHeader;

#define vec__header(v) ((VecHeader *)((char *)(v) - sizeof(VecHeader)))

#define vec_len(v)  ((v) ? vec__header(v)->len : 0)
#define vec_cap(v)  ((v) ? vec__header(v)->cap : 0)
#define vec_last(v) ((v)[vec_len(v) - 1])

#define vec_free(v) do { if (v) { free(vec__header(v)); (v) = NULL; } } while(0)

#define vec__needgrow(v, n) ((v) == NULL || vec__header(v)->len + (n) > vec__header(v)->cap)

#define vec__grow(v, n) do { \
    size_t _newcap = (v) ? vec__header(v)->cap * 2 : 8; \
    size_t _needed = vec_len(v) + (n); \
    if (_newcap < _needed) _newcap = _needed; \
    size_t _bytes = sizeof(VecHeader) + _newcap * sizeof(*(v)); \
    VecHeader *_h; \
    if (v) { \
        _h = (VecHeader *)realloc(vec__header(v), _bytes); \
    } else { \
        _h = (VecHeader *)malloc(_bytes); \
        _h->len = 0; \
    } \
    _h->cap = _newcap; \
    (v) = (void *)((char *)_h + sizeof(VecHeader)); \
} while(0)

#define vec_push(v, item) do { \
    if (vec__needgrow(v, 1)) vec__grow(v, 1); \
    (v)[vec__header(v)->len++] = (item); \
} while(0)

#define vec_pop(v) ((v)[--vec__header(v)->len])
#define vec_pop_discard(v) (--vec__header(v)->len)

#define vec_clear(v) do { if (v) vec__header(v)->len = 0; } while(0)

#endif
