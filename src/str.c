#include "str.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

void intern_init(InternTable *t) {
    t->cap = 256;
    t->count = 0;
    t->entries = (const char **)calloc(t->cap, sizeof(const char *));
}

void intern_free(InternTable *t) {
    for (size_t i = 0; i < t->cap; i++) {
        if (t->entries[i]) free((void *)t->entries[i]);
    }
    free(t->entries);
    t->entries = NULL;
    t->count = 0;
    t->cap = 0;
}

static uint64_t hash_bytes(const char *s, size_t len) {
    uint64_t h = 14695981039346656037ULL;
    for (size_t i = 0; i < len; i++) {
        h ^= (uint8_t)s[i];
        h *= 1099511628211ULL;
    }
    return h;
}

const char *str_intern_range(InternTable *t, const char *start, size_t len) {
    /* Grow if over 70% full */
    if (t->count * 10 >= t->cap * 7) {
        size_t new_cap = t->cap * 2;
        const char **new_entries = (const char **)calloc(new_cap, sizeof(const char *));
        for (size_t i = 0; i < t->cap; i++) {
            if (t->entries[i]) {
                uint64_t h = hash_bytes(t->entries[i], strlen(t->entries[i]));
                size_t idx = h & (new_cap - 1);
                while (new_entries[idx]) idx = (idx + 1) & (new_cap - 1);
                new_entries[idx] = t->entries[i];
            }
        }
        free(t->entries);
        t->entries = new_entries;
        t->cap = new_cap;
    }

    uint64_t h = hash_bytes(start, len);
    size_t idx = h & (t->cap - 1);
    while (t->entries[idx]) {
        if (strlen(t->entries[idx]) == len && memcmp(t->entries[idx], start, len) == 0) {
            return t->entries[idx];
        }
        idx = (idx + 1) & (t->cap - 1);
    }

    char *s = (char *)malloc(len + 1);
    memcpy(s, start, len);
    s[len] = '\0';
    t->entries[idx] = s;
    t->count++;
    return s;
}

const char *str_intern(InternTable *t, const char *s) {
    return str_intern_range(t, s, strlen(s));
}
