#ifndef LUX_ARENA_H
#define LUX_ARENA_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ARENA_BLOCK_SIZE (1024 * 1024) /* 1 MB */

typedef struct ArenaBlock {
    struct ArenaBlock *next;
    size_t size;
    size_t used;
    char data[];
} ArenaBlock;

typedef struct {
    ArenaBlock *head;
    ArenaBlock *current;
} Arena;

static inline ArenaBlock *arena_block_new(size_t size) {
    ArenaBlock *b = (ArenaBlock *)malloc(sizeof(ArenaBlock) + size);
    b->next = NULL;
    b->size = size;
    b->used = 0;
    return b;
}

static inline void arena_init(Arena *a) {
    a->head = arena_block_new(ARENA_BLOCK_SIZE);
    a->current = a->head;
}

static inline void *arena_alloc(Arena *a, size_t size) {
    /* Align to 8 bytes */
    size = (size + 7) & ~(size_t)7;
    if (a->current->used + size > a->current->size) {
        size_t block_size = size > ARENA_BLOCK_SIZE ? size : ARENA_BLOCK_SIZE;
        ArenaBlock *b = arena_block_new(block_size);
        a->current->next = b;
        a->current = b;
    }
    void *ptr = a->current->data + a->current->used;
    a->current->used += size;
    return ptr;
}

static inline char *arena_strdup(Arena *a, const char *s, size_t len) {
    char *p = (char *)arena_alloc(a, len + 1);
    memcpy(p, s, len);
    p[len] = '\0';
    return p;
}

static inline void arena_destroy(Arena *a) {
    ArenaBlock *b = a->head;
    while (b) {
        ArenaBlock *next = b->next;
        free(b);
        b = next;
    }
    a->head = NULL;
    a->current = NULL;
}

#endif
