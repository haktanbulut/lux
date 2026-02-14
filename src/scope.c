#include "scope.h"
#include <string.h>
#include <stdint.h>

/* Hash an interned pointer — since strings are interned, we hash the pointer value */
static unsigned hash_ptr(const char *p) {
    uintptr_t v = (uintptr_t)p;
    v = (v >> 3) * 2654435761U;
    return (unsigned)(v & (SCOPE_BUCKETS - 1));
}

void symtab_init(SymTab *st, Arena *arena) {
    st->arena = arena;
    st->current = NULL;
    symtab_push_scope(st); /* global scope */
}

void symtab_push_scope(SymTab *st) {
    Scope *s = (Scope *)arena_alloc(st->arena, sizeof(Scope));
    memset(s, 0, sizeof(Scope));
    s->parent = st->current;
    st->current = s;
}

void symtab_pop_scope(SymTab *st) {
    if (st->current) {
        st->current = st->current->parent;
    }
}

Symbol *symtab_define(SymTab *st, const char *name, SymbolKind kind,
                      Type *type, bool is_mutable, SrcLoc loc) {
    /* Check for redefinition in current scope */
    unsigned h = hash_ptr(name);
    for (Symbol *s = st->current->buckets[h]; s; s = s->next) {
        if (s->name == name) return NULL; /* already defined */
    }

    Symbol *sym = (Symbol *)arena_alloc(st->arena, sizeof(Symbol));
    sym->name = name;
    sym->kind = kind;
    sym->type = type;
    sym->is_mutable = is_mutable;
    sym->loc = loc;
    sym->next = st->current->buckets[h];
    st->current->buckets[h] = sym;
    return sym;
}

Symbol *symtab_lookup(SymTab *st, const char *name) {
    unsigned h = hash_ptr(name);
    for (Scope *scope = st->current; scope; scope = scope->parent) {
        for (Symbol *s = scope->buckets[h]; s; s = s->next) {
            if (s->name == name) return s;
        }
    }
    return NULL;
}

Symbol *symtab_lookup_local(SymTab *st, const char *name) {
    unsigned h = hash_ptr(name);
    for (Symbol *s = st->current->buckets[h]; s; s = s->next) {
        if (s->name == name) return s;
    }
    return NULL;
}
