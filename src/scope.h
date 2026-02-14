#ifndef LUX_SCOPE_H
#define LUX_SCOPE_H

#include "type.h"
#include "error.h"
#include <stdbool.h>

typedef enum {
    SYM_VAR,
    SYM_FN,
    SYM_TYPE,
    SYM_CONST,
    SYM_EXTERN_FN,
    SYM_PARAM,
} SymbolKind;

typedef struct Symbol {
    const char *name;       /* interned */
    SymbolKind kind;
    Type *type;
    bool is_mutable;
    SrcLoc loc;
    struct Symbol *next;    /* hash chain */
} Symbol;

#define SCOPE_BUCKETS 64

typedef struct Scope {
    Symbol *buckets[SCOPE_BUCKETS];
    struct Scope *parent;
} Scope;

typedef struct {
    Scope *current;
    Arena *arena;
} SymTab;

void symtab_init(SymTab *st, Arena *arena);
void symtab_push_scope(SymTab *st);
void symtab_pop_scope(SymTab *st);

/* Define a symbol in the current scope. Returns NULL if already defined in this scope. */
Symbol *symtab_define(SymTab *st, const char *name, SymbolKind kind,
                      Type *type, bool is_mutable, SrcLoc loc);

/* Lookup a symbol walking up the scope chain. Returns NULL if not found. */
Symbol *symtab_lookup(SymTab *st, const char *name);

/* Lookup only in the current (innermost) scope. */
Symbol *symtab_lookup_local(SymTab *st, const char *name);

#endif
