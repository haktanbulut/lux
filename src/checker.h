#ifndef LUX_CHECKER_H
#define LUX_CHECKER_H

#include "ast.h"
#include "type.h"
#include "scope.h"
#include "str.h"

typedef struct {
    Arena *arena;
    InternTable *interns;
    SymTab symtab;
    Type *current_ret_type;
    int errors;
    int typevar_counter;

    /* Interned builtin type names for fast comparison */
    const char *int_name;
    const char *float_name;
    const char *bool_name;
    const char *char_name;
    const char *string_name;
    const char *none_name;
} Checker;

void checker_init(Checker *c, Arena *arena, InternTable *interns);
void checker_check(Checker *c, AstNode *program);

#endif
