#ifndef LUX_PARSER_H
#define LUX_PARSER_H

#include "ast.h"
#include "token.h"
#include "str.h"

typedef struct {
    Token *tokens;
    int count;
    int pos;
    Arena *arena;
    InternTable *interns;
    int errors;
} Parser;

void parser_init(Parser *p, Token *tokens, int count, Arena *arena, InternTable *interns);
AstNode *parse_program(Parser *p);

#endif
