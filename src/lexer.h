#ifndef LUX_LEXER_H
#define LUX_LEXER_H

#include "token.h"
#include "str.h"

typedef struct {
    const char *source;
    const char *filename;
    const char *pos;
    int line;
    int col;
    int bracket_depth;  /* () [] {} nesting — suppress indent inside */
    int *indent_stack;  /* vec of indent levels */
    int pending_dedents;
    int at_line_start;
    InternTable *interns;
    /* String interpolation state */
    int *interp_depth_stack; /* vec: bracket depth when entering interpolation */
} Lexer;

void lexer_init(Lexer *lex, const char *source, const char *filename, InternTable *interns);
void lexer_free(Lexer *lex);
Token lexer_next(Lexer *lex);

/* Lex all tokens into a dynamic array */
Token *lexer_lex_all(Lexer *lex);

#endif
