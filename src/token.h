#ifndef LUX_TOKEN_H
#define LUX_TOKEN_H

#include "error.h"

typedef enum {
    /* Literals */
    TOK_INT,            /* integer literal */
    TOK_FLOAT,          /* float literal */
    TOK_STRING,         /* simple string literal (no interpolation) */
    TOK_STRING_BEGIN,   /* start of interpolated string */
    TOK_STRING_PART,    /* middle segment of interpolated string */
    TOK_STRING_END,     /* end segment of interpolated string */
    TOK_CHAR,           /* character literal */
    TOK_IDENT,          /* lowercase identifier */
    TOK_UPPER_IDENT,    /* uppercase identifier (type names) */

    /* Keywords */
    TOK_AND,
    TOK_AS,
    TOK_ASSERT,
    TOK_AWAIT,
    TOK_BREAK,
    TOK_COMPTIME,
    TOK_CONST,
    TOK_CONTINUE,
    TOK_DEFER,
    TOK_DO,
    TOK_ELIF,
    TOK_ELSE,
    TOK_EXTERN,
    TOK_FALSE,
    TOK_FN,
    TOK_FOR,
    TOK_IF,
    TOK_IMPORT,
    TOK_IN,
    TOK_IS,
    TOK_LET,
    TOK_LOOP,
    TOK_MATCH,
    TOK_MUT,
    TOK_NONE,
    TOK_NOT,
    TOK_OR,
    TOK_RETURN,
    TOK_SCOPE,
    TOK_SELF_LOWER,     /* self */
    TOK_SELF_UPPER,     /* Self */
    TOK_SPAWN,
    TOK_THEN,
    TOK_TRAIT,
    TOK_TRUE,
    TOK_TYPE,
    TOK_VAR,
    TOK_WHILE,
    TOK_WITH,

    /* Operators & Punctuation */
    TOK_PLUS,           /* + */
    TOK_MINUS,          /* - */
    TOK_STAR,           /* * */
    TOK_SLASH,          /* / */
    TOK_PERCENT,        /* % */
    TOK_AMP,            /* & */
    TOK_PIPE,           /* | */
    TOK_CARET,          /* ^ */
    TOK_TILDE,          /* ~ */
    TOK_BANG,           /* ! */
    TOK_QUESTION,       /* ? */
    TOK_AT,             /* @ */
    TOK_DOT,            /* . */
    TOK_DOTDOT,         /* .. */
    TOK_DOTDOTEQ,       /* ..= */
    TOK_COMMA,          /* , */
    TOK_COLON,          /* : */
    TOK_HASH,           /* # */
    TOK_ARROW,          /* -> */
    TOK_PIPE_ARROW,     /* |> */
    TOK_TICK,           /* ' */
    TOK_UNDERSCORE,     /* _ */

    /* Comparison */
    TOK_EQ,             /* == */
    TOK_NEQ,            /* != */
    TOK_LT,             /* < */
    TOK_GT,             /* > */
    TOK_LE,             /* <= */
    TOK_GE,             /* >= */

    /* Shift */
    TOK_SHL,            /* << */
    TOK_SHR,            /* >> */

    /* Assignment */
    TOK_ASSIGN,         /* = */
    TOK_PLUS_ASSIGN,    /* += */
    TOK_MINUS_ASSIGN,   /* -= */
    TOK_STAR_ASSIGN,    /* *= */
    TOK_SLASH_ASSIGN,   /* /= */
    TOK_PERCENT_ASSIGN, /* %= */
    TOK_AMP_ASSIGN,     /* &= */
    TOK_PIPE_ASSIGN,    /* |= */
    TOK_CARET_ASSIGN,   /* ^= */
    TOK_AND_ASSIGN,     /* &&= */
    TOK_OR_ASSIGN,      /* ||= */
    TOK_SHL_ASSIGN,     /* <<= */
    TOK_SHR_ASSIGN,     /* >>= */

    /* Delimiters */
    TOK_LPAREN,         /* ( */
    TOK_RPAREN,         /* ) */
    TOK_LBRACKET,       /* [ */
    TOK_RBRACKET,       /* ] */
    TOK_LBRACE,         /* { */
    TOK_RBRACE,         /* } */

    /* Whitespace-significant */
    TOK_NEWLINE,
    TOK_INDENT,
    TOK_DEDENT,

    /* Special */
    TOK_ELLIPSIS,       /* ... */
    TOK_EOF,
    TOK_ERROR,

    TOK_COUNT
} TokenKind;

typedef struct {
    TokenKind kind;
    SrcLoc loc;
    const char *start;  /* pointer into source */
    int len;            /* length of token text */
    /* For string/int/float, value stored as text; parsed later if needed */
} Token;

const char *token_kind_str(TokenKind kind);

#endif
