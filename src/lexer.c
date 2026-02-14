#include "lexer.h"
#include "vec.h"
#include <ctype.h>
#include <string.h>
#include <stdio.h>

const char *token_kind_str(TokenKind kind) {
    static const char *names[] = {
        [TOK_INT] = "INT", [TOK_FLOAT] = "FLOAT",
        [TOK_STRING] = "STRING", [TOK_STRING_BEGIN] = "STRING_BEGIN",
        [TOK_STRING_PART] = "STRING_PART", [TOK_STRING_END] = "STRING_END",
        [TOK_CHAR] = "CHAR",
        [TOK_IDENT] = "IDENT", [TOK_UPPER_IDENT] = "UPPER_IDENT",
        [TOK_AND] = "and", [TOK_AS] = "as", [TOK_ASSERT] = "assert",
        [TOK_AWAIT] = "await", [TOK_BREAK] = "break",
        [TOK_COMPTIME] = "comptime", [TOK_CONST] = "const",
        [TOK_CONTINUE] = "continue", [TOK_DEFER] = "defer",
        [TOK_DO] = "do", [TOK_ELIF] = "elif", [TOK_ELSE] = "else",
        [TOK_EXTERN] = "extern", [TOK_FALSE] = "false",
        [TOK_FN] = "fn", [TOK_FOR] = "for", [TOK_IF] = "if",
        [TOK_IMPORT] = "import", [TOK_IN] = "in", [TOK_IS] = "is",
        [TOK_LET] = "let", [TOK_LOOP] = "loop", [TOK_MATCH] = "match",
        [TOK_MUT] = "mut", [TOK_NONE] = "none", [TOK_NOT] = "not",
        [TOK_OR] = "or", [TOK_RETURN] = "return", [TOK_SCOPE] = "scope",
        [TOK_SELF_LOWER] = "self", [TOK_SELF_UPPER] = "Self",
        [TOK_SPAWN] = "spawn", [TOK_THEN] = "then",
        [TOK_TRAIT] = "trait", [TOK_TRUE] = "true", [TOK_TYPE] = "type",
        [TOK_VAR] = "var", [TOK_WHILE] = "while", [TOK_WITH] = "with",
        [TOK_PLUS] = "+", [TOK_MINUS] = "-", [TOK_STAR] = "*",
        [TOK_SLASH] = "/", [TOK_PERCENT] = "%", [TOK_AMP] = "&",
        [TOK_PIPE] = "|", [TOK_CARET] = "^", [TOK_TILDE] = "~",
        [TOK_BANG] = "!", [TOK_QUESTION] = "?", [TOK_AT] = "@",
        [TOK_DOT] = ".", [TOK_DOTDOT] = "..", [TOK_DOTDOTEQ] = "..=",
        [TOK_COMMA] = ",", [TOK_COLON] = ":", [TOK_HASH] = "#",
        [TOK_ARROW] = "->", [TOK_PIPE_ARROW] = "|>", [TOK_TICK] = "'",
        [TOK_UNDERSCORE] = "_",
        [TOK_EQ] = "==", [TOK_NEQ] = "!=",
        [TOK_LT] = "<", [TOK_GT] = ">", [TOK_LE] = "<=", [TOK_GE] = ">=",
        [TOK_SHL] = "<<", [TOK_SHR] = ">>",
        [TOK_ASSIGN] = "=",
        [TOK_PLUS_ASSIGN] = "+=", [TOK_MINUS_ASSIGN] = "-=",
        [TOK_STAR_ASSIGN] = "*=", [TOK_SLASH_ASSIGN] = "/=",
        [TOK_PERCENT_ASSIGN] = "%=", [TOK_AMP_ASSIGN] = "&=",
        [TOK_PIPE_ASSIGN] = "|=", [TOK_CARET_ASSIGN] = "^=",
        [TOK_AND_ASSIGN] = "&&=", [TOK_OR_ASSIGN] = "||=",
        [TOK_SHL_ASSIGN] = "<<=", [TOK_SHR_ASSIGN] = ">>=",
        [TOK_LPAREN] = "(", [TOK_RPAREN] = ")",
        [TOK_LBRACKET] = "[", [TOK_RBRACKET] = "]",
        [TOK_LBRACE] = "{", [TOK_RBRACE] = "}",
        [TOK_NEWLINE] = "NEWLINE", [TOK_INDENT] = "INDENT",
        [TOK_DEDENT] = "DEDENT",
        [TOK_ELLIPSIS] = "...", [TOK_EOF] = "EOF", [TOK_ERROR] = "ERROR",
    };
    if (kind >= 0 && kind < TOK_COUNT && names[kind]) return names[kind];
    return "???";
}

void lexer_init(Lexer *lex, const char *source, const char *filename, InternTable *interns) {
    lex->source = source;
    lex->filename = filename;
    lex->pos = source;
    lex->line = 1;
    lex->col = 1;
    lex->bracket_depth = 0;
    lex->indent_stack = NULL;
    vec_push(lex->indent_stack, 0); /* base indent level */
    lex->pending_dedents = 0;
    lex->at_line_start = 1;
    lex->interns = interns;
    lex->interp_depth_stack = NULL;
}

void lexer_free(Lexer *lex) {
    vec_free(lex->indent_stack);
    vec_free(lex->interp_depth_stack);
}

static char peek(Lexer *lex) { return *lex->pos; }
static char peek2(Lexer *lex) { return lex->pos[0] ? lex->pos[1] : 0; }
static char peek3(Lexer *lex) { return (lex->pos[0] && lex->pos[1]) ? lex->pos[2] : 0; }

static void advance(Lexer *lex) {
    if (*lex->pos == '\n') {
        lex->line++;
        lex->col = 1;
    } else {
        lex->col++;
    }
    lex->pos++;
}

static SrcLoc loc(Lexer *lex) {
    return (SrcLoc){ .file = lex->filename, .line = lex->line, .col = lex->col };
}

static Token make_token(Lexer *lex, TokenKind kind, const char *start, int len, SrcLoc sloc) {
    (void)lex;
    return (Token){ .kind = kind, .loc = sloc, .start = start, .len = len };
}

static void skip_line_comment(Lexer *lex) {
    /* -- comment until end of line */
    while (*lex->pos && *lex->pos != '\n') advance(lex);
}

static void skip_block_comment(Lexer *lex) {
    /* {- ... -} with nesting */
    advance(lex); advance(lex); /* skip {- */
    int depth = 1;
    while (*lex->pos && depth > 0) {
        if (lex->pos[0] == '{' && lex->pos[1] == '-') {
            advance(lex); advance(lex);
            depth++;
        } else if (lex->pos[0] == '-' && lex->pos[1] == '}') {
            advance(lex); advance(lex);
            depth--;
        } else {
            advance(lex);
        }
    }
}

static Token lex_number(Lexer *lex) {
    SrcLoc sloc = loc(lex);
    const char *start = lex->pos;
    int is_float = 0;

    if (lex->pos[0] == '0' && lex->pos[1] == 'x') {
        advance(lex); advance(lex);
        while (isxdigit(peek(lex)) || peek(lex) == '_') advance(lex);
    } else if (lex->pos[0] == '0' && lex->pos[1] == 'o') {
        advance(lex); advance(lex);
        while ((peek(lex) >= '0' && peek(lex) <= '7') || peek(lex) == '_') advance(lex);
    } else if (lex->pos[0] == '0' && lex->pos[1] == 'b') {
        advance(lex); advance(lex);
        while (peek(lex) == '0' || peek(lex) == '1' || peek(lex) == '_') advance(lex);
    } else {
        while (isdigit(peek(lex)) || peek(lex) == '_') advance(lex);
        if (peek(lex) == '.' && isdigit(peek2(lex))) {
            is_float = 1;
            advance(lex); /* skip . */
            while (isdigit(peek(lex)) || peek(lex) == '_') advance(lex);
        }
        if (peek(lex) == 'e' || peek(lex) == 'E') {
            is_float = 1;
            advance(lex);
            if (peek(lex) == '+' || peek(lex) == '-') advance(lex);
            while (isdigit(peek(lex))) advance(lex);
        }
    }

    return make_token(lex, is_float ? TOK_FLOAT : TOK_INT,
                      start, (int)(lex->pos - start), sloc);
}

static Token lex_char(Lexer *lex) {
    SrcLoc sloc = loc(lex);
    const char *start = lex->pos;
    advance(lex); /* skip opening ' */
    if (peek(lex) == '\\') {
        advance(lex); /* skip \ */
        advance(lex); /* skip escape char */
        /* handle \x, \u */
        if (lex->pos[-1] == 'x') {
            advance(lex); advance(lex);
        } else if (lex->pos[-1] == 'u') {
            advance(lex); /* { */
            while (peek(lex) != '}' && peek(lex)) advance(lex);
            advance(lex); /* } */
        }
    } else {
        advance(lex); /* the character itself */
    }
    if (peek(lex) == '\'') advance(lex); /* closing ' */
    return make_token(lex, TOK_CHAR, start, (int)(lex->pos - start), sloc);
}

/* Check if we're inside string interpolation and at closing } */
static int in_string_interp(Lexer *lex) {
    if (vec_len(lex->interp_depth_stack) == 0) return 0;
    return lex->bracket_depth == vec_last(lex->interp_depth_stack);
}

/*
 * Lex a string that may contain interpolation.
 * On entry: lex->pos points at opening " OR we've just consumed the } of an interpolation.
 * For the first call: kind_begin = TOK_STRING or TOK_STRING_BEGIN
 */
static Token lex_string_segment(Lexer *lex, TokenKind kind) {
    SrcLoc sloc = loc(lex);
    const char *start = lex->pos;

    if (kind == TOK_STRING || kind == TOK_STRING_BEGIN) {
        advance(lex); /* skip opening " */
    }

    while (peek(lex) && peek(lex) != '"' && peek(lex) != '\n') {
        if (peek(lex) == '\\') {
            advance(lex);
            advance(lex);
        } else if (peek(lex) == '{') {
            /* Found interpolation — return what we have so far as STRING_BEGIN or STRING_PART */
            TokenKind seg_kind = (kind == TOK_STRING || kind == TOK_STRING_BEGIN)
                                 ? TOK_STRING_BEGIN : TOK_STRING_PART;
            Token tok = make_token(lex, seg_kind, start, (int)(lex->pos - start), sloc);
            advance(lex); /* skip { */
            lex->bracket_depth++;
            vec_push(lex->interp_depth_stack, lex->bracket_depth);
            return tok;
        } else {
            advance(lex);
        }
    }

    /* End of string */
    TokenKind end_kind;
    if (kind == TOK_STRING || kind == TOK_STRING_BEGIN) {
        /* No interpolation was ever found (or this is a simple string) */
        end_kind = (kind == TOK_STRING) ? TOK_STRING : TOK_STRING_END;
        /* Actually: if we started as STRING and never hit {, it's a simple string */
        if (kind == TOK_STRING_BEGIN) end_kind = TOK_STRING_END;
    } else {
        end_kind = TOK_STRING_END;
    }

    if (peek(lex) == '"') advance(lex); /* skip closing " */
    return make_token(lex, end_kind, start, (int)(lex->pos - start), sloc);
}

static TokenKind keyword_kind(const char *s, int len) {
    /* Check against all keywords */
    typedef struct { const char *kw; TokenKind kind; } KW;
    static const KW keywords[] = {
        {"and", TOK_AND}, {"as", TOK_AS}, {"assert", TOK_ASSERT},
        {"await", TOK_AWAIT}, {"break", TOK_BREAK},
        {"comptime", TOK_COMPTIME}, {"const", TOK_CONST},
        {"continue", TOK_CONTINUE}, {"defer", TOK_DEFER}, {"do", TOK_DO},
        {"elif", TOK_ELIF}, {"else", TOK_ELSE}, {"extern", TOK_EXTERN},
        {"false", TOK_FALSE}, {"fn", TOK_FN}, {"for", TOK_FOR},
        {"if", TOK_IF}, {"import", TOK_IMPORT}, {"in", TOK_IN},
        {"is", TOK_IS}, {"let", TOK_LET}, {"loop", TOK_LOOP},
        {"match", TOK_MATCH}, {"mut", TOK_MUT}, {"none", TOK_NONE},
        {"not", TOK_NOT}, {"or", TOK_OR}, {"return", TOK_RETURN},
        {"scope", TOK_SCOPE}, {"self", TOK_SELF_LOWER},
        {"Self", TOK_SELF_UPPER}, {"spawn", TOK_SPAWN},
        {"then", TOK_THEN}, {"trait", TOK_TRAIT}, {"true", TOK_TRUE},
        {"type", TOK_TYPE}, {"var", TOK_VAR}, {"while", TOK_WHILE},
        {"with", TOK_WITH},
        {NULL, TOK_ERROR}
    };
    for (int i = 0; keywords[i].kw; i++) {
        if ((int)strlen(keywords[i].kw) == len && memcmp(keywords[i].kw, s, len) == 0) {
            return keywords[i].kind;
        }
    }
    return TOK_ERROR; /* not a keyword */
}

static Token lex_ident_or_keyword(Lexer *lex) {
    SrcLoc sloc = loc(lex);
    const char *start = lex->pos;
    int is_upper = isupper(peek(lex));

    while (isalnum(peek(lex)) || peek(lex) == '_') advance(lex);
    int len = (int)(lex->pos - start);

    /* Check for _ alone */
    if (len == 1 && start[0] == '_') {
        return make_token(lex, TOK_UNDERSCORE, start, len, sloc);
    }

    /* Check keywords */
    TokenKind kw = keyword_kind(start, len);
    if (kw != TOK_ERROR) {
        return make_token(lex, kw, start, len, sloc);
    }

    /* Intern the identifier */
    str_intern_range(lex->interns, start, len);
    TokenKind kind = is_upper ? TOK_UPPER_IDENT : TOK_IDENT;
    return make_token(lex, kind, start, len, sloc);
}

/* Process indentation at the start of a line.
 * Returns: 0 = no indent change, 1 = emitted INDENT (caller should return it),
 * -N = N pending DEDENT tokens to emit */
static int handle_indent(Lexer *lex) {
    if (lex->bracket_depth > 0) return 0;

    int indent = 0;
    const char *p = lex->pos;
    while (*p == ' ') { indent++; p++; }
    while (*p == '\t') { indent += 4; p++; } /* treat tab as 4 spaces */

    /* skip blank lines and comment-only lines */
    if (*p == '\n' || *p == '\0' || (*p == '-' && *(p+1) == '-')) return 0;

    int current = vec_last(lex->indent_stack);
    if (indent > current) {
        vec_push(lex->indent_stack, indent);
        return 1; /* emit INDENT */
    } else if (indent < current) {
        int dedents = 0;
        while (vec_len(lex->indent_stack) > 1 && vec_last(lex->indent_stack) > indent) {
            (void)vec_pop(lex->indent_stack);
            dedents++;
        }
        return -dedents;
    }
    return 0;
}

Token lexer_next(Lexer *lex) {
    /* Emit pending dedents */
    if (lex->pending_dedents > 0) {
        lex->pending_dedents--;
        return make_token(lex, TOK_DEDENT, lex->pos, 0, loc(lex));
    }

    /* Check if returning from string interpolation */
    if (in_string_interp(lex) && peek(lex) == '}') {
        (void)vec_pop(lex->interp_depth_stack);
        lex->bracket_depth--;
        advance(lex); /* skip } */
        /* Continue lexing the rest of the string */
        return lex_string_segment(lex, TOK_STRING_PART);
    }

    /* Handle indentation at line start */
    if (lex->at_line_start) {
        lex->at_line_start = 0;
        int result = handle_indent(lex);
        if (result == 1) {
            return make_token(lex, TOK_INDENT, lex->pos, 0, loc(lex));
        } else if (result < 0) {
            lex->pending_dedents = (-result) - 1;
            return make_token(lex, TOK_DEDENT, lex->pos, 0, loc(lex));
        }
    }

    /* Skip whitespace (not newlines) */
    while (peek(lex) == ' ' || peek(lex) == '\t' || peek(lex) == '\r') advance(lex);

    /* Skip comments */
    if (peek(lex) == '-' && peek2(lex) == '-') {
        skip_line_comment(lex);
        /* Fall through to handle newline below */
    }
    if (peek(lex) == '{' && peek2(lex) == '-') {
        skip_block_comment(lex);
        /* Skip whitespace after block comment */
        while (peek(lex) == ' ' || peek(lex) == '\t' || peek(lex) == '\r') advance(lex);
    }

    SrcLoc sloc = loc(lex);
    const char *start = lex->pos;

    if (peek(lex) == '\0') {
        /* Emit remaining dedents at EOF */
        if (vec_len(lex->indent_stack) > 1) {
            (void)vec_pop(lex->indent_stack);
            lex->pending_dedents = (int)vec_len(lex->indent_stack) - 1;
            return make_token(lex, TOK_DEDENT, start, 0, sloc);
        }
        return make_token(lex, TOK_EOF, start, 0, sloc);
    }

    /* Newline */
    if (peek(lex) == '\n') {
        advance(lex);
        lex->at_line_start = 1;
        if (lex->bracket_depth > 0) {
            /* Inside brackets, skip newlines (implicit line continuation) */
            return lexer_next(lex);
        }
        return make_token(lex, TOK_NEWLINE, start, 1, sloc);
    }

    /* Numbers */
    if (isdigit(peek(lex))) return lex_number(lex);

    /* Identifiers and keywords */
    if (isalpha(peek(lex)) || peek(lex) == '_') return lex_ident_or_keyword(lex);

    /* Character literal */
    if (peek(lex) == '\'' && peek2(lex) != '\0') {
        /* Check if this is a label tick or a char literal */
        /* Label: 'ident (tick followed by lowercase letter) */
        if (isalpha(peek2(lex)) || peek2(lex) == '_') {
            /* Could be char literal 'a' or label 'name */
            /* Look ahead: if there's a closing ', it's a char literal */
            const char *p = lex->pos + 1;
            if (*p == '\\') {
                return lex_char(lex);
            }
            p++; /* skip the char */
            if (*p == '\'') {
                return lex_char(lex); /* char literal like 'a' */
            }
            /* It's a label tick */
            advance(lex);
            return make_token(lex, TOK_TICK, start, 1, sloc);
        }
        return lex_char(lex);
    }

    /* String literal */
    if (peek(lex) == '"') {
        return lex_string_segment(lex, TOK_STRING);
    }

    /* Multi-character operators */
    char c = peek(lex);
    char c2 = peek2(lex);
    char c3 = peek3(lex);

    /* Three-char tokens */
    if (c == '.' && c2 == '.' && c3 == '=') {
        advance(lex); advance(lex); advance(lex);
        return make_token(lex, TOK_DOTDOTEQ, start, 3, sloc);
    }
    if (c == '.' && c2 == '.' && c3 == '.') {
        advance(lex); advance(lex); advance(lex);
        return make_token(lex, TOK_ELLIPSIS, start, 3, sloc);
    }
    if (c == '<' && c2 == '<' && c3 == '=') {
        advance(lex); advance(lex); advance(lex);
        return make_token(lex, TOK_SHL_ASSIGN, start, 3, sloc);
    }
    if (c == '>' && c2 == '>' && c3 == '=') {
        advance(lex); advance(lex); advance(lex);
        return make_token(lex, TOK_SHR_ASSIGN, start, 3, sloc);
    }
    if (c == '&' && c2 == '&' && c3 == '=') {
        advance(lex); advance(lex); advance(lex);
        return make_token(lex, TOK_AND_ASSIGN, start, 3, sloc);
    }
    if (c == '|' && c2 == '|' && c3 == '=') {
        advance(lex); advance(lex); advance(lex);
        return make_token(lex, TOK_OR_ASSIGN, start, 3, sloc);
    }

    /* Two-char tokens */
    if (c == '.' && c2 == '.') { advance(lex); advance(lex); return make_token(lex, TOK_DOTDOT, start, 2, sloc); }
    if (c == '-' && c2 == '>') { advance(lex); advance(lex); return make_token(lex, TOK_ARROW, start, 2, sloc); }
    if (c == '|' && c2 == '>') { advance(lex); advance(lex); return make_token(lex, TOK_PIPE_ARROW, start, 2, sloc); }
    if (c == '=' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_EQ, start, 2, sloc); }
    if (c == '!' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_NEQ, start, 2, sloc); }
    if (c == '<' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_LE, start, 2, sloc); }
    if (c == '>' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_GE, start, 2, sloc); }
    if (c == '<' && c2 == '<') { advance(lex); advance(lex); return make_token(lex, TOK_SHL, start, 2, sloc); }
    if (c == '>' && c2 == '>') { advance(lex); advance(lex); return make_token(lex, TOK_SHR, start, 2, sloc); }
    if (c == '+' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_PLUS_ASSIGN, start, 2, sloc); }
    if (c == '-' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_MINUS_ASSIGN, start, 2, sloc); }
    if (c == '*' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_STAR_ASSIGN, start, 2, sloc); }
    if (c == '/' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_SLASH_ASSIGN, start, 2, sloc); }
    if (c == '%' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_PERCENT_ASSIGN, start, 2, sloc); }
    if (c == '&' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_AMP_ASSIGN, start, 2, sloc); }
    if (c == '|' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_PIPE_ASSIGN, start, 2, sloc); }
    if (c == '^' && c2 == '=') { advance(lex); advance(lex); return make_token(lex, TOK_CARET_ASSIGN, start, 2, sloc); }

    /* Single-char tokens */
    advance(lex);
    switch (c) {
        case '(': lex->bracket_depth++; return make_token(lex, TOK_LPAREN, start, 1, sloc);
        case ')': lex->bracket_depth--; return make_token(lex, TOK_RPAREN, start, 1, sloc);
        case '[': lex->bracket_depth++; return make_token(lex, TOK_LBRACKET, start, 1, sloc);
        case ']': lex->bracket_depth--; return make_token(lex, TOK_RBRACKET, start, 1, sloc);
        case '{': lex->bracket_depth++; return make_token(lex, TOK_LBRACE, start, 1, sloc);
        case '}': lex->bracket_depth--; return make_token(lex, TOK_RBRACE, start, 1, sloc);
        case '+': return make_token(lex, TOK_PLUS, start, 1, sloc);
        case '-': return make_token(lex, TOK_MINUS, start, 1, sloc);
        case '*': return make_token(lex, TOK_STAR, start, 1, sloc);
        case '/': return make_token(lex, TOK_SLASH, start, 1, sloc);
        case '%': return make_token(lex, TOK_PERCENT, start, 1, sloc);
        case '&': return make_token(lex, TOK_AMP, start, 1, sloc);
        case '|': return make_token(lex, TOK_PIPE, start, 1, sloc);
        case '^': return make_token(lex, TOK_CARET, start, 1, sloc);
        case '~': return make_token(lex, TOK_TILDE, start, 1, sloc);
        case '!': return make_token(lex, TOK_BANG, start, 1, sloc);
        case '?': return make_token(lex, TOK_QUESTION, start, 1, sloc);
        case '@': return make_token(lex, TOK_AT, start, 1, sloc);
        case '.': return make_token(lex, TOK_DOT, start, 1, sloc);
        case ',': return make_token(lex, TOK_COMMA, start, 1, sloc);
        case ':': return make_token(lex, TOK_COLON, start, 1, sloc);
        case '#': return make_token(lex, TOK_HASH, start, 1, sloc);
        case '<': return make_token(lex, TOK_LT, start, 1, sloc);
        case '>': return make_token(lex, TOK_GT, start, 1, sloc);
        case '=': return make_token(lex, TOK_ASSIGN, start, 1, sloc);
        case '\'': return make_token(lex, TOK_TICK, start, 1, sloc);
        default:
            return make_token(lex, TOK_ERROR, start, 1, sloc);
    }
}

Token *lexer_lex_all(Lexer *lex) {
    Token *tokens = NULL;
    for (;;) {
        Token tok = lexer_next(lex);
        vec_push(tokens, tok);
        if (tok.kind == TOK_EOF) break;
    }
    return tokens;
}
