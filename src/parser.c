#include "parser.h"
#include "vec.h"
#include <stdio.h>
#include <string.h>

/* ===== Utility ===== */

void parser_init(Parser *p, Token *tokens, int count, Arena *arena, InternTable *interns) {
    p->tokens = tokens;
    p->count = count;
    p->pos = 0;
    p->arena = arena;
    p->interns = interns;
    p->errors = 0;
}

static Token *peek(Parser *p) {
    return &p->tokens[p->pos];
}

static Token *advance(Parser *p) {
    Token *t = &p->tokens[p->pos];
    if (t->kind != TOK_EOF) p->pos++;
    return t;
}

static bool at(Parser *p, TokenKind kind) {
    return peek(p)->kind == kind;
}

static bool match(Parser *p, TokenKind kind) {
    if (peek(p)->kind == kind) {
        advance(p);
        return true;
    }
    return false;
}

static Token *expect(Parser *p, TokenKind kind) {
    if (peek(p)->kind == kind) {
        return advance(p);
    }
    p->errors++;
    error_at(peek(p)->loc, "expected '%s', got '%s'",
             token_kind_str(kind), token_kind_str(peek(p)->kind));
    return peek(p);
}

static SrcLoc cur_loc(Parser *p) {
    return peek(p)->loc;
}

static void skip_newlines(Parser *p) {
    while (at(p, TOK_NEWLINE)) advance(p);
}

static Ident make_ident(Token *t) {
    return (Ident){ .name = t->start, .len = t->len, .loc = t->loc };
}

/* Forward declarations */
static AstNode *parse_expr(Parser *p);
static AstNode *parse_statement(Parser *p);
static AstType *parse_type_expr(Parser *p);
static AstPattern *parse_pattern(Parser *p);
static AstPattern *parse_base_pattern(Parser *p);
static NodeList parse_block(Parser *p);
static TypePath parse_type_path(Parser *p);

/* ===== Arena helpers ===== */

static void *aalloc(Parser *p, size_t size) {
    return arena_alloc(p->arena, size);
}

/* ===== Type Expressions ===== */

static TypePath parse_type_path(Parser *p) {
    TypePath tp = {0};
    Ident *segs = NULL;
    Token *t = expect(p, TOK_UPPER_IDENT);
    vec_push(segs, make_ident(t));
    while (at(p, TOK_DOT) && p->tokens[p->pos + 1].kind == TOK_UPPER_IDENT) {
        advance(p); /* . */
        t = advance(p);
        vec_push(segs, make_ident(t));
    }
    tp.seg_count = (int)vec_len(segs);
    tp.segments = (Ident *)aalloc(p, tp.seg_count * sizeof(Ident));
    memcpy(tp.segments, segs, tp.seg_count * sizeof(Ident));
    vec_free(segs);
    return tp;
}

static TypeList parse_type_list(Parser *p) {
    AstType **types = NULL;
    vec_push(types, parse_type_expr(p));
    while (match(p, TOK_COMMA)) {
        if (at(p, TOK_RPAREN) || at(p, TOK_RBRACKET) || at(p, TOK_RBRACE)) break;
        vec_push(types, parse_type_expr(p));
    }
    TypeList tl;
    tl.count = (int)vec_len(types);
    tl.items = (AstType **)ast_type_list(p->arena, types, tl.count);
    vec_free(types);
    return tl;
}

static TypeList parse_type_args(Parser *p) {
    expect(p, TOK_LBRACKET);
    TypeList tl = parse_type_list(p);
    expect(p, TOK_RBRACKET);
    return tl;
}

static AstType *parse_type_primary(Parser *p) {
    SrcLoc loc = cur_loc(p);

    if (at(p, TOK_STAR)) {
        advance(p);
        AstType *elem = parse_type_primary(p);
        AstType *t = ast_new_type(p->arena, TYPE_POINTER, loc);
        t->as.pointer_elem = elem;
        return t;
    }

    if (at(p, TOK_SELF_UPPER)) {
        advance(p);
        return ast_new_type(p->arena, TYPE_SELF, loc);
    }

    if (at(p, TOK_FN)) {
        advance(p);
        expect(p, TOK_LPAREN);
        TypeList params = {0};
        if (!at(p, TOK_RPAREN)) {
            params = parse_type_list(p);
        }
        expect(p, TOK_RPAREN);
        AstType *ret = NULL;
        if (match(p, TOK_ARROW)) {
            ret = parse_type_expr(p);
        }
        AstType *t = ast_new_type(p->arena, TYPE_FN, loc);
        t->as.fn.params = params;
        t->as.fn.ret = ret;
        return t;
    }

    if (at(p, TOK_LPAREN)) {
        advance(p);
        TypeList tl = parse_type_list(p);
        expect(p, TOK_RPAREN);
        AstType *t = ast_new_type(p->arena, TYPE_TUPLE, loc);
        t->as.tuple = tl;
        return t;
    }

    if (at(p, TOK_LBRACKET)) {
        advance(p);
        AstType *elem = parse_type_expr(p);
        expect(p, TOK_RBRACKET);
        AstType *t = ast_new_type(p->arena, TYPE_LIST, loc);
        t->as.list_elem = elem;
        return t;
    }

    if (at(p, TOK_LBRACE)) {
        advance(p);
        AstType *key = parse_type_expr(p);
        expect(p, TOK_COLON);
        AstType *val = parse_type_expr(p);
        expect(p, TOK_RBRACE);
        AstType *t = ast_new_type(p->arena, TYPE_MAP, loc);
        t->as.map.key = key;
        t->as.map.val = val;
        return t;
    }

    /* type_path with optional type_args */
    if (at(p, TOK_UPPER_IDENT)) {
        TypePath path = parse_type_path(p);
        TypeList args = {0};
        if (at(p, TOK_LBRACKET)) {
            args = parse_type_args(p);
        }
        AstType *t = ast_new_type(p->arena, TYPE_PATH, loc);
        t->as.path.path = path;
        t->as.path.args = args;
        return t;
    }

    p->errors++;
    error_at(loc, "expected type expression");
    return ast_new_type(p->arena, TYPE_PATH, loc);
}

static AstType *parse_type_expr(Parser *p) {
    AstType *first = parse_type_primary(p);

    if (!at(p, TOK_PIPE)) return first;

    /* Union type */
    AstType **types = NULL;
    vec_push(types, first);
    while (match(p, TOK_PIPE)) {
        vec_push(types, parse_type_primary(p));
    }
    SrcLoc loc = first->loc;
    AstType *t = ast_new_type(p->arena, TYPE_UNION, loc);
    t->as.union_types.count = (int)vec_len(types);
    t->as.union_types.items = (AstType **)ast_type_list(p->arena, types, t->as.union_types.count);
    vec_free(types);
    return t;
}

/* ===== Type Parameters ===== */

static TypeParamList parse_type_params(Parser *p) {
    expect(p, TOK_LBRACKET);
    TypeParam *params = NULL;
    do {
        TypeParam tp = {0};
        Token *t = expect(p, TOK_UPPER_IDENT);
        tp.name = make_ident(t);
        if (match(p, TOK_COLON)) {
            TypePath *cons = NULL;
            vec_push(cons, parse_type_path(p));
            while (match(p, TOK_PLUS)) {
                vec_push(cons, parse_type_path(p));
            }
            tp.constraint_count = (int)vec_len(cons);
            tp.constraints = (TypePath *)aalloc(p, tp.constraint_count * sizeof(TypePath));
            memcpy(tp.constraints, cons, tp.constraint_count * sizeof(TypePath));
            vec_free(cons);
        }
        vec_push(params, tp);
    } while (match(p, TOK_COMMA) && !at(p, TOK_RBRACKET));
    expect(p, TOK_RBRACKET);

    TypeParamList tpl;
    tpl.count = (int)vec_len(params);
    tpl.items = (TypeParam *)aalloc(p, tpl.count * sizeof(TypeParam));
    memcpy(tpl.items, params, tpl.count * sizeof(TypeParam));
    vec_free(params);
    return tpl;
}

/* ===== Patterns ===== */

static AstPattern *parse_base_pattern(Parser *p) {
    SrcLoc loc = cur_loc(p);

    /* Wildcard */
    if (at(p, TOK_UNDERSCORE)) {
        advance(p);
        return ast_new_pattern(p->arena, PAT_WILDCARD, loc);
    }

    /* Rest pattern .. */
    if (at(p, TOK_DOTDOT)) {
        advance(p);
        AstPattern *pat = ast_new_pattern(p->arena, PAT_REST, loc);
        if (at(p, TOK_IDENT)) {
            Token *t = advance(p);
            Ident *name = (Ident *)aalloc(p, sizeof(Ident));
            *name = make_ident(t);
            pat->as.rest_name = name;
        }
        return pat;
    }

    /* Literal patterns */
    if (at(p, TOK_INT) || at(p, TOK_FLOAT) || at(p, TOK_STRING) ||
        at(p, TOK_CHAR) || at(p, TOK_TRUE) || at(p, TOK_FALSE) || at(p, TOK_NONE)) {
        AstPattern *pat = ast_new_pattern(p->arena, PAT_LITERAL, loc);
        /* Create a literal node for the value */
        Token *t = advance(p);
        AstNode *lit = ast_new(p->arena, NODE_LITERAL, t->loc);
        lit->as.literal.value = t->start;
        lit->as.literal.len = t->len;
        if (t->kind == TOK_INT) lit->as.literal.kind = LIT_INT;
        else if (t->kind == TOK_FLOAT) lit->as.literal.kind = LIT_FLOAT;
        else if (t->kind == TOK_STRING) lit->as.literal.kind = LIT_STRING;
        else if (t->kind == TOK_CHAR) lit->as.literal.kind = LIT_CHAR;
        else if (t->kind == TOK_TRUE || t->kind == TOK_FALSE) lit->as.literal.kind = LIT_BOOL;
        else lit->as.literal.kind = LIT_NONE;
        pat->as.literal = lit;
        return pat;
    }

    /* Variant pattern: UPPER_IDENT(.UPPER_IDENT)* [( patterns )] */
    if (at(p, TOK_UPPER_IDENT)) {
        TypePath path = parse_type_path(p);
        AstPattern *pat = ast_new_pattern(p->arena, PAT_VARIANT, loc);
        pat->as.variant.path = path;
        if (match(p, TOK_LPAREN)) {
            AstPattern **pats = NULL;
            if (!at(p, TOK_RPAREN)) {
                vec_push(pats, parse_pattern(p));
                while (match(p, TOK_COMMA)) {
                    if (at(p, TOK_RPAREN)) break;
                    vec_push(pats, parse_pattern(p));
                }
            }
            expect(p, TOK_RPAREN);
            pat->as.variant.args.count = (int)vec_len(pats);
            pat->as.variant.args.items = ast_pattern_list(p->arena, pats, pat->as.variant.args.count);
            vec_free(pats);
        }
        return pat;
    }

    /* Tuple pattern (a, b) */
    if (at(p, TOK_LPAREN)) {
        advance(p);
        AstPattern **pats = NULL;
        if (!at(p, TOK_RPAREN)) {
            vec_push(pats, parse_pattern(p));
            while (match(p, TOK_COMMA)) {
                if (at(p, TOK_RPAREN)) break;
                vec_push(pats, parse_pattern(p));
            }
        }
        expect(p, TOK_RPAREN);
        AstPattern *pat = ast_new_pattern(p->arena, PAT_TUPLE, loc);
        pat->as.tuple.count = (int)vec_len(pats);
        pat->as.tuple.items = ast_pattern_list(p->arena, pats, pat->as.tuple.count);
        vec_free(pats);
        return pat;
    }

    /* List pattern [a, b] or [a, b, ..rest] */
    if (at(p, TOK_LBRACKET)) {
        advance(p);
        AstPattern **pats = NULL;
        bool has_rest = false;
        Ident *rest_name = NULL;

        if (!at(p, TOK_RBRACKET)) {
            vec_push(pats, parse_pattern(p));
            while (match(p, TOK_COMMA)) {
                if (at(p, TOK_RBRACKET)) break;
                if (at(p, TOK_DOTDOT)) {
                    advance(p);
                    has_rest = true;
                    if (at(p, TOK_IDENT)) {
                        Token *t = advance(p);
                        rest_name = (Ident *)aalloc(p, sizeof(Ident));
                        *rest_name = make_ident(t);
                    }
                    match(p, TOK_COMMA); /* optional trailing comma */
                    break;
                }
                vec_push(pats, parse_pattern(p));
            }
        }
        expect(p, TOK_RBRACKET);

        if (has_rest) {
            AstPattern *pat = ast_new_pattern(p->arena, PAT_LIST_REST, loc);
            pat->as.list_rest.elems.count = (int)vec_len(pats);
            pat->as.list_rest.elems.items = ast_pattern_list(p->arena, pats, pat->as.list_rest.elems.count);
            pat->as.list_rest.rest_name = rest_name;
            vec_free(pats);
            return pat;
        }

        AstPattern *pat = ast_new_pattern(p->arena, PAT_LIST, loc);
        pat->as.list.count = (int)vec_len(pats);
        pat->as.list.items = ast_pattern_list(p->arena, pats, pat->as.list.count);
        vec_free(pats);
        return pat;
    }

    /* Record pattern { x: pat, y } */
    if (at(p, TOK_LBRACE)) {
        advance(p);
        FieldPattern *fields = NULL;

        while (!at(p, TOK_RBRACE) && !at(p, TOK_EOF)) {
            if (at(p, TOK_DOTDOT)) {
                advance(p);
                /* Rest pattern in record — we'll store it as a field with no pattern and ".." name */
                FieldPattern fp = {0};
                fp.name = (Ident){ .name = "..", .len = 2, .loc = cur_loc(p) };
                vec_push(fields, fp);
                match(p, TOK_COMMA);
                break;
            }
            Token *t = expect(p, TOK_IDENT);
            FieldPattern fp = {0};
            fp.name = make_ident(t);
            if (match(p, TOK_COLON)) {
                fp.pattern = parse_pattern(p);
            }
            vec_push(fields, fp);
            if (!match(p, TOK_COMMA)) break;
        }
        expect(p, TOK_RBRACE);

        AstPattern *pat = ast_new_pattern(p->arena, PAT_RECORD, loc);
        pat->as.record.count = (int)vec_len(fields);
        pat->as.record.fields = (FieldPattern *)aalloc(p, pat->as.record.count * sizeof(FieldPattern));
        memcpy(pat->as.record.fields, fields, pat->as.record.count * sizeof(FieldPattern));
        vec_free(fields);
        return pat;
    }

    /* Binding pattern: ident possibly followed by @ pattern */
    if (at(p, TOK_IDENT)) {
        Token *t = advance(p);
        if (match(p, TOK_AT)) {
            AstPattern *inner = parse_pattern(p);
            AstPattern *pat = ast_new_pattern(p->arena, PAT_BIND_AS, loc);
            pat->as.bind_as.name = make_ident(t);
            pat->as.bind_as.pattern = inner;
            return pat;
        }
        AstPattern *pat = ast_new_pattern(p->arena, PAT_BIND, loc);
        pat->as.bind = make_ident(t);
        return pat;
    }

    /* Negative literal pattern: -INT or -FLOAT */
    if (at(p, TOK_MINUS) && (p->tokens[p->pos + 1].kind == TOK_INT || p->tokens[p->pos + 1].kind == TOK_FLOAT)) {
        Token *neg = advance(p); /* - */
        Token *t = advance(p);
        AstPattern *pat = ast_new_pattern(p->arena, PAT_LITERAL, loc);
        AstNode *lit = ast_new(p->arena, NODE_LITERAL, neg->loc);
        /* Point to the minus sign; length covers minus and number */
        lit->as.literal.value = neg->start;
        lit->as.literal.len = (int)(t->start + t->len - neg->start);
        lit->as.literal.kind = (t->kind == TOK_INT) ? LIT_INT : LIT_FLOAT;
        pat->as.literal = lit;
        return pat;
    }

    p->errors++;
    error_at(loc, "expected pattern");
    return ast_new_pattern(p->arena, PAT_WILDCARD, loc);
}

static AstPattern *parse_pattern(Parser *p) {
    AstPattern *first = parse_base_pattern(p);

    /* Check for or-pattern */
    if (!at(p, TOK_PIPE)) return first;

    AstPattern **pats = NULL;
    vec_push(pats, first);
    while (match(p, TOK_PIPE)) {
        vec_push(pats, parse_base_pattern(p));
    }
    AstPattern *pat = ast_new_pattern(p->arena, PAT_OR, first->loc);
    pat->as.or_pats.count = (int)vec_len(pats);
    pat->as.or_pats.items = ast_pattern_list(p->arena, pats, pat->as.or_pats.count);
    vec_free(pats);
    return pat;
}

/* ===== Expression Parsing ===== */

static AstNode *parse_primary(Parser *p);
static AstNode *parse_postfix(Parser *p);
static AstNode *parse_unary(Parser *p);
static AstNode *parse_if(Parser *p);
static AstNode *parse_match_expr(Parser *p);
static AstNode *parse_for(Parser *p);
static AstNode *parse_while(Parser *p);
static AstNode *parse_loop(Parser *p);
static AstNode *parse_lambda(Parser *p);

/* Parse argument list for function calls */
static ArgList parse_arg_list(Parser *p) {
    Arg *args = NULL;
    if (!at(p, TOK_RPAREN)) {
        do {
            if (at(p, TOK_RPAREN)) break;
            Arg arg = {0};
            /* Check for named argument: ident ":" expr */
            if (at(p, TOK_IDENT) && p->tokens[p->pos + 1].kind == TOK_COLON) {
                Token *t = advance(p);
                advance(p); /* skip : */
                arg.name = (Ident *)aalloc(p, sizeof(Ident));
                *arg.name = make_ident(t);
                arg.value = parse_expr(p);
            } else if (at(p, TOK_UNDERSCORE)) {
                /* Placeholder in pipe target */
                SrcLoc loc = cur_loc(p);
                advance(p);
                arg.value = ast_new(p->arena, NODE_PLACEHOLDER, loc);
            } else {
                arg.value = parse_expr(p);
            }
            vec_push(args, arg);
        } while (match(p, TOK_COMMA));
    }

    ArgList al;
    al.count = (int)vec_len(args);
    al.items = (Arg *)aalloc(p, al.count * sizeof(Arg));
    if (al.count > 0) memcpy(al.items, args, al.count * sizeof(Arg));
    vec_free(args);
    return al;
}

static AstNode *parse_literal(Parser *p) {
    Token *t = advance(p);
    AstNode *n = ast_new(p->arena, NODE_LITERAL, t->loc);
    n->as.literal.value = t->start;
    n->as.literal.len = t->len;
    switch (t->kind) {
        case TOK_INT:    n->as.literal.kind = LIT_INT; break;
        case TOK_FLOAT:  n->as.literal.kind = LIT_FLOAT; break;
        case TOK_STRING: case TOK_STRING_BEGIN: case TOK_STRING_END:
            n->as.literal.kind = LIT_STRING; break;
        case TOK_CHAR:   n->as.literal.kind = LIT_CHAR; break;
        case TOK_TRUE: case TOK_FALSE:
            n->as.literal.kind = LIT_BOOL; break;
        case TOK_NONE:   n->as.literal.kind = LIT_NONE; break;
        default:         n->as.literal.kind = LIT_INT; break;
    }
    return n;
}

static AstNode *parse_if(Parser *p) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_IF);
    AstNode *cond = parse_expr(p);

    /* Inline if: if x then a else b */
    if (match(p, TOK_THEN)) {
        AstNode *then_expr = parse_expr(p);
        expect(p, TOK_ELSE);
        AstNode *else_expr = parse_expr(p);
        AstNode *n = ast_new(p->arena, NODE_IF, loc);
        n->as.if_expr.cond = cond;
        n->as.if_expr.is_inline = true;
        n->as.if_expr.then_expr = then_expr;
        n->as.if_expr.else_expr = else_expr;
        return n;
    }

    /* Block if */
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);
    NodeList then_block = parse_block(p);
    expect(p, TOK_DEDENT);

    /* Collect elifs */
    ElifClause *elifs = NULL;
    while (at(p, TOK_ELIF)) {
        advance(p);
        AstNode *elif_cond = parse_expr(p);
        expect(p, TOK_NEWLINE);
        expect(p, TOK_INDENT);
        NodeList elif_block = parse_block(p);
        expect(p, TOK_DEDENT);
        ElifClause elif = { elif_cond, elif_block };
        vec_push(elifs, elif);
    }

    /* Optional else */
    NodeList else_block = {0};
    if (match(p, TOK_ELSE)) {
        expect(p, TOK_NEWLINE);
        expect(p, TOK_INDENT);
        else_block = parse_block(p);
        expect(p, TOK_DEDENT);
    }

    AstNode *n = ast_new(p->arena, NODE_IF, loc);
    n->as.if_expr.cond = cond;
    n->as.if_expr.then_block = then_block;
    n->as.if_expr.else_block = else_block;
    n->as.if_expr.is_inline = false;
    n->as.if_expr.elif_count = (int)vec_len(elifs);
    if (n->as.if_expr.elif_count > 0) {
        size_t sz = n->as.if_expr.elif_count * sizeof(elifs[0]);
        n->as.if_expr.elifs = aalloc(p, sz);
        memcpy(n->as.if_expr.elifs, elifs, sz);
    }
    vec_free(elifs);
    return n;
}

static AstNode *parse_match_expr(Parser *p) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_MATCH);
    AstNode *subject = parse_expr(p);
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);

    MatchArm *arms = NULL;
    while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
        skip_newlines(p);
        if (at(p, TOK_DEDENT)) break;

        MatchArm arm = {0};
        arm.pattern = parse_pattern(p);
        if (match(p, TOK_IF)) {
            arm.guard = parse_expr(p);
        }
        expect(p, TOK_ARROW);

        /* arm body: expr or INDENT block DEDENT */
        if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
            advance(p); /* NEWLINE */
            expect(p, TOK_INDENT);
            NodeList body = parse_block(p);
            expect(p, TOK_DEDENT);
            AstNode *block = ast_new(p->arena, NODE_BLOCK, loc);
            block->as.block = body;
            arm.body = block;
        } else {
            arm.body = parse_expr(p);
        }
        vec_push(arms, arm);
        skip_newlines(p);
    }
    expect(p, TOK_DEDENT);

    AstNode *n = ast_new(p->arena, NODE_MATCH, loc);
    n->as.match.subject = subject;
    n->as.match.arm_count = (int)vec_len(arms);
    n->as.match.arms = (MatchArm *)aalloc(p, n->as.match.arm_count * sizeof(MatchArm));
    memcpy(n->as.match.arms, arms, n->as.match.arm_count * sizeof(MatchArm));
    vec_free(arms);
    return n;
}

static AstNode *parse_for(Parser *p) {
    SrcLoc loc = cur_loc(p);
    Ident *label = NULL;

    /* Check for label: 'name for ... */
    if (at(p, TOK_TICK)) {
        advance(p);
        Token *t = expect(p, TOK_IDENT);
        label = (Ident *)aalloc(p, sizeof(Ident));
        *label = make_ident(t);
    }

    expect(p, TOK_FOR);
    AstPattern *pattern = parse_pattern(p);
    expect(p, TOK_IN);
    AstNode *iter = parse_expr(p);
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);
    NodeList body = parse_block(p);
    expect(p, TOK_DEDENT);

    AstNode *n = ast_new(p->arena, NODE_FOR, loc);
    n->as.for_expr.label = label;
    n->as.for_expr.pattern = pattern;
    n->as.for_expr.iter = iter;
    n->as.for_expr.body = body;
    return n;
}

static AstNode *parse_while(Parser *p) {
    SrcLoc loc = cur_loc(p);
    Ident *label = NULL;

    if (at(p, TOK_TICK)) {
        advance(p);
        Token *t = expect(p, TOK_IDENT);
        label = (Ident *)aalloc(p, sizeof(Ident));
        *label = make_ident(t);
    }

    expect(p, TOK_WHILE);
    AstNode *cond = parse_expr(p);
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);
    NodeList body = parse_block(p);
    expect(p, TOK_DEDENT);

    AstNode *n = ast_new(p->arena, NODE_WHILE, loc);
    n->as.while_expr.label = label;
    n->as.while_expr.cond = cond;
    n->as.while_expr.body = body;
    return n;
}

static AstNode *parse_loop(Parser *p) {
    SrcLoc loc = cur_loc(p);
    Ident *label = NULL;

    if (at(p, TOK_TICK)) {
        advance(p);
        Token *t = expect(p, TOK_IDENT);
        label = (Ident *)aalloc(p, sizeof(Ident));
        *label = make_ident(t);
    }

    expect(p, TOK_LOOP);
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);
    NodeList body = parse_block(p);
    expect(p, TOK_DEDENT);

    AstNode *n = ast_new(p->arena, NODE_LOOP, loc);
    n->as.loop_expr.label = label;
    n->as.loop_expr.body = body;
    return n;
}

static AstNode *parse_lambda(Parser *p) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_FN);
    expect(p, TOK_LPAREN);

    /* Parse short param list: ident [ ":" type ] */
    Param *params = NULL;
    if (!at(p, TOK_RPAREN)) {
        do {
            if (at(p, TOK_RPAREN)) break;
            Param param = {0};
            Token *t = expect(p, TOK_IDENT);
            AstPattern *pat = ast_new_pattern(p->arena, PAT_BIND, t->loc);
            pat->as.bind = make_ident(t);
            param.pattern = pat;
            if (match(p, TOK_COLON)) {
                param.type = parse_type_expr(p);
            }
            vec_push(params, param);
        } while (match(p, TOK_COMMA));
    }
    expect(p, TOK_RPAREN);

    AstNode *n = ast_new(p->arena, NODE_LAMBDA, loc);
    n->as.lambda.params.count = (int)vec_len(params);
    n->as.lambda.params.items = (Param *)aalloc(p, n->as.lambda.params.count * sizeof(Param));
    if (n->as.lambda.params.count > 0) {
        memcpy(n->as.lambda.params.items, params, n->as.lambda.params.count * sizeof(Param));
    }
    vec_free(params);

    /* Body: expr or INDENT block DEDENT */
    if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
        advance(p); /* NEWLINE */
        expect(p, TOK_INDENT);
        n->as.lambda.body_block = parse_block(p);
        expect(p, TOK_DEDENT);
    } else {
        n->as.lambda.body_expr = parse_expr(p);
    }
    return n;
}

static AstNode *parse_list_or_comp(Parser *p) {
    SrcLoc loc = cur_loc(p);
    advance(p); /* [ */

    if (at(p, TOK_RBRACKET)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_LIST, loc);
        n->as.list = (NodeList){0};
        return n;
    }

    AstNode *first = parse_expr(p);

    /* List comprehension: [expr for pattern in iter [if filter]] */
    if (at(p, TOK_FOR)) {
        advance(p);
        AstPattern *pat = parse_pattern(p);
        expect(p, TOK_IN);
        AstNode *iter = parse_expr(p);
        AstNode *filter = NULL;
        if (match(p, TOK_IF)) filter = parse_expr(p);
        expect(p, TOK_RBRACKET);
        AstNode *n = ast_new(p->arena, NODE_LIST_COMP, loc);
        n->as.list_comp.expr = first;
        n->as.list_comp.pattern = pat;
        n->as.list_comp.iter = iter;
        n->as.list_comp.filter = filter;
        return n;
    }

    /* Regular list */
    AstNode **items = NULL;
    vec_push(items, first);
    while (match(p, TOK_COMMA)) {
        if (at(p, TOK_RBRACKET)) break;
        vec_push(items, parse_expr(p));
    }
    expect(p, TOK_RBRACKET);

    AstNode *n = ast_new(p->arena, NODE_LIST, loc);
    n->as.list.count = (int)vec_len(items);
    n->as.list.items = ast_node_list(p->arena, items, n->as.list.count);
    vec_free(items);
    return n;
}

static AstNode *parse_map_or_record(Parser *p) {
    SrcLoc loc = cur_loc(p);
    advance(p); /* { */

    if (at(p, TOK_RBRACE)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_MAP, loc);
        n->as.map.entries = NULL;
        n->as.map.count = 0;
        return n;
    }

    /* Parse first entry to determine if it's a map or might be a comprehension */
    MapEntry *entries = NULL;

    while (!at(p, TOK_RBRACE) && !at(p, TOK_EOF)) {
        MapEntry entry = {0};

        if (at(p, TOK_DOTDOT)) {
            /* Spread */
            advance(p);
            entry.is_spread = true;
            entry.value = parse_expr(p);
        } else {
            AstNode *first = parse_expr(p);
            if (at(p, TOK_COLON)) {
                /* key: value */
                advance(p);

                /* Check for map comprehension after first entry */
                if (vec_len(entries) == 0) {
                    AstNode *val = parse_expr(p);
                    if (at(p, TOK_FOR)) {
                        advance(p);
                        AstPattern *pat = parse_pattern(p);
                        expect(p, TOK_IN);
                        AstNode *iter = parse_expr(p);
                        AstNode *filter = NULL;
                        if (match(p, TOK_IF)) filter = parse_expr(p);
                        expect(p, TOK_RBRACE);
                        vec_free(entries);
                        AstNode *n = ast_new(p->arena, NODE_MAP_COMP, loc);
                        n->as.map_comp.key_expr = first;
                        n->as.map_comp.val_expr = val;
                        n->as.map_comp.pattern = pat;
                        n->as.map_comp.iter = iter;
                        n->as.map_comp.filter = filter;
                        return n;
                    }
                    entry.key = first;
                    entry.value = val;
                } else {
                    entry.key = first;
                    entry.value = parse_expr(p);
                }
            } else {
                /* Shorthand: just ident */
                entry.value = first;
            }
        }
        vec_push(entries, entry);
        if (!match(p, TOK_COMMA)) break;
    }
    expect(p, TOK_RBRACE);

    AstNode *n = ast_new(p->arena, NODE_MAP, loc);
    n->as.map.count = (int)vec_len(entries);
    n->as.map.entries = (MapEntry *)aalloc(p, n->as.map.count * sizeof(MapEntry));
    memcpy(n->as.map.entries, entries, n->as.map.count * sizeof(MapEntry));
    vec_free(entries);
    return n;
}

static AstNode *parse_primary(Parser *p) {
    SrcLoc loc = cur_loc(p);

    /* Literals */
    if (at(p, TOK_INT) || at(p, TOK_FLOAT) || at(p, TOK_STRING) ||
        at(p, TOK_CHAR) || at(p, TOK_TRUE) || at(p, TOK_FALSE) || at(p, TOK_NONE)) {
        return parse_literal(p);
    }

    /* String interpolation: STRING_BEGIN expr (STRING_PART expr)* STRING_END
     * We represent this as a series of CALL nodes to a synthetic "str_concat",
     * but for simplicity just build a list of parts as a special literal. */
    if (at(p, TOK_STRING_BEGIN)) {
        /* Collect all parts into a single string literal node for now.
         * A real compiler would create interpolation nodes. We'll create a
         * call-like structure: NODE_CALL with callee = "__interp__" */
        AstNode **parts = NULL;
        Token *begin = advance(p);
        AstNode *part = ast_new(p->arena, NODE_LITERAL, begin->loc);
        part->as.literal.kind = LIT_STRING;
        part->as.literal.value = begin->start;
        part->as.literal.len = begin->len;
        vec_push(parts, part);

        /* Parse interpolated expression */
        vec_push(parts, parse_expr(p));

        /* Parse remaining STRING_PART ... expr ... STRING_END */
        while (at(p, TOK_STRING_PART)) {
            Token *sp = advance(p);
            AstNode *seg = ast_new(p->arena, NODE_LITERAL, sp->loc);
            seg->as.literal.kind = LIT_STRING;
            seg->as.literal.value = sp->start;
            seg->as.literal.len = sp->len;
            vec_push(parts, seg);
            vec_push(parts, parse_expr(p));
        }

        if (at(p, TOK_STRING_END)) {
            Token *end = advance(p);
            AstNode *seg = ast_new(p->arena, NODE_LITERAL, end->loc);
            seg->as.literal.kind = LIT_STRING;
            seg->as.literal.value = end->start;
            seg->as.literal.len = end->len;
            vec_push(parts, seg);
        }

        /* Build a string interpolation node containing all string parts */
        AstNode *n = ast_new(p->arena, NODE_STRING_INTERP, loc);
        n->as.list.count = (int)vec_len(parts);
        n->as.list.items = ast_node_list(p->arena, parts, n->as.list.count);
        vec_free(parts);
        return n;
    }

    /* Identifier */
    if (at(p, TOK_IDENT)) {
        Token *t = advance(p);
        AstNode *n = ast_new(p->arena, NODE_IDENT, t->loc);
        n->as.ident = make_ident(t);
        return n;
    }

    /* Upper identifier (type path / variant constructor) */
    if (at(p, TOK_UPPER_IDENT)) {
        TypePath path = parse_type_path(p);
        AstNode *n = ast_new(p->arena, NODE_UPPER_IDENT, loc);
        n->as.type_path = path;
        return n;
    }

    /* Self */
    if (at(p, TOK_SELF_LOWER)) {
        Token *t = advance(p);
        AstNode *n = ast_new(p->arena, NODE_IDENT, t->loc);
        n->as.ident = make_ident(t);
        return n;
    }

    /* Grouping / Tuple: (expr) or (expr, ...) */
    if (at(p, TOK_LPAREN)) {
        advance(p);
        if (at(p, TOK_RPAREN)) {
            advance(p);
            /* Empty tuple / unit */
            AstNode *n = ast_new(p->arena, NODE_TUPLE, loc);
            n->as.list = (NodeList){0};
            return n;
        }
        AstNode *first = parse_expr(p);
        if (at(p, TOK_COMMA)) {
            /* Tuple */
            AstNode **items = NULL;
            vec_push(items, first);
            while (match(p, TOK_COMMA)) {
                if (at(p, TOK_RPAREN)) break;
                vec_push(items, parse_expr(p));
            }
            expect(p, TOK_RPAREN);
            AstNode *n = ast_new(p->arena, NODE_TUPLE, loc);
            n->as.list.count = (int)vec_len(items);
            n->as.list.items = ast_node_list(p->arena, items, n->as.list.count);
            vec_free(items);
            return n;
        }
        expect(p, TOK_RPAREN);
        return first; /* just grouping */
    }

    /* List literal or comprehension */
    if (at(p, TOK_LBRACKET)) {
        return parse_list_or_comp(p);
    }

    /* Map / record literal */
    if (at(p, TOK_LBRACE)) {
        return parse_map_or_record(p);
    }

    /* if expression */
    if (at(p, TOK_IF)) return parse_if(p);

    /* match expression */
    if (at(p, TOK_MATCH)) return parse_match_expr(p);

    /* for expression */
    if (at(p, TOK_FOR) || (at(p, TOK_TICK) && p->tokens[p->pos + 2].kind == TOK_FOR)) {
        return parse_for(p);
    }

    /* while expression */
    if (at(p, TOK_WHILE) || (at(p, TOK_TICK) && p->tokens[p->pos + 2].kind == TOK_WHILE)) {
        return parse_while(p);
    }

    /* loop expression */
    if (at(p, TOK_LOOP) || (at(p, TOK_TICK) && p->tokens[p->pos + 2].kind == TOK_LOOP)) {
        return parse_loop(p);
    }

    /* spawn */
    if (at(p, TOK_SPAWN)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_SPAWN, loc);
        if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
            advance(p);
            expect(p, TOK_INDENT);
            NodeList body = parse_block(p);
            expect(p, TOK_DEDENT);
            AstNode *block = ast_new(p->arena, NODE_BLOCK, loc);
            block->as.block = body;
            n->as.unary_stmt.value = block;
        } else {
            n->as.unary_stmt.value = parse_expr(p);
        }
        return n;
    }

    /* await */
    if (at(p, TOK_AWAIT)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_AWAIT, loc);
        n->as.unary_stmt.value = parse_expr(p);
        return n;
    }

    /* scope */
    if (at(p, TOK_SCOPE)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_SCOPE, loc);
        n->as.unary_stmt.value = parse_lambda(p);
        return n;
    }

    /* comptime */
    if (at(p, TOK_COMPTIME)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_COMPTIME, loc);
        if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
            advance(p);
            expect(p, TOK_INDENT);
            NodeList body = parse_block(p);
            expect(p, TOK_DEDENT);
            AstNode *block = ast_new(p->arena, NODE_BLOCK, loc);
            block->as.block = body;
            n->as.unary_stmt.value = block;
        } else {
            n->as.unary_stmt.value = parse_expr(p);
        }
        return n;
    }

    /* lambda: fn(...) expr */
    if (at(p, TOK_FN)) {
        return parse_lambda(p);
    }

    /* do block: do INDENT block DEDENT */
    if (at(p, TOK_DO)) {
        advance(p);
        expect(p, TOK_NEWLINE);
        expect(p, TOK_INDENT);
        NodeList body = parse_block(p);
        expect(p, TOK_DEDENT);
        AstNode *n = ast_new(p->arena, NODE_DO_BLOCK, loc);
        n->as.block = body;
        return n;
    }

    /* Underscore as placeholder */
    if (at(p, TOK_UNDERSCORE)) {
        advance(p);
        return ast_new(p->arena, NODE_PLACEHOLDER, loc);
    }

    p->errors++;
    error_at(loc, "expected expression, got '%s'", token_kind_str(peek(p)->kind));
    advance(p);
    return ast_new(p->arena, NODE_LITERAL, loc);
}

static AstNode *parse_postfix(Parser *p) {
    AstNode *expr = parse_primary(p);

    for (;;) {
        SrcLoc loc = cur_loc(p);

        /* Function call */
        if (at(p, TOK_LPAREN)) {
            advance(p);
            ArgList args = parse_arg_list(p);
            expect(p, TOK_RPAREN);
            AstNode *n = ast_new(p->arena, NODE_CALL, loc);
            n->as.call.callee = expr;
            n->as.call.args = args;
            expr = n;
            continue;
        }

        /* Field access */
        if (at(p, TOK_DOT) && (p->tokens[p->pos + 1].kind == TOK_IDENT ||
                                 p->tokens[p->pos + 1].kind == TOK_INT)) {
            advance(p); /* . */
            Token *field = advance(p);
            AstNode *n = ast_new(p->arena, NODE_FIELD_ACCESS, loc);
            n->as.field_access.object = expr;
            n->as.field_access.field = make_ident(field);
            expr = n;
            continue;
        }

        /* Index */
        if (at(p, TOK_LBRACKET)) {
            advance(p);
            AstNode *index = parse_expr(p);
            expect(p, TOK_RBRACKET);
            AstNode *n = ast_new(p->arena, NODE_INDEX, loc);
            n->as.index.object = expr;
            n->as.index.index = index;
            expr = n;
            continue;
        }

        /* Try (?) */
        if (at(p, TOK_QUESTION)) {
            advance(p);
            AstNode *n = ast_new(p->arena, NODE_TRY, loc);
            n->as.postfix.operand = expr;
            expr = n;
            continue;
        }

        /* Mutate (!) */
        if (at(p, TOK_BANG)) {
            advance(p);
            AstNode *n = ast_new(p->arena, NODE_MUTATE, loc);
            n->as.postfix.operand = expr;
            expr = n;
            continue;
        }

        /* With expression: expr with { ... } */
        if (at(p, TOK_WITH)) {
            advance(p);
            expect(p, TOK_LBRACE);
            FieldUpdate *updates = NULL;
            while (!at(p, TOK_RBRACE) && !at(p, TOK_EOF)) {
                Token *t = expect(p, TOK_IDENT);
                FieldUpdate fu = {0};
                fu.name = make_ident(t);
                if (match(p, TOK_COLON)) {
                    fu.value = parse_expr(p);
                }
                vec_push(updates, fu);
                if (!match(p, TOK_COMMA)) break;
            }
            expect(p, TOK_RBRACE);
            AstNode *n = ast_new(p->arena, NODE_WITH, loc);
            n->as.with.base = expr;
            n->as.with.update_count = (int)vec_len(updates);
            n->as.with.updates = (FieldUpdate *)aalloc(p, n->as.with.update_count * sizeof(FieldUpdate));
            memcpy(n->as.with.updates, updates, n->as.with.update_count * sizeof(FieldUpdate));
            vec_free(updates);
            expr = n;
            continue;
        }

        break;
    }
    return expr;
}

static AstNode *parse_unary(Parser *p) {
    if (at(p, TOK_MINUS) || at(p, TOK_TILDE)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *operand = parse_postfix(p);
        AstNode *n = ast_new(p->arena, NODE_UNARY, loc);
        n->as.unary.op = op->kind;
        n->as.unary.operand = operand;
        return n;
    }
    return parse_postfix(p);
}

/* Binary expression parsing by precedence climbing */

static AstNode *parse_mul(Parser *p) {
    AstNode *left = parse_unary(p);
    while (at(p, TOK_STAR) || at(p, TOK_SLASH) || at(p, TOK_PERCENT)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_unary(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_add(Parser *p) {
    AstNode *left = parse_mul(p);
    while (at(p, TOK_PLUS) || at(p, TOK_MINUS)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_mul(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_shift(Parser *p) {
    AstNode *left = parse_add(p);
    while (at(p, TOK_SHL) || at(p, TOK_SHR)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_add(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_bitand(Parser *p) {
    AstNode *left = parse_shift(p);
    while (at(p, TOK_AMP)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_shift(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_bitxor(Parser *p) {
    AstNode *left = parse_bitand(p);
    while (at(p, TOK_CARET)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_bitand(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_bitor(Parser *p) {
    AstNode *left = parse_bitxor(p);
    /* Note: | is also used for union types and or-patterns, but in expr context it's bitwise or */
    while (at(p, TOK_PIPE) && !at(p, TOK_PIPE_ARROW)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_bitxor(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_range(Parser *p) {
    AstNode *left = parse_bitor(p);
    if (at(p, TOK_DOTDOT) || at(p, TOK_DOTDOTEQ)) {
        SrcLoc loc = cur_loc(p);
        bool inclusive = at(p, TOK_DOTDOTEQ);
        advance(p);
        AstNode *right = parse_bitor(p);
        AstNode *n = ast_new(p->arena, NODE_RANGE, loc);
        n->as.range.start = left;
        n->as.range.end = right;
        n->as.range.inclusive = inclusive;
        return n;
    }
    return left;
}

static AstNode *parse_comparison(Parser *p) {
    AstNode *left = parse_range(p);
    if (at(p, TOK_EQ) || at(p, TOK_NEQ) || at(p, TOK_LT) || at(p, TOK_GT) ||
        at(p, TOK_LE) || at(p, TOK_GE) || at(p, TOK_IS)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_range(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        return n;
    }
    return left;
}

static AstNode *parse_not(Parser *p) {
    if (at(p, TOK_NOT)) {
        SrcLoc loc = cur_loc(p);
        advance(p);
        AstNode *operand = parse_comparison(p);
        AstNode *n = ast_new(p->arena, NODE_UNARY, loc);
        n->as.unary.op = TOK_NOT;
        n->as.unary.operand = operand;
        return n;
    }
    return parse_comparison(p);
}

static AstNode *parse_and(Parser *p) {
    AstNode *left = parse_not(p);
    while (at(p, TOK_AND)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_not(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_or(Parser *p) {
    AstNode *left = parse_and(p);
    while (at(p, TOK_OR)) {
        SrcLoc loc = cur_loc(p);
        Token *op = advance(p);
        AstNode *right = parse_and(p);
        AstNode *n = ast_new(p->arena, NODE_BINARY, loc);
        n->as.binary.op = op->kind;
        n->as.binary.left = left;
        n->as.binary.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_pipe_expr(Parser *p) {
    AstNode *left = parse_or(p);
    while (at(p, TOK_PIPE_ARROW)) {
        SrcLoc loc = cur_loc(p);
        advance(p); /* |> */

        /* Pipe target: ident [call_args], lambda, or ident(args_with_placeholder) */
        AstNode *right;
        if (at(p, TOK_FN)) {
            right = parse_lambda(p);
        } else {
            right = parse_postfix(p);
        }

        AstNode *n = ast_new(p->arena, NODE_PIPE, loc);
        n->as.pipe.left = left;
        n->as.pipe.right = right;
        left = n;
    }
    return left;
}

static AstNode *parse_expr(Parser *p) {
    return parse_pipe_expr(p);
}

/* ===== Statements ===== */

static bool is_assign_op(TokenKind k) {
    return k == TOK_ASSIGN || k == TOK_PLUS_ASSIGN || k == TOK_MINUS_ASSIGN ||
           k == TOK_STAR_ASSIGN || k == TOK_SLASH_ASSIGN || k == TOK_PERCENT_ASSIGN ||
           k == TOK_AMP_ASSIGN || k == TOK_PIPE_ASSIGN || k == TOK_CARET_ASSIGN ||
           k == TOK_AND_ASSIGN || k == TOK_OR_ASSIGN || k == TOK_SHL_ASSIGN ||
           k == TOK_SHR_ASSIGN;
}

static AstNode *parse_statement(Parser *p) {
    SrcLoc loc = cur_loc(p);

    /* let statement */
    if (at(p, TOK_LET)) {
        advance(p);
        AstPattern *pat = parse_pattern(p);
        AstType *type = NULL;
        if (match(p, TOK_COLON)) type = parse_type_expr(p);
        expect(p, TOK_ASSIGN);
        AstNode *value = parse_expr(p);
        AstNode *n = ast_new(p->arena, NODE_LET, loc);
        n->as.let.pattern = pat;
        n->as.let.type = type;
        n->as.let.value = value;
        return n;
    }

    /* var statement */
    if (at(p, TOK_VAR)) {
        advance(p);
        Token *t = expect(p, TOK_IDENT);
        AstPattern *pat = ast_new_pattern(p->arena, PAT_BIND, t->loc);
        pat->as.bind = make_ident(t);
        AstType *type = NULL;
        if (match(p, TOK_COLON)) type = parse_type_expr(p);
        expect(p, TOK_ASSIGN);
        AstNode *value = parse_expr(p);
        AstNode *n = ast_new(p->arena, NODE_VAR, loc);
        n->as.let.pattern = pat;
        n->as.let.type = type;
        n->as.let.value = value;
        return n;
    }

    /* return statement */
    if (at(p, TOK_RETURN)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_RETURN, loc);
        if (!at(p, TOK_NEWLINE) && !at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
            n->as.unary_stmt.value = parse_expr(p);
        }
        return n;
    }

    /* break statement */
    if (at(p, TOK_BREAK)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_BREAK, loc);
        if (at(p, TOK_TICK)) {
            advance(p);
            Token *t = expect(p, TOK_IDENT);
            n->as.break_stmt.label = (Ident *)aalloc(p, sizeof(Ident));
            *n->as.break_stmt.label = make_ident(t);
        }
        if (!at(p, TOK_NEWLINE) && !at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
            n->as.break_stmt.value = parse_expr(p);
        }
        return n;
    }

    /* continue statement */
    if (at(p, TOK_CONTINUE)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_CONTINUE, loc);
        if (at(p, TOK_TICK)) {
            advance(p);
            Token *t = expect(p, TOK_IDENT);
            n->as.continue_stmt.label = (Ident *)aalloc(p, sizeof(Ident));
            *n->as.continue_stmt.label = make_ident(t);
        }
        return n;
    }

    /* defer statement */
    if (at(p, TOK_DEFER)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_DEFER, loc);
        if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
            advance(p);
            expect(p, TOK_INDENT);
            NodeList body = parse_block(p);
            expect(p, TOK_DEDENT);
            AstNode *block = ast_new(p->arena, NODE_BLOCK, loc);
            block->as.block = body;
            n->as.defer.body = block;
        } else {
            n->as.defer.body = parse_expr(p);
        }
        return n;
    }

    /* assert statement */
    if (at(p, TOK_ASSERT)) {
        advance(p);
        AstNode *n = ast_new(p->arena, NODE_ASSERT, loc);
        n->as.assert_stmt.cond = parse_expr(p);
        if (match(p, TOK_COMMA)) {
            n->as.assert_stmt.message = parse_expr(p);
        }
        return n;
    }

    /* Expression (which could turn out to be an assignment) */
    AstNode *expr = parse_expr(p);

    /* Check for assignment */
    if (is_assign_op(peek(p)->kind)) {
        Token *op = advance(p);
        AstNode *value = parse_expr(p);
        AstNode *n = ast_new(p->arena, NODE_ASSIGN, loc);
        n->as.assign.target = expr;
        n->as.assign.op = op->kind;
        n->as.assign.value = value;
        return n;
    }

    /* Expression statement */
    AstNode *n = ast_new(p->arena, NODE_EXPR_STMT, loc);
    n->as.expr_stmt.expr = expr;
    return n;
}

static NodeList parse_block(Parser *p) {
    AstNode **stmts = NULL;
    while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
        skip_newlines(p);
        if (at(p, TOK_DEDENT) || at(p, TOK_EOF)) break;
        AstNode *stmt = parse_statement(p);
        vec_push(stmts, stmt);
        /* Consume trailing newline(s) */
        if (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
            if (at(p, TOK_NEWLINE)) {
                while (match(p, TOK_NEWLINE)) {}
            }
        }
    }
    NodeList nl;
    nl.count = (int)vec_len(stmts);
    nl.items = ast_node_list(p->arena, stmts, nl.count);
    vec_free(stmts);
    return nl;
}

/* ===== Top-Level Declarations ===== */

static Attribute parse_attribute_single(Parser *p) {
    expect(p, TOK_AT);
    Token *name = expect(p, TOK_IDENT);
    Attribute attr = {0};
    attr.name = make_ident(name);
    if (match(p, TOK_LPAREN)) {
        Arg *args = NULL;
        while (!at(p, TOK_RPAREN) && !at(p, TOK_EOF)) {
            Arg arg = {0};
            if (at(p, TOK_IDENT) && p->tokens[p->pos + 1].kind == TOK_COLON) {
                Token *t = advance(p);
                advance(p); /* : */
                arg.name = (Ident *)aalloc(p, sizeof(Ident));
                *arg.name = make_ident(t);
                arg.value = parse_expr(p);
            } else {
                arg.value = parse_expr(p);
            }
            vec_push(args, arg);
            if (!match(p, TOK_COMMA)) break;
        }
        expect(p, TOK_RPAREN);
        attr.arg_count = (int)vec_len(args);
        attr.args = (Arg *)aalloc(p, attr.arg_count * sizeof(Arg));
        memcpy(attr.args, args, attr.arg_count * sizeof(Arg));
        vec_free(args);
    }
    return attr;
}

/* Parse param list for fn declarations */
static ParamList parse_param_list(Parser *p) {
    Param *params = NULL;
    if (!at(p, TOK_RPAREN)) {
        do {
            if (at(p, TOK_RPAREN)) break;
            Param param = {0};
            if (at(p, TOK_SELF_LOWER)) {
                advance(p);
                param.is_self = true;
            } else {
                param.pattern = parse_pattern(p);
                if (match(p, TOK_COLON)) {
                    param.type = parse_type_expr(p);
                }
                if (match(p, TOK_ASSIGN)) {
                    param.default_val = parse_expr(p);
                }
            }
            vec_push(params, param);
        } while (match(p, TOK_COMMA));
    }

    ParamList pl;
    pl.count = (int)vec_len(params);
    pl.items = (Param *)aalloc(p, pl.count * sizeof(Param));
    if (pl.count > 0) memcpy(pl.items, params, pl.count * sizeof(Param));
    vec_free(params);
    return pl;
}

static AstNode *parse_import(Parser *p) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_IMPORT);

    /* import_path: ident { "." ident } */
    Ident *path = NULL;
    Token *t = expect(p, TOK_IDENT);
    vec_push(path, make_ident(t));
    while (at(p, TOK_DOT) &&
           (p->tokens[p->pos + 1].kind == TOK_IDENT) &&
           p->tokens[p->pos + 1].kind != TOK_LBRACE) {
        advance(p); /* . */
        t = advance(p);
        vec_push(path, make_ident(t));
    }

    AstNode *n = ast_new(p->arena, NODE_IMPORT, loc);
    n->as.import.path_count = (int)vec_len(path);
    n->as.import.path = (Ident *)aalloc(p, n->as.import.path_count * sizeof(Ident));
    memcpy(n->as.import.path, path, n->as.import.path_count * sizeof(Ident));
    vec_free(path);

    /* import_select: .{names} or .* */
    if (at(p, TOK_DOT)) {
        advance(p);
        if (match(p, TOK_STAR)) {
            n->as.import.is_glob = true;
        } else if (match(p, TOK_LBRACE)) {
            ImportName *names = NULL;
            do {
                if (at(p, TOK_RBRACE)) break;
                /* Import names can be IDENT or UPPER_IDENT (types) */
                Token *name;
                if (at(p, TOK_UPPER_IDENT)) name = advance(p);
                else name = expect(p, TOK_IDENT);
                ImportName in = {0};
                in.name = make_ident(name);
                if (match(p, TOK_AS)) {
                    Token *alias;
                    if (at(p, TOK_UPPER_IDENT)) alias = advance(p);
                    else alias = expect(p, TOK_IDENT);
                    in.alias = (Ident *)aalloc(p, sizeof(Ident));
                    *in.alias = make_ident(alias);
                }
                vec_push(names, in);
            } while (match(p, TOK_COMMA));
            expect(p, TOK_RBRACE);
            n->as.import.name_count = (int)vec_len(names);
            n->as.import.names = (ImportName *)aalloc(p, n->as.import.name_count * sizeof(ImportName));
            memcpy(n->as.import.names, names, n->as.import.name_count * sizeof(ImportName));
            vec_free(names);
        }
    }

    return n;
}

static FieldDef parse_field_def(Parser *p) {
    FieldDef f = {0};
    Token *t = expect(p, TOK_IDENT);
    f.name = make_ident(t);
    expect(p, TOK_COLON);
    f.type = parse_type_expr(p);
    if (match(p, TOK_ASSIGN)) {
        f.default_val = parse_expr(p);
    }
    return f;
}

static AstNode *parse_type_decl(Parser *p, Attribute *attrs, int attr_count) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_TYPE);
    Token *name = expect(p, TOK_UPPER_IDENT);

    AstNode *n = ast_new(p->arena, NODE_TYPE_DECL, loc);
    n->as.type_decl.name = make_ident(name);
    n->attrs = attrs;
    n->attr_count = attr_count;

    /* Optional type params */
    if (at(p, TOK_LBRACKET)) {
        n->as.type_decl.type_params = parse_type_params(p);
    }

    /* Type body: alias, record, or enum */
    if (match(p, TOK_ASSIGN)) {
        /* Type alias */
        n->as.type_decl.body_kind = TYPE_BODY_ALIAS;
        n->as.type_decl.body.alias = parse_type_expr(p);
    } else if (at(p, TOK_NEWLINE)) {
        advance(p); /* NEWLINE */
        expect(p, TOK_INDENT);

        /* Peek to see if it's variants (UPPER_IDENT) or fields (lower ident) */
        if (at(p, TOK_UPPER_IDENT)) {
            /* Enum / ADT */
            n->as.type_decl.body_kind = TYPE_BODY_ENUM;
            VariantDef *variants = NULL;
            while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
                skip_newlines(p);
                if (at(p, TOK_DEDENT)) break;
                Token *vname = expect(p, TOK_UPPER_IDENT);
                VariantDef v = {0};
                v.name = make_ident(vname);

                if (match(p, TOK_LPAREN)) {
                    /* Tuple variant */
                    FieldDef *fields = NULL;
                    while (!at(p, TOK_RPAREN) && !at(p, TOK_EOF)) {
                        FieldDef f = {0};
                        /* variant_field: [ident ":"] type_expr */
                        if (at(p, TOK_IDENT) && p->tokens[p->pos + 1].kind == TOK_COLON) {
                            Token *fname = advance(p);
                            advance(p); /* : */
                            f.name = make_ident(fname);
                            f.type = parse_type_expr(p);
                        } else {
                            f.type = parse_type_expr(p);
                        }
                        vec_push(fields, f);
                        if (!match(p, TOK_COMMA)) break;
                    }
                    expect(p, TOK_RPAREN);
                    v.field_count = (int)vec_len(fields);
                    v.fields = (FieldDef *)aalloc(p, v.field_count * sizeof(FieldDef));
                    memcpy(v.fields, fields, v.field_count * sizeof(FieldDef));
                    vec_free(fields);
                    v.is_record = false;
                } else if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
                    /* Record variant */
                    advance(p); /* NEWLINE */
                    expect(p, TOK_INDENT);
                    FieldDef *fields = NULL;
                    while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
                        skip_newlines(p);
                        if (at(p, TOK_DEDENT)) break;
                        vec_push(fields, parse_field_def(p));
                        skip_newlines(p);
                    }
                    expect(p, TOK_DEDENT);
                    v.field_count = (int)vec_len(fields);
                    v.fields = (FieldDef *)aalloc(p, v.field_count * sizeof(FieldDef));
                    memcpy(v.fields, fields, v.field_count * sizeof(FieldDef));
                    vec_free(fields);
                    v.is_record = true;
                }

                vec_push(variants, v);
                skip_newlines(p);
            }
            expect(p, TOK_DEDENT);
            n->as.type_decl.body.enumt.count = (int)vec_len(variants);
            n->as.type_decl.body.enumt.variants = (VariantDef *)aalloc(p,
                n->as.type_decl.body.enumt.count * sizeof(VariantDef));
            memcpy(n->as.type_decl.body.enumt.variants, variants,
                   n->as.type_decl.body.enumt.count * sizeof(VariantDef));
            vec_free(variants);
        } else {
            /* Record / struct */
            n->as.type_decl.body_kind = TYPE_BODY_RECORD;
            FieldDef *fields = NULL;
            while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
                skip_newlines(p);
                if (at(p, TOK_DEDENT)) break;
                vec_push(fields, parse_field_def(p));
                skip_newlines(p);
            }
            expect(p, TOK_DEDENT);
            n->as.type_decl.body.record.count = (int)vec_len(fields);
            n->as.type_decl.body.record.fields = (FieldDef *)aalloc(p,
                n->as.type_decl.body.record.count * sizeof(FieldDef));
            memcpy(n->as.type_decl.body.record.fields, fields,
                   n->as.type_decl.body.record.count * sizeof(FieldDef));
            vec_free(fields);
        }
    } else if (at(p, TOK_LBRACE)) {
        /* Inline record: Type { field: Type, ... } */
        advance(p);
        n->as.type_decl.body_kind = TYPE_BODY_RECORD;
        FieldDef *fields = NULL;
        while (!at(p, TOK_RBRACE) && !at(p, TOK_EOF)) {
            vec_push(fields, parse_field_def(p));
            if (!match(p, TOK_COMMA)) break;
        }
        expect(p, TOK_RBRACE);
        n->as.type_decl.body.record.count = (int)vec_len(fields);
        n->as.type_decl.body.record.fields = (FieldDef *)aalloc(p,
            n->as.type_decl.body.record.count * sizeof(FieldDef));
        memcpy(n->as.type_decl.body.record.fields, fields,
               n->as.type_decl.body.record.count * sizeof(FieldDef));
        vec_free(fields);
    }

    return n;
}

static AstNode *parse_trait_decl(Parser *p, Attribute *attrs, int attr_count) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_TRAIT);
    Token *name = expect(p, TOK_UPPER_IDENT);

    AstNode *n = ast_new(p->arena, NODE_TRAIT_DECL, loc);
    n->as.trait_decl.name = make_ident(name);
    n->attrs = attrs;
    n->attr_count = attr_count;

    if (at(p, TOK_LBRACKET)) {
        n->as.trait_decl.type_params = parse_type_params(p);
    }
    if (match(p, TOK_COLON)) {
        TypePath *cons = NULL;
        vec_push(cons, parse_type_path(p));
        while (match(p, TOK_PLUS)) {
            vec_push(cons, parse_type_path(p));
        }
        n->as.trait_decl.constraint_count = (int)vec_len(cons);
        n->as.trait_decl.constraints = (TypePath *)aalloc(p,
            n->as.trait_decl.constraint_count * sizeof(TypePath));
        memcpy(n->as.trait_decl.constraints, cons,
               n->as.trait_decl.constraint_count * sizeof(TypePath));
        vec_free(cons);
    }

    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);

    /* Parse trait body: fn signatures and fn declarations */
    AstNode **members = NULL;
    while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
        skip_newlines(p);
        if (at(p, TOK_DEDENT)) break;

        /* Collect any attributes */
        Attribute *fn_attrs = NULL;
        int fn_attr_count = 0;
        while (at(p, TOK_AT)) {
            Attribute a = parse_attribute_single(p);
            vec_push(fn_attrs, a);
            fn_attr_count++;
            skip_newlines(p);
        }

        SrcLoc fn_loc = cur_loc(p);
        expect(p, TOK_FN);
        Token *fn_name = expect(p, TOK_IDENT);

        TypeParamList tp = {0};
        if (at(p, TOK_LBRACKET)) tp = parse_type_params(p);

        expect(p, TOK_LPAREN);
        ParamList params = parse_param_list(p);
        expect(p, TOK_RPAREN);

        AstType *ret = NULL;
        if (match(p, TOK_ARROW)) ret = parse_type_expr(p);

        AstNode *fn = ast_new(p->arena, NODE_FN_DECL, fn_loc);
        fn->as.fn_decl.name = make_ident(fn_name);
        fn->as.fn_decl.type_params = tp;
        fn->as.fn_decl.params = params;
        fn->as.fn_decl.ret_type = ret;
        if (fn_attr_count > 0) {
            fn->attrs = (Attribute *)aalloc(p, fn_attr_count * sizeof(Attribute));
            memcpy(fn->attrs, fn_attrs, fn_attr_count * sizeof(Attribute));
            fn->attr_count = fn_attr_count;
        }
        vec_free(fn_attrs);

        /* Check if there's a body (default impl) or just signature */
        if (at(p, TOK_NEWLINE) && p->tokens[p->pos + 1].kind == TOK_INDENT) {
            advance(p); /* NEWLINE */
            expect(p, TOK_INDENT);
            fn->as.fn_decl.body = parse_block(p);
            expect(p, TOK_DEDENT);
            fn->as.fn_decl.is_signature = false;
        } else {
            fn->as.fn_decl.is_signature = true;
        }

        vec_push(members, fn);
        skip_newlines(p);
    }
    expect(p, TOK_DEDENT);

    n->as.trait_decl.members.count = (int)vec_len(members);
    n->as.trait_decl.members.items = ast_node_list(p->arena, members, n->as.trait_decl.members.count);
    vec_free(members);
    return n;
}

static AstNode *parse_fn_or_method(Parser *p, Attribute *attrs, int attr_count) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_FN);

    /* Check if this is a method: type_path "." ident */
    /* Method: UPPER_IDENT ("." UPPER_IDENT)* "." ident */
    if (at(p, TOK_UPPER_IDENT)) {
        /* Look ahead to see if it's TypePath.method_name */
        int save = p->pos;
        TypePath path = parse_type_path(p);
        if (match(p, TOK_DOT) && at(p, TOK_IDENT)) {
            /* It's a method */
            Token *method_name = advance(p);
            TypeParamList tp = {0};
            if (at(p, TOK_LBRACKET)) tp = parse_type_params(p);
            expect(p, TOK_LPAREN);
            ParamList params = parse_param_list(p);
            expect(p, TOK_RPAREN);
            AstType *ret = NULL;
            if (match(p, TOK_ARROW)) ret = parse_type_expr(p);
            expect(p, TOK_NEWLINE);
            expect(p, TOK_INDENT);
            NodeList body = parse_block(p);
            expect(p, TOK_DEDENT);

            AstNode *n = ast_new(p->arena, NODE_METHOD_DECL, loc);
            n->as.method_decl.type_path = path;
            n->as.method_decl.method_name = make_ident(method_name);
            n->as.method_decl.type_params = tp;
            n->as.method_decl.params = params;
            n->as.method_decl.ret_type = ret;
            n->as.method_decl.body = body;
            n->attrs = attrs;
            n->attr_count = attr_count;
            return n;
        }
        /* Not a method — backtrack */
        p->pos = save;
    }

    /* Regular function */
    Token *name = expect(p, TOK_IDENT);
    TypeParamList tp = {0};
    if (at(p, TOK_LBRACKET)) tp = parse_type_params(p);
    expect(p, TOK_LPAREN);
    ParamList params = parse_param_list(p);
    expect(p, TOK_RPAREN);
    AstType *ret = NULL;
    if (match(p, TOK_ARROW)) ret = parse_type_expr(p);
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);
    NodeList body = parse_block(p);
    expect(p, TOK_DEDENT);

    AstNode *n = ast_new(p->arena, NODE_FN_DECL, loc);
    n->as.fn_decl.name = make_ident(name);
    n->as.fn_decl.type_params = tp;
    n->as.fn_decl.params = params;
    n->as.fn_decl.ret_type = ret;
    n->as.fn_decl.body = body;
    n->as.fn_decl.is_signature = false;
    n->attrs = attrs;
    n->attr_count = attr_count;
    return n;
}

static AstNode *parse_const_decl(Parser *p, Attribute *attrs, int attr_count) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_CONST);
    Token *name = expect(p, TOK_UPPER_IDENT);
    AstType *type = NULL;
    if (match(p, TOK_COLON)) type = parse_type_expr(p);
    expect(p, TOK_ASSIGN);
    AstNode *value = parse_expr(p);

    AstNode *n = ast_new(p->arena, NODE_CONST_DECL, loc);
    n->as.const_decl.name = make_ident(name);
    n->as.const_decl.type = type;
    n->as.const_decl.value = value;
    n->attrs = attrs;
    n->attr_count = attr_count;
    return n;
}

static AstNode *parse_extern_block(Parser *p, Attribute *attrs, int attr_count) {
    SrcLoc loc = cur_loc(p);
    expect(p, TOK_EXTERN);
    Token *abi = expect(p, TOK_STRING);
    expect(p, TOK_NEWLINE);
    expect(p, TOK_INDENT);

    AstNode **decls = NULL;
    while (!at(p, TOK_DEDENT) && !at(p, TOK_EOF)) {
        skip_newlines(p);
        if (at(p, TOK_DEDENT)) break;

        SrcLoc fn_loc = cur_loc(p);
        expect(p, TOK_FN);
        Token *name = expect(p, TOK_IDENT);
        expect(p, TOK_LPAREN);

        ExternParam *params = NULL;
        if (!at(p, TOK_RPAREN)) {
            do {
                if (at(p, TOK_RPAREN)) break;
                ExternParam ep = {0};
                if (at(p, TOK_ELLIPSIS)) {
                    advance(p);
                    ep.is_variadic = true;
                } else if (at(p, TOK_IDENT) && p->tokens[p->pos + 1].kind == TOK_COLON) {
                    Token *pname = advance(p);
                    advance(p); /* : */
                    ep.name = (Ident *)aalloc(p, sizeof(Ident));
                    *ep.name = make_ident(pname);
                    ep.type = parse_type_expr(p);
                } else {
                    ep.type = parse_type_expr(p);
                }
                vec_push(params, ep);
            } while (match(p, TOK_COMMA));
        }
        expect(p, TOK_RPAREN);

        AstType *ret = NULL;
        if (match(p, TOK_ARROW)) ret = parse_type_expr(p);

        AstNode *efn = ast_new(p->arena, NODE_EXTERN_FN, fn_loc);
        efn->as.extern_fn.name = make_ident(name);
        efn->as.extern_fn.param_count = (int)vec_len(params);
        efn->as.extern_fn.params = (ExternParam *)aalloc(p,
            efn->as.extern_fn.param_count * sizeof(ExternParam));
        if (efn->as.extern_fn.param_count > 0) {
            memcpy(efn->as.extern_fn.params, params,
                   efn->as.extern_fn.param_count * sizeof(ExternParam));
        }
        vec_free(params);
        efn->as.extern_fn.ret_type = ret;
        vec_push(decls, efn);

        skip_newlines(p);
    }
    expect(p, TOK_DEDENT);

    AstNode *n = ast_new(p->arena, NODE_EXTERN_BLOCK, loc);
    /* Store ABI string without quotes */
    n->as.extern_block.abi = abi->start + 1;
    n->as.extern_block.abi_len = abi->len - 2;
    n->as.extern_block.decls.count = (int)vec_len(decls);
    n->as.extern_block.decls.items = ast_node_list(p->arena, decls, n->as.extern_block.decls.count);
    vec_free(decls);
    n->attrs = attrs;
    n->attr_count = attr_count;
    return n;
}

static AstNode *parse_top_level_item(Parser *p) {
    /* Collect attributes */
    Attribute *attrs = NULL;
    int attr_count = 0;
    while (at(p, TOK_AT)) {
        Attribute a = parse_attribute_single(p);
        vec_push(attrs, a);
        attr_count++;
        skip_newlines(p);
    }

    Attribute *arena_attrs = NULL;
    if (attr_count > 0) {
        arena_attrs = (Attribute *)aalloc(p, attr_count * sizeof(Attribute));
        memcpy(arena_attrs, attrs, attr_count * sizeof(Attribute));
    }
    vec_free(attrs);

    if (at(p, TOK_IMPORT))  return parse_import(p);
    if (at(p, TOK_TYPE))    return parse_type_decl(p, arena_attrs, attr_count);
    if (at(p, TOK_TRAIT))   return parse_trait_decl(p, arena_attrs, attr_count);
    if (at(p, TOK_FN))      return parse_fn_or_method(p, arena_attrs, attr_count);
    if (at(p, TOK_CONST))   return parse_const_decl(p, arena_attrs, attr_count);
    if (at(p, TOK_EXTERN))  return parse_extern_block(p, arena_attrs, attr_count);

    p->errors++;
    error_at(cur_loc(p), "expected top-level declaration, got '%s'",
             token_kind_str(peek(p)->kind));
    advance(p);
    return ast_new(p->arena, NODE_LITERAL, cur_loc(p)); /* dummy */
}

AstNode *parse_program(Parser *p) {
    SrcLoc loc = cur_loc(p);
    AstNode **items = NULL;

    skip_newlines(p);
    while (!at(p, TOK_EOF)) {
        AstNode *item = parse_top_level_item(p);
        vec_push(items, item);
        skip_newlines(p);
    }

    AstNode *prog = ast_new(p->arena, NODE_PROGRAM, loc);
    prog->as.program.count = (int)vec_len(items);
    prog->as.program.items = ast_node_list(p->arena, items, prog->as.program.count);
    vec_free(items);
    return prog;
}
