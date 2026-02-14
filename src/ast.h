#ifndef LUX_AST_H
#define LUX_AST_H

#include "arena.h"
#include "error.h"
#include "token.h"
#include <stdbool.h>

/* Forward declarations */
typedef struct AstNode AstNode;
typedef struct AstType AstType;
typedef struct AstPattern AstPattern;
typedef struct Type Type;

/* ===== Node Lists ===== */
typedef struct { AstNode **items; int count; } NodeList;
typedef struct { AstType **items; int count; } TypeList;
typedef struct { AstPattern **items; int count; } PatternList;

/* Elif clause (needs NodeList defined first) */
typedef struct {
    AstNode *cond;
    NodeList block;
} ElifClause;

/* ===== Identifier ===== */
typedef struct {
    const char *name;
    int len;
    SrcLoc loc;
} Ident;

/* ===== Type Expressions ===== */
typedef enum {
    TYPE_PATH,       /* Foo, Foo.Bar */
    TYPE_FN,         /* fn(A, B) -> C */
    TYPE_TUPLE,      /* (A, B, C) */
    TYPE_LIST,       /* [T] */
    TYPE_MAP,        /* {K: V} */
    TYPE_UNION,      /* A | B | C */
    TYPE_SELF,       /* Self */
    TYPE_POINTER,    /* *T */
} AstTypeKind;

typedef struct {
    Ident *segments;   /* e.g. Foo.Bar = [Foo, Bar] */
    int seg_count;
} TypePath;

struct AstType {
    AstTypeKind kind;
    SrcLoc loc;
    union {
        struct { TypePath path; TypeList args; } path;   /* TYPE_PATH */
        struct { TypeList params; AstType *ret; } fn;    /* TYPE_FN */
        TypeList tuple;                                   /* TYPE_TUPLE */
        AstType *list_elem;                              /* TYPE_LIST */
        struct { AstType *key; AstType *val; } map;      /* TYPE_MAP */
        TypeList union_types;                             /* TYPE_UNION */
        AstType *pointer_elem;                           /* TYPE_POINTER */
    } as;
};

/* ===== Type Parameters ===== */
typedef struct {
    Ident name;
    TypePath *constraints;   /* Trait1 + Trait2 */
    int constraint_count;
} TypeParam;

typedef struct { TypeParam *items; int count; } TypeParamList;

/* ===== Patterns ===== */
typedef enum {
    PAT_WILDCARD,     /* _ */
    PAT_BIND,         /* x */
    PAT_LITERAL,      /* 42, "hello", true */
    PAT_VARIANT,      /* Some(x) */
    PAT_TUPLE,        /* (a, b, c) */
    PAT_LIST,         /* [a, b, c] */
    PAT_LIST_REST,    /* [a, b, ..rest] */
    PAT_RECORD,       /* { x: pat, y } */
    PAT_OR,           /* a | b | c */
    PAT_BIND_AS,      /* name @ pattern */
    PAT_REST,         /* ..name */
} AstPatternKind;

typedef struct {
    Ident name;
    AstPattern *pattern;   /* NULL for shorthand */
} FieldPattern;

struct AstPattern {
    AstPatternKind kind;
    SrcLoc loc;
    union {
        Ident bind;                                       /* PAT_BIND */
        AstNode *literal;                                 /* PAT_LITERAL */
        struct { TypePath path; PatternList args; } variant; /* PAT_VARIANT */
        PatternList tuple;                                /* PAT_TUPLE */
        PatternList list;                                 /* PAT_LIST */
        struct { PatternList elems; Ident *rest_name; } list_rest; /* PAT_LIST_REST */
        struct { FieldPattern *fields; int count; } record; /* PAT_RECORD */
        PatternList or_pats;                              /* PAT_OR */
        struct { Ident name; AstPattern *pattern; } bind_as; /* PAT_BIND_AS */
        Ident *rest_name;                                 /* PAT_REST */
    } as;
};

/* ===== AST Nodes ===== */
typedef enum {
    /* Top-level */
    NODE_PROGRAM,
    NODE_IMPORT,
    NODE_TYPE_DECL,
    NODE_TRAIT_DECL,
    NODE_FN_DECL,
    NODE_METHOD_DECL,
    NODE_CONST_DECL,
    NODE_EXTERN_BLOCK,
    NODE_EXTERN_FN,

    /* Statements */
    NODE_LET,
    NODE_VAR,
    NODE_ASSIGN,
    NODE_RETURN,
    NODE_BREAK,
    NODE_CONTINUE,
    NODE_DEFER,
    NODE_ASSERT,
    NODE_EXPR_STMT,

    /* Expressions */
    NODE_BINARY,
    NODE_UNARY,
    NODE_CALL,
    NODE_FIELD_ACCESS,
    NODE_INDEX,
    NODE_TRY,         /* ? */
    NODE_MUTATE,       /* ! */
    NODE_PIPE,
    NODE_IF,
    NODE_MATCH,
    NODE_FOR,
    NODE_WHILE,
    NODE_LOOP,
    NODE_SPAWN,
    NODE_AWAIT,
    NODE_SCOPE,
    NODE_COMPTIME,
    NODE_LAMBDA,
    NODE_BLOCK,
    NODE_DO_BLOCK,
    NODE_LITERAL,
    NODE_IDENT,
    NODE_UPPER_IDENT,
    NODE_TUPLE,
    NODE_LIST,
    NODE_LIST_COMP,
    NODE_MAP,
    NODE_MAP_COMP,
    NODE_STRING_INTERP,
    NODE_WITH,
    NODE_RANGE,
    NODE_ATTRIBUTE,
    NODE_PLACEHOLDER,  /* _ in pipe args */
} AstNodeKind;

typedef enum {
    LIT_INT,
    LIT_FLOAT,
    LIT_STRING,
    LIT_CHAR,
    LIT_BOOL,
    LIT_NONE,
} LiteralKind;

/* Function parameter */
typedef struct {
    AstPattern *pattern;  /* NULL for 'self' */
    AstType *type;        /* NULL for untyped (lambda params) */
    AstNode *default_val; /* NULL if no default */
    bool is_self;
} Param;

typedef struct { Param *items; int count; } ParamList;

/* Call argument */
typedef struct {
    Ident *name;     /* NULL for positional */
    AstNode *value;
} Arg;

typedef struct { Arg *items; int count; } ArgList;

/* Map entry */
typedef struct {
    AstNode *key;    /* NULL for shorthand or spread */
    AstNode *value;
    bool is_spread;
} MapEntry;

/* Match arm */
typedef struct {
    AstPattern *pattern;
    AstNode *guard;      /* NULL if no guard */
    AstNode *body;
} MatchArm;

/* Field definition (in type decls) */
typedef struct {
    Ident name;
    AstType *type;
    AstNode *default_val;
} FieldDef;

/* Variant definition */
typedef struct {
    Ident name;
    /* Payload can be positional fields or record fields */
    FieldDef *fields;
    int field_count;
    bool is_record;     /* true = record variant, false = tuple variant */
} VariantDef;

/* Import name (with optional alias) */
typedef struct {
    Ident name;
    Ident *alias;       /* NULL if no alias */
} ImportName;

/* Field update in 'with' expression */
typedef struct {
    Ident name;
    AstNode *value;     /* NULL for shorthand */
} FieldUpdate;

/* Attribute */
typedef struct {
    Ident name;
    Arg *args;
    int arg_count;
} Attribute;

/* Extern parameter */
typedef struct {
    Ident *name;         /* NULL if unnamed */
    AstType *type;       /* NULL for variadic (...) */
    bool is_variadic;
} ExternParam;

struct AstNode {
    AstNodeKind kind;
    SrcLoc loc;
    Attribute *attrs;
    int attr_count;
    Type *checked_type;  /* Set by typechecker, NULL after parsing */
    union {
        /* NODE_PROGRAM */
        NodeList program;

        /* NODE_IMPORT */
        struct {
            Ident *path;
            int path_count;
            ImportName *names;   /* NULL for no select, or list */
            int name_count;
            bool is_glob;        /* import foo.* */
        } import;

        /* NODE_TYPE_DECL */
        struct {
            Ident name;
            TypeParamList type_params;
            enum { TYPE_BODY_RECORD, TYPE_BODY_ENUM, TYPE_BODY_ALIAS } body_kind;
            union {
                struct { FieldDef *fields; int count; } record;
                struct { VariantDef *variants; int count; } enumt;
                AstType *alias;
            } body;
        } type_decl;

        /* NODE_TRAIT_DECL */
        struct {
            Ident name;
            TypeParamList type_params;
            TypePath *constraints;
            int constraint_count;
            NodeList members;   /* fn_decl or fn signature nodes */
        } trait_decl;

        /* NODE_FN_DECL */
        struct {
            Ident name;
            TypeParamList type_params;
            ParamList params;
            AstType *ret_type;   /* NULL if no return type */
            NodeList body;       /* block statements */
            bool is_signature;   /* true = no body (trait member) */
        } fn_decl;

        /* NODE_METHOD_DECL */
        struct {
            TypePath type_path;
            Ident method_name;
            TypeParamList type_params;
            ParamList params;
            AstType *ret_type;
            NodeList body;
        } method_decl;

        /* NODE_CONST_DECL */
        struct {
            Ident name;
            AstType *type;
            AstNode *value;
        } const_decl;

        /* NODE_EXTERN_BLOCK */
        struct {
            const char *abi;
            int abi_len;
            NodeList decls;
        } extern_block;

        /* NODE_EXTERN_FN */
        struct {
            Ident name;
            ExternParam *params;
            int param_count;
            AstType *ret_type;
        } extern_fn;

        /* NODE_LET, NODE_VAR */
        struct {
            AstPattern *pattern;  /* let uses pattern, var uses ident (stored as bind pat) */
            AstType *type;
            AstNode *value;
        } let;

        /* NODE_ASSIGN */
        struct {
            AstNode *target;
            TokenKind op;
            AstNode *value;
        } assign;

        /* NODE_RETURN, NODE_AWAIT, NODE_SPAWN, NODE_COMPTIME, NODE_SCOPE */
        struct {
            AstNode *value;    /* may be NULL */
        } unary_stmt;

        /* NODE_BREAK */
        struct {
            Ident *label;
            AstNode *value;
        } break_stmt;

        /* NODE_CONTINUE */
        struct {
            Ident *label;
        } continue_stmt;

        /* NODE_DEFER */
        struct {
            AstNode *body;     /* expression or block */
        } defer;

        /* NODE_ASSERT */
        struct {
            AstNode *cond;
            AstNode *message;  /* NULL if no message */
        } assert_stmt;

        /* NODE_BINARY */
        struct {
            TokenKind op;
            AstNode *left;
            AstNode *right;
        } binary;

        /* NODE_UNARY */
        struct {
            TokenKind op;
            AstNode *operand;
        } unary;

        /* NODE_CALL */
        struct {
            AstNode *callee;
            ArgList args;
        } call;

        /* NODE_FIELD_ACCESS */
        struct {
            AstNode *object;
            Ident field;
        } field_access;

        /* NODE_INDEX */
        struct {
            AstNode *object;
            AstNode *index;
        } index;

        /* NODE_TRY, NODE_MUTATE */
        struct {
            AstNode *operand;
        } postfix;

        /* NODE_PIPE */
        struct {
            AstNode *left;
            AstNode *right;    /* the pipe target (call/lambda) */
        } pipe;

        /* NODE_IF */
        struct {
            AstNode *cond;
            NodeList then_block;
            ElifClause *elifs;
            int elif_count;
            NodeList else_block;   /* count=0 if no else */
            bool is_inline;        /* if x then a else b */
            AstNode *then_expr;    /* for inline if */
            AstNode *else_expr;    /* for inline if */
        } if_expr;

        /* NODE_MATCH */
        struct {
            AstNode *subject;
            MatchArm *arms;
            int arm_count;
        } match;

        /* NODE_FOR */
        struct {
            Ident *label;
            AstPattern *pattern;
            AstNode *iter;
            NodeList body;
        } for_expr;

        /* NODE_WHILE */
        struct {
            Ident *label;
            AstNode *cond;
            NodeList body;
        } while_expr;

        /* NODE_LOOP */
        struct {
            Ident *label;
            NodeList body;
        } loop_expr;

        /* NODE_LAMBDA */
        struct {
            ParamList params;
            AstNode *body_expr;   /* single-expr body, or NULL */
            NodeList body_block;  /* block body, count=0 if expr */
        } lambda;

        /* NODE_BLOCK, NODE_DO_BLOCK */
        NodeList block;

        /* NODE_LITERAL */
        struct {
            LiteralKind kind;
            const char *value;
            int len;
        } literal;

        /* NODE_IDENT */
        Ident ident;

        /* NODE_UPPER_IDENT */
        TypePath type_path;

        /* NODE_TUPLE, NODE_LIST */
        NodeList list;

        /* NODE_LIST_COMP */
        struct {
            AstNode *expr;
            AstPattern *pattern;
            AstNode *iter;
            AstNode *filter;     /* NULL if no filter */
        } list_comp;

        /* NODE_MAP */
        struct {
            MapEntry *entries;
            int count;
        } map;

        /* NODE_MAP_COMP */
        struct {
            AstNode *key_expr;
            AstNode *val_expr;
            AstPattern *pattern;
            AstNode *iter;
            AstNode *filter;
        } map_comp;

        /* NODE_WITH */
        struct {
            AstNode *base;
            FieldUpdate *updates;
            int update_count;
        } with;

        /* NODE_RANGE */
        struct {
            AstNode *start;
            AstNode *end;
            bool inclusive;
        } range;

        /* NODE_EXPR_STMT */
        struct {
            AstNode *expr;
        } expr_stmt;

        /* NODE_ATTRIBUTE */
        Attribute attribute;
    } as;
};

/* ===== Constructors ===== */
AstNode *ast_new(Arena *a, AstNodeKind kind, SrcLoc loc);
AstType *ast_new_type(Arena *a, AstTypeKind kind, SrcLoc loc);
AstPattern *ast_new_pattern(Arena *a, AstPatternKind kind, SrcLoc loc);

/* Arena-allocated arrays */
AstNode **ast_node_list(Arena *a, AstNode **vec, int count);
AstType **ast_type_list(Arena *a, AstType **vec, int count);
AstPattern **ast_pattern_list(Arena *a, AstPattern **vec, int count);

/* ===== Pretty Printer ===== */
void ast_print(AstNode *node, int indent);
void ast_print_type(AstType *type);
void ast_print_pattern(AstPattern *pat);

#endif
