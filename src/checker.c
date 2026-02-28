#include "checker.h"
#include "vec.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ===== Error reporting ===== */

static void check_error(Checker *c, SrcLoc loc, const char *fmt, ...) {
    c->errors++;
    fprintf(stderr, "%s:%d:%d: error: ", loc.file, loc.line, loc.col);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
}

/* ===== Forward declarations ===== */

static Type *synth_expr(Checker *c, AstNode *node);
static void check_stmt(Checker *c, AstNode *node);
static void check_block(Checker *c, NodeList block);
static Type *resolve_ast_type(Checker *c, AstType *at);
static void define_pattern_binding(Checker *c, AstPattern *pat, Type *type, bool is_mutable);
static void check_pattern(Checker *c, AstPattern *pat, Type *expected_type);

/* ===== Unification ===== */

static bool unify(Checker *c, Type *a, Type *b, SrcLoc loc) {
    a = type_resolve(a);
    b = type_resolve(b);
    if (!a || !b) return false;
    if (a == b) return true;
    if (a->kind == TY_ERROR || b->kind == TY_ERROR) return true;
    if (a->kind == TY_NEVER) return true;
    if (b->kind == TY_NEVER) return true;

    /* Type variable binding */
    if (a->kind == TY_TYPEVAR) {
        a->as.typevar.resolved = b;
        return true;
    }
    if (b->kind == TY_TYPEVAR) {
        b->as.typevar.resolved = a;
        return true;
    }

    /* TY_PARAM matches anything (lenient checking for generic function bodies) */
    if (a->kind == TY_PARAM || b->kind == TY_PARAM) return true;

    if (type_equals(a, b)) return true;

    /* String literals are compatible with *Char (C strings) */
    if ((a->kind == TY_PTR && a->as.ptr.elem->kind == TY_CHAR && b->kind == TY_STRING) ||
        (b->kind == TY_PTR && b->as.ptr.elem->kind == TY_CHAR && a->kind == TY_STRING)) {
        return true;
    }

    check_error(c, loc, "type mismatch: expected '%s', got '%s'",
                type_to_string(a), type_to_string(b));
    return false;
}

/* ===== Type parameter substitution for generics ===== */

typedef struct { const char *name; Type *concrete; } TypeSubst;

static Type *type_substitute(Arena *a, Type *t, TypeSubst *substs, int count) {
    t = type_resolve(t);
    if (!t) return NULL;

    if (t->kind == TY_PARAM) {
        for (int i = 0; i < count; i++) {
            if (t->as.param.name == substs[i].name) return substs[i].concrete;
        }
        return t;
    }

    if (t->kind == TY_FN) {
        Type **params = (Type **)arena_alloc(a, t->as.fn.param_count * sizeof(Type *));
        for (int i = 0; i < t->as.fn.param_count; i++)
            params[i] = type_substitute(a, t->as.fn.param_types[i], substs, count);
        Type *ret = type_substitute(a, t->as.fn.ret, substs, count);
        return type_fn(a, params, t->as.fn.param_count, ret, t->as.fn.is_variadic);
    }

    if (t->kind == TY_TUPLE) {
        Type **elems = (Type **)arena_alloc(a, t->as.tuple.count * sizeof(Type *));
        for (int i = 0; i < t->as.tuple.count; i++)
            elems[i] = type_substitute(a, t->as.tuple.elems[i], substs, count);
        return type_tuple(a, elems, t->as.tuple.count);
    }

    if (t->kind == TY_PTR)
        return type_ptr(a, type_substitute(a, t->as.ptr.elem, substs, count));
    if (t->kind == TY_LIST)
        return type_list(a, type_substitute(a, t->as.list.elem, substs, count));
    if (t->kind == TY_MAP)
        return type_map(a, type_substitute(a, t->as.map.key, substs, count),
                        type_substitute(a, t->as.map.val, substs, count));

    return t; /* primitives, records, enums — no substitution needed */
}

/* Infer type substitutions from argument types matching against generic parameter types */
static bool infer_type_params(Type *param_type, Type *arg_type,
                              TypeSubst *substs, int count) {
    param_type = type_resolve(param_type);
    arg_type = type_resolve(arg_type);
    if (!param_type || !arg_type) return false;

    if (param_type->kind == TY_PARAM) {
        for (int i = 0; i < count; i++) {
            if (param_type->as.param.name == substs[i].name) {
                if (!substs[i].concrete) {
                    substs[i].concrete = arg_type;
                }
                return true;
            }
        }
        return false;
    }

    if (param_type->kind == TY_PTR && arg_type->kind == TY_PTR)
        return infer_type_params(param_type->as.ptr.elem, arg_type->as.ptr.elem, substs, count);
    if (param_type->kind == TY_LIST && arg_type->kind == TY_LIST)
        return infer_type_params(param_type->as.list.elem, arg_type->as.list.elem, substs, count);
    if (param_type->kind == TY_TUPLE && arg_type->kind == TY_TUPLE &&
        param_type->as.tuple.count == arg_type->as.tuple.count) {
        for (int i = 0; i < param_type->as.tuple.count; i++)
            infer_type_params(param_type->as.tuple.elems[i], arg_type->as.tuple.elems[i], substs, count);
        return true;
    }

    return true;
}

/* ===== Resolve AST type expressions to semantic types ===== */

static Type *resolve_ast_type(Checker *c, AstType *at) {
    if (!at) return type_none();

    switch (at->kind) {
    case TYPE_PATH: {
        if (at->as.path.path.seg_count == 1) {
            const char *raw_name = at->as.path.path.segments[0].name;
            int len = at->as.path.path.segments[0].len;
            /* Intern the name for pointer-equality comparison */
            const char *name = str_intern_range(c->interns, raw_name, len);

            if (name == c->int_name) return type_int();
            if (name == c->float_name) return type_float();
            if (name == c->bool_name) return type_bool();
            if (name == c->char_name) return type_char();
            if (name == c->string_name) return type_string();
            if (name == c->none_name) return type_none();

            /* Look up in symbol table */
            Symbol *sym = symtab_lookup(&c->symtab, name);
            if (sym && sym->kind == SYM_TYPE) return sym->type;

            check_error(c, at->loc, "unknown type '%.*s'", len, raw_name);
            return type_error();
        }
        check_error(c, at->loc, "qualified type paths not yet supported");
        return type_error();
    }
    case TYPE_FN: {
        int n = at->as.fn.params.count;
        Type **params = NULL;
        if (n > 0) {
            params = (Type **)arena_alloc(c->arena, n * sizeof(Type *));
            for (int i = 0; i < n; i++) {
                params[i] = resolve_ast_type(c, at->as.fn.params.items[i]);
            }
        }
        Type *ret = resolve_ast_type(c, at->as.fn.ret);
        return type_fn(c->arena, params, n, ret, false);
    }
    case TYPE_TUPLE: {
        int n = at->as.tuple.count;
        Type **elems = (Type **)arena_alloc(c->arena, n * sizeof(Type *));
        for (int i = 0; i < n; i++) {
            elems[i] = resolve_ast_type(c, at->as.tuple.items[i]);
        }
        return type_tuple(c->arena, elems, n);
    }
    case TYPE_POINTER: {
        Type *elem = resolve_ast_type(c, at->as.pointer_elem);
        return type_ptr(c->arena, elem);
    }
    case TYPE_LIST: {
        Type *elem = resolve_ast_type(c, at->as.list_elem);
        return type_list(c->arena, elem);
    }
    case TYPE_MAP: {
        Type *key = resolve_ast_type(c, at->as.map.key);
        Type *val = resolve_ast_type(c, at->as.map.val);
        return type_map(c->arena, key, val);
    }
    default:
        check_error(c, at->loc, "unsupported type expression");
        return type_error();
    }
}

/* ===== Expression synthesis (bottom-up type inference) ===== */

static Type *synth_expr(Checker *c, AstNode *node) {
    if (!node) return type_none();
    Type *result = NULL;

    switch (node->kind) {
    case NODE_LITERAL:
        switch (node->as.literal.kind) {
        case LIT_INT:    result = type_int(); break;
        case LIT_FLOAT:  result = type_float(); break;
        case LIT_BOOL:   result = type_bool(); break;
        case LIT_CHAR:   result = type_char(); break;
        case LIT_STRING: result = type_string(); break;
        case LIT_NONE:   result = type_none(); break;
        }
        break;

    case NODE_IDENT: {
        const char *id_name = str_intern_range(c->interns, node->as.ident.name,
                                               node->as.ident.len);
        Symbol *sym = symtab_lookup(&c->symtab, id_name);
        if (!sym) {
            check_error(c, node->loc, "undefined variable '%.*s'",
                        node->as.ident.len, node->as.ident.name);
            result = type_error();
        } else {
            result = sym->type;
        }
        break;
    }

    case NODE_BINARY: {
        Type *left = synth_expr(c, node->as.binary.left);
        Type *right = synth_expr(c, node->as.binary.right);

        switch (node->as.binary.op) {
        case TOK_PLUS: case TOK_MINUS: case TOK_STAR:
        case TOK_SLASH: case TOK_PERCENT:
            /* Arithmetic: both numeric, same type (TY_PARAM allowed for generics) */
            if (!type_is_numeric(left) && left->kind != TY_ERROR && left->kind != TY_PARAM) {
                check_error(c, node->loc, "arithmetic on non-numeric type '%s'",
                            type_to_string(left));
            }
            unify(c, left, right, node->loc);
            result = type_resolve(left);
            break;

        case TOK_EQ: case TOK_NEQ:
        case TOK_LT: case TOK_GT: case TOK_LE: case TOK_GE:
            unify(c, left, right, node->loc);
            result = type_bool();
            break;

        case TOK_AND: case TOK_OR:
            unify(c, left, type_bool(), node->loc);
            unify(c, right, type_bool(), node->loc);
            result = type_bool();
            break;

        case TOK_AMP: case TOK_PIPE: case TOK_CARET:
        case TOK_SHL: case TOK_SHR:
            if (type_resolve(left)->kind != TY_INT && left->kind != TY_ERROR && left->kind != TY_PARAM) {
                check_error(c, node->loc, "bitwise operation on non-integer type '%s'",
                            type_to_string(left));
            }
            unify(c, left, right, node->loc);
            result = type_int();
            break;

        default:
            result = type_error();
            break;
        }
        break;
    }

    case NODE_UNARY: {
        Type *operand = synth_expr(c, node->as.unary.operand);
        switch (node->as.unary.op) {
        case TOK_MINUS:
            if (!type_is_numeric(operand) && operand->kind != TY_ERROR && operand->kind != TY_PARAM) {
                check_error(c, node->loc, "negation on non-numeric type '%s'",
                            type_to_string(operand));
            }
            result = operand;
            break;
        case TOK_NOT:
            unify(c, operand, type_bool(), node->loc);
            result = type_bool();
            break;
        case TOK_TILDE:
            if (type_resolve(operand)->kind != TY_INT && operand->kind != TY_ERROR && operand->kind != TY_PARAM) {
                check_error(c, node->loc, "bitwise NOT on non-integer type '%s'",
                            type_to_string(operand));
            }
            result = type_int();
            break;
        default:
            result = operand;
            break;
        }
        break;
    }

    case NODE_CALL: {
        /* Check if callee is a type name (record constructor) */
        const char *cname = NULL;
        if (node->as.call.callee->kind == NODE_IDENT) {
            cname = str_intern_range(c->interns,
                node->as.call.callee->as.ident.name,
                node->as.call.callee->as.ident.len);
        } else if (node->as.call.callee->kind == NODE_UPPER_IDENT &&
                   node->as.call.callee->as.type_path.seg_count >= 1) {
            cname = str_intern_range(c->interns,
                node->as.call.callee->as.type_path.segments[0].name,
                node->as.call.callee->as.type_path.segments[0].len);
        }
        if (cname) {
            Symbol *sym = symtab_lookup(&c->symtab, cname);
            if (sym && sym->kind == SYM_TYPE && sym->type->kind == TY_RECORD) {
                Type *rec_type = sym->type;
                int expected = rec_type->as.record.field_count;
                int got = node->as.call.args.count;

                if (got != expected) {
                    check_error(c, node->loc, "record '%s' has %d fields, got %d arguments",
                                type_to_string(rec_type), expected, got);
                }
                int n = got < expected ? got : expected;
                for (int i = 0; i < n; i++) {
                    Type *arg_type = synth_expr(c, node->as.call.args.items[i].value);
                    unify(c, rec_type->as.record.field_types[i], arg_type,
                          node->as.call.args.items[i].value->loc);
                }
                node->as.call.callee->checked_type = rec_type;
                result = rec_type;
                break;
            }
        }

        Type *callee_type = synth_expr(c, node->as.call.callee);
        callee_type = type_resolve(callee_type);

        if (callee_type->kind == TY_ERROR) {
            /* Synthesize arg types anyway to catch nested errors */
            for (int i = 0; i < node->as.call.args.count; i++) {
                synth_expr(c, node->as.call.args.items[i].value);
            }
            result = type_error();
            break;
        }

        if (callee_type->kind != TY_FN) {
            check_error(c, node->loc, "cannot call non-function type '%s'",
                        type_to_string(callee_type));
            result = type_error();
            break;
        }

        /* For method calls (callee is NODE_FIELD_ACCESS on a type with a method), skip self parameter */
        bool is_method_call = false;
        if (node->as.call.callee->kind == NODE_FIELD_ACCESS) {
            Type *fa_obj_type = node->as.call.callee->as.field_access.object->checked_type;
            fa_obj_type = fa_obj_type ? type_resolve(fa_obj_type) : NULL;
            if (fa_obj_type && fa_obj_type->kind == TY_PTR)
                fa_obj_type = type_resolve(fa_obj_type->as.ptr.elem);
            const char *fa_fname = str_intern_range(c->interns,
                node->as.call.callee->as.field_access.field.name,
                node->as.call.callee->as.field_access.field.len);
            const char *tname = NULL;
            if (fa_obj_type && fa_obj_type->kind == TY_RECORD) tname = fa_obj_type->as.record.name;
            else if (fa_obj_type && fa_obj_type->kind == TY_ENUM) tname = fa_obj_type->as.enumt.name;
            if (tname) {
                char mangled[256];
                snprintf(mangled, sizeof(mangled), "%s_%s", tname, fa_fname);
                const char *interned = str_intern(c->interns, mangled);
                Symbol *msym = symtab_lookup(&c->symtab, interned);
                if (msym && msym->kind == SYM_FN) is_method_call = true;
            }
        }
        int param_offset = is_method_call ? 1 : 0;
        int expected = callee_type->as.fn.param_count - param_offset;
        int got = node->as.call.args.count;

        if (callee_type->as.fn.is_variadic) {
            if (got < expected) {
                check_error(c, node->loc, "too few arguments: expected at least %d, got %d",
                            expected, got);
            }
        } else {
            if (got != expected) {
                check_error(c, node->loc, "wrong number of arguments: expected %d, got %d",
                            expected, got);
            }
        }

        /* Detect generic function — check if any param type is TY_PARAM */
        bool is_generic_call = false;
        for (int i = 0; i < callee_type->as.fn.param_count; i++) {
            Type *pt = type_resolve(callee_type->as.fn.param_types[i]);
            if (pt->kind == TY_PARAM) { is_generic_call = true; break; }
        }
        if (type_resolve(callee_type->as.fn.ret)->kind == TY_PARAM) is_generic_call = true;

        /* Check each argument type and infer generics */
        int check_count = got < expected ? got : expected;
        TypeSubst *substs = NULL;
        int subst_count = 0;

        if (is_generic_call) {
            /* Collect unique type param names */
            for (int i = 0; i < callee_type->as.fn.param_count; i++) {
                Type *pt = type_resolve(callee_type->as.fn.param_types[i]);
                if (pt->kind == TY_PARAM) {
                    bool found = false;
                    for (int j = 0; j < subst_count; j++) {
                        if (substs[j].name == pt->as.param.name) { found = true; break; }
                    }
                    if (!found) {
                        substs = (TypeSubst *)realloc(substs, (subst_count + 1) * sizeof(TypeSubst));
                        substs[subst_count].name = pt->as.param.name;
                        substs[subst_count].concrete = NULL;
                        subst_count++;
                    }
                }
            }
            /* Also check return type */
            Type *ret = type_resolve(callee_type->as.fn.ret);
            if (ret->kind == TY_PARAM) {
                bool found = false;
                for (int j = 0; j < subst_count; j++) {
                    if (substs[j].name == ret->as.param.name) { found = true; break; }
                }
                if (!found) {
                    substs = (TypeSubst *)realloc(substs, (subst_count + 1) * sizeof(TypeSubst));
                    substs[subst_count].name = ret->as.param.name;
                    substs[subst_count].concrete = NULL;
                    subst_count++;
                }
            }

            /* Infer from arguments */
            for (int i = 0; i < check_count; i++) {
                Type *arg_type = synth_expr(c, node->as.call.args.items[i].value);
                infer_type_params(callee_type->as.fn.param_types[i + param_offset],
                                  arg_type, substs, subst_count);
            }

            /* Substitute and check */
            Type *specialized = type_substitute(c->arena, callee_type, substs, subst_count);
            for (int i = 0; i < check_count; i++) {
                Type *arg_type = node->as.call.args.items[i].value->checked_type;
                if (arg_type) {
                    Type *expected_t = type_resolve(specialized->as.fn.param_types[i + param_offset]);
                    unify(c, expected_t, type_resolve(arg_type), node->as.call.args.items[i].value->loc);
                }
            }

            result = type_resolve(specialized->as.fn.ret);
            free(substs);
        } else {
            for (int i = 0; i < check_count; i++) {
                Type *arg_type = synth_expr(c, node->as.call.args.items[i].value);
                unify(c, callee_type->as.fn.param_types[i + param_offset], arg_type, node->as.call.args.items[i].value->loc);
            }
            result = callee_type->as.fn.ret;
        }

        /* Synthesize remaining variadic args */
        for (int i = check_count; i < got; i++) {
            synth_expr(c, node->as.call.args.items[i].value);
        }
        break;
    }

    case NODE_IF: {
        if (node->as.if_expr.is_inline) {
            Type *cond_type = synth_expr(c, node->as.if_expr.cond);
            unify(c, cond_type, type_bool(), node->as.if_expr.cond->loc);
            Type *then_type = synth_expr(c, node->as.if_expr.then_expr);
            Type *else_type = synth_expr(c, node->as.if_expr.else_expr);
            unify(c, then_type, else_type, node->loc);
            result = type_resolve(then_type);
        } else {
            Type *cond_type = synth_expr(c, node->as.if_expr.cond);
            unify(c, cond_type, type_bool(), node->as.if_expr.cond->loc);
            check_block(c, node->as.if_expr.then_block);
            for (int i = 0; i < node->as.if_expr.elif_count; i++) {
                Type *elif_cond = synth_expr(c, node->as.if_expr.elifs[i].cond);
                unify(c, elif_cond, type_bool(), node->as.if_expr.elifs[i].cond->loc);
                check_block(c, node->as.if_expr.elifs[i].block);
            }
            if (node->as.if_expr.else_block.count > 0) {
                check_block(c, node->as.if_expr.else_block);
            }
            result = type_none();
        }
        break;
    }

    case NODE_WHILE: {
        Type *cond_type = synth_expr(c, node->as.while_expr.cond);
        unify(c, cond_type, type_bool(), node->as.while_expr.cond->loc);
        symtab_push_scope(&c->symtab);
        check_block(c, node->as.while_expr.body);
        symtab_pop_scope(&c->symtab);
        result = type_none();
        break;
    }

    case NODE_LOOP: {
        symtab_push_scope(&c->symtab);
        check_block(c, node->as.loop_expr.body);
        symtab_pop_scope(&c->symtab);
        result = type_none();
        break;
    }

    case NODE_BLOCK:
    case NODE_DO_BLOCK: {
        symtab_push_scope(&c->symtab);
        check_block(c, node->as.block);
        symtab_pop_scope(&c->symtab);
        result = type_none();
        break;
    }

    case NODE_SCOPE: {
        if (node->as.unary_stmt.value) {
            result = synth_expr(c, node->as.unary_stmt.value);
        } else {
            result = type_none();
        }
        break;
    }

    case NODE_SPAWN: {
        AstNode *inner = node->as.unary_stmt.value;
        if (!inner) {
            check_error(c, node->loc, "spawn requires an expression");
            result = type_error();
            break;
        }
        Type *inner_type = synth_expr(c, inner);
        result = type_task(c->arena, inner_type);
        break;
    }

    case NODE_AWAIT: {
        AstNode *inner = node->as.unary_stmt.value;
        if (!inner) {
            check_error(c, node->loc, "await requires an expression");
            result = type_error();
            break;
        }
        Type *inner_type = synth_expr(c, inner);
        inner_type = type_resolve(inner_type);
        if (inner_type->kind != TY_TASK) {
            check_error(c, node->loc, "await expects Task type, got %s",
                        type_to_string(inner_type));
            result = type_error();
        } else {
            result = inner_type->as.task.elem;
        }
        break;
    }

    case NODE_UPPER_IDENT: {
        /* Resolve type path to a symbol */
        if (node->as.type_path.seg_count >= 1) {
            const char *name = str_intern_range(c->interns,
                node->as.type_path.segments[0].name,
                node->as.type_path.segments[0].len);
            Symbol *sym = symtab_lookup(&c->symtab, name);
            if (sym) {
                result = sym->type;
            } else {
                check_error(c, node->loc, "undefined type '%.*s'",
                            node->as.type_path.segments[0].len,
                            node->as.type_path.segments[0].name);
                result = type_error();
            }
        } else {
            result = type_error();
        }
        break;
    }

    case NODE_FIELD_ACCESS: {
        Type *obj_type = synth_expr(c, node->as.field_access.object);
        obj_type = type_resolve(obj_type);

        /* Auto-dereference pointers */
        if (obj_type->kind == TY_PTR) {
            obj_type = type_resolve(obj_type->as.ptr.elem);
        }

        if (obj_type->kind == TY_RECORD) {
            const char *fname = str_intern_range(c->interns,
                node->as.field_access.field.name,
                node->as.field_access.field.len);
            bool found = false;
            for (int i = 0; i < obj_type->as.record.field_count; i++) {
                if (obj_type->as.record.field_names[i] == fname) {
                    result = obj_type->as.record.field_types[i];
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* Check for method: TypeName_fieldName */
                const char *type_name = obj_type->as.record.name;
                if (type_name) {
                    char mangled[256];
                    snprintf(mangled, sizeof(mangled), "%s_%s", type_name, fname);
                    const char *interned = str_intern(c->interns, mangled);
                    Symbol *method_sym = symtab_lookup(&c->symtab, interned);
                    if (method_sym && method_sym->kind == SYM_FN) {
                        result = method_sym->type;
                        found = true;
                    }
                }
                if (!found) {
                    check_error(c, node->loc, "no field '%.*s' on type '%s'",
                                node->as.field_access.field.len,
                                node->as.field_access.field.name,
                                type_to_string(obj_type));
                    result = type_error();
                }
            }
        } else if (obj_type->kind == TY_LIST) {
            const char *fname = str_intern_range(c->interns,
                node->as.field_access.field.name,
                node->as.field_access.field.len);
            if (fname == str_intern(c->interns, "len")) {
                result = type_int();
            } else {
                check_error(c, node->loc, "no field '%.*s' on List type",
                            node->as.field_access.field.len,
                            node->as.field_access.field.name);
                result = type_error();
            }
        } else if (obj_type->kind == TY_TUPLE) {
            check_error(c, node->loc, "use indexing to access tuple elements");
            result = type_error();
        } else if (obj_type->kind == TY_ERROR) {
            result = type_error();
        } else {
            /* Try method lookup for any named type */
            const char *type_name = type_to_string(obj_type);
            const char *fname = str_intern_range(c->interns,
                node->as.field_access.field.name,
                node->as.field_access.field.len);
            char mangled[256];
            snprintf(mangled, sizeof(mangled), "%s_%s", type_name, fname);
            const char *interned = str_intern(c->interns, mangled);
            Symbol *method_sym = symtab_lookup(&c->symtab, interned);
            if (method_sym && method_sym->kind == SYM_FN) {
                result = method_sym->type;
            } else {
                check_error(c, node->loc, "cannot access field on type '%s'",
                            type_to_string(obj_type));
                result = type_error();
            }
        }
        break;
    }

    case NODE_MATCH: {
        Type *subject_type = synth_expr(c, node->as.match.subject);

        Type *arm_type = NULL;
        for (int i = 0; i < node->as.match.arm_count; i++) {
            MatchArm *arm = &node->as.match.arms[i];
            symtab_push_scope(&c->symtab);

            /* Check pattern and bind variables */
            check_pattern(c, arm->pattern, subject_type);

            /* Check guard */
            if (arm->guard) {
                Type *guard_type = synth_expr(c, arm->guard);
                unify(c, guard_type, type_bool(), arm->guard->loc);
            }

            /* Check body */
            Type *body_type = synth_expr(c, arm->body);

            if (i == 0) {
                arm_type = body_type;
            } else {
                unify(c, arm_type, body_type, arm->body->loc);
            }

            symtab_pop_scope(&c->symtab);
        }

        result = arm_type ? arm_type : type_none();
        break;
    }

    case NODE_RANGE: {
        Type *start_type = synth_expr(c, node->as.range.start);
        Type *end_type = synth_expr(c, node->as.range.end);
        unify(c, start_type, type_int(), node->loc);
        unify(c, end_type, type_int(), node->loc);
        result = type_int(); /* range represented as its element type for iteration */
        break;
    }

    case NODE_FOR: {
        Type *iter_type = synth_expr(c, node->as.for_expr.iter);
        iter_type = type_resolve(iter_type);

        /* The iteration variable has the element type */
        Type *elem_type = iter_type; /* for ranges, this is Int */

        symtab_push_scope(&c->symtab);
        define_pattern_binding(c, node->as.for_expr.pattern, elem_type, false);
        check_block(c, node->as.for_expr.body);
        symtab_pop_scope(&c->symtab);
        result = type_none();
        break;
    }

    case NODE_PIPE: {
        /* a |> f  becomes f(a), a |> f(b) becomes f(a, b) */
        Type *left_type = synth_expr(c, node->as.pipe.left);
        AstNode *right = node->as.pipe.right;

        if (right->kind == NODE_CALL) {
            /* Prepend left as first argument — shift args right */
            int old_count = right->as.call.args.count;
            int new_count = old_count + 1;
            Arg *new_args = (Arg *)arena_alloc(c->arena, new_count * sizeof(Arg));
            new_args[0].name = NULL;
            new_args[0].value = node->as.pipe.left;
            for (int i = 0; i < old_count; i++) {
                new_args[i + 1] = right->as.call.args.items[i];
            }
            right->as.call.args.items = new_args;
            right->as.call.args.count = new_count;
            result = synth_expr(c, right);
        } else {
            /* right must be a function, call it with left */
            Type *fn_type = synth_expr(c, right);
            fn_type = type_resolve(fn_type);
            if (fn_type->kind == TY_FN) {
                if (fn_type->as.fn.param_count >= 1) {
                    unify(c, fn_type->as.fn.param_types[0], left_type, node->loc);
                }
                result = fn_type->as.fn.ret;
            } else if (fn_type->kind != TY_ERROR) {
                check_error(c, node->loc, "pipe target must be a function or call");
                result = type_error();
            } else {
                result = type_error();
            }
        }
        break;
    }

    case NODE_TUPLE: {
        int count = node->as.list.count;
        Type **elem_types = (Type **)arena_alloc(c->arena, count * sizeof(Type *));
        for (int i = 0; i < count; i++) {
            elem_types[i] = synth_expr(c, node->as.list.items[i]);
        }
        result = type_tuple(c->arena, elem_types, count);
        break;
    }

    case NODE_STRING_INTERP: {
        /* String interpolation: alternating string parts and expressions */
        for (int i = 0; i < node->as.list.count; i++) {
            synth_expr(c, node->as.list.items[i]);
        }
        result = type_string();
        break;
    }

    case NODE_LIST: {
        int count = node->as.list.count;
        if (count == 0) {
            result = type_list(c->arena, type_error());
            break;
        }
        Type *elem_type = synth_expr(c, node->as.list.items[0]);
        for (int i = 1; i < count; i++) {
            Type *et = synth_expr(c, node->as.list.items[i]);
            if (!type_equals(elem_type, et)) {
                check_error(c, node->as.list.items[i]->loc,
                    "list element type mismatch: expected '%s', got '%s'",
                    type_to_string(elem_type), type_to_string(et));
            }
        }
        result = type_list(c->arena, elem_type);
        break;
    }

    case NODE_MAP: {
        int count = node->as.map.count;
        if (count == 0) {
            result = type_map(c->arena, type_error(), type_error());
            break;
        }
        Type *key_type = NULL, *val_type = NULL;
        for (int i = 0; i < count; i++) {
            MapEntry *e = &node->as.map.entries[i];
            if (e->is_spread) continue;
            Type *kt = synth_expr(c, e->key);
            Type *vt = synth_expr(c, e->value);
            if (!key_type) { key_type = kt; val_type = vt; }
            else {
                if (!type_equals(key_type, kt))
                    check_error(c, e->key->loc, "map key type mismatch: expected '%s', got '%s'",
                                type_to_string(key_type), type_to_string(kt));
                if (!type_equals(val_type, vt))
                    check_error(c, e->value->loc, "map value type mismatch: expected '%s', got '%s'",
                                type_to_string(val_type), type_to_string(vt));
            }
        }
        if (!key_type) key_type = type_error();
        if (!val_type) val_type = type_error();
        result = type_map(c->arena, key_type, val_type);
        break;
    }

    case NODE_LIST_COMP: {
        /* [expr for pattern in iter] or [expr for pattern in iter if filter] */
        Type *iter_type = synth_expr(c, node->as.list_comp.iter);
        iter_type = type_resolve(iter_type);

        /* Determine element type from iterator */
        Type *elem_type = type_int(); /* default for ranges */
        if (iter_type->kind == TY_LIST) {
            elem_type = iter_type->as.list.elem;
        }

        symtab_push_scope(&c->symtab);
        /* Bind pattern */
        if (node->as.list_comp.pattern && node->as.list_comp.pattern->kind == PAT_BIND) {
            const char *pname = str_intern_range(c->interns,
                node->as.list_comp.pattern->as.bind.name,
                node->as.list_comp.pattern->as.bind.len);
            symtab_define(&c->symtab, pname, SYM_VAR, elem_type, false,
                          node->as.list_comp.pattern->as.bind.loc);
        }

        /* Check filter if present */
        if (node->as.list_comp.filter) {
            Type *filt_type = synth_expr(c, node->as.list_comp.filter);
            unify(c, filt_type, type_bool(), node->as.list_comp.filter->loc);
        }

        /* Synth expression type */
        Type *expr_type = synth_expr(c, node->as.list_comp.expr);
        symtab_pop_scope(&c->symtab);

        result = type_list(c->arena, expr_type);
        break;
    }

    case NODE_LAMBDA: {
        int param_count = node->as.lambda.params.count;
        Type **param_types = (Type **)arena_alloc(c->arena, param_count * sizeof(Type *));

        symtab_push_scope(&c->symtab);
        for (int i = 0; i < param_count; i++) {
            Param *p = &node->as.lambda.params.items[i];
            Type *pt = p->type ? resolve_ast_type(c, p->type) : type_error();
            param_types[i] = pt;
            if (p->pattern && p->pattern->kind == PAT_BIND) {
                const char *pname = str_intern_range(c->interns,
                    p->pattern->as.bind.name, p->pattern->as.bind.len);
                symtab_define(&c->symtab, pname, SYM_PARAM, pt, false,
                              p->pattern->as.bind.loc);
            }
        }

        Type *body_type;
        if (node->as.lambda.body_expr) {
            body_type = synth_expr(c, node->as.lambda.body_expr);
        } else {
            check_block(c, node->as.lambda.body_block);
            body_type = type_none();
        }
        symtab_pop_scope(&c->symtab);

        result = type_fn(c->arena, param_types, param_count, body_type, false);
        break;
    }

    case NODE_WITH: {
        Type *base_type = synth_expr(c, node->as.with.base);
        base_type = type_resolve(base_type);
        if (base_type->kind == TY_RECORD) {
            for (int i = 0; i < node->as.with.update_count; i++) {
                FieldUpdate *fu = &node->as.with.updates[i];
                const char *fname = str_intern_range(c->interns, fu->name.name, fu->name.len);
                Type *fval_type = synth_expr(c, fu->value);
                /* Find field in record */
                bool found = false;
                for (int j = 0; j < base_type->as.record.field_count; j++) {
                    if (base_type->as.record.field_names[j] == fname) {
                        unify(c, base_type->as.record.field_types[j], fval_type, fu->value->loc);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    check_error(c, fu->name.loc, "record '%s' has no field '%.*s'",
                                type_to_string(base_type), fu->name.len, fu->name.name);
                }
            }
            result = base_type;
        } else {
            check_error(c, node->loc, "'with' expression on non-record type '%s'",
                        type_to_string(base_type));
            result = type_error();
        }
        break;
    }

    case NODE_INDEX: {
        Type *obj_type = synth_expr(c, node->as.index.object);
        synth_expr(c, node->as.index.index);
        obj_type = type_resolve(obj_type);

        if (obj_type->kind == TY_TUPLE) {
            /* Tuple indexing: index must be an int literal */
            if (node->as.index.index->kind == NODE_LITERAL &&
                node->as.index.index->as.literal.kind == LIT_INT) {
                int idx = (int)strtol(node->as.index.index->as.literal.value, NULL, 10);
                if (idx < 0 || idx >= obj_type->as.tuple.count) {
                    check_error(c, node->loc, "tuple index %d out of range (tuple has %d elements)",
                                idx, obj_type->as.tuple.count);
                    result = type_error();
                } else {
                    result = obj_type->as.tuple.elems[idx];
                }
            } else {
                check_error(c, node->loc, "tuple index must be an integer literal");
                result = type_error();
            }
        } else if (obj_type->kind == TY_LIST) {
            Type *idx_type = type_resolve(node->as.index.index->checked_type);
            if (idx_type && idx_type->kind != TY_INT) {
                check_error(c, node->as.index.index->loc,
                    "list index must be Int, got '%s'", type_to_string(idx_type));
            }
            result = obj_type->as.list.elem;
        } else if (obj_type->kind == TY_MAP) {
            Type *idx_type = type_resolve(node->as.index.index->checked_type);
            if (idx_type && !type_equals(obj_type->as.map.key, idx_type)) {
                check_error(c, node->as.index.index->loc,
                    "map key type mismatch: expected '%s', got '%s'",
                    type_to_string(obj_type->as.map.key), type_to_string(idx_type));
            }
            result = obj_type->as.map.val;
        } else if (obj_type->kind == TY_ERROR) {
            result = type_error();
        } else {
            check_error(c, node->loc, "cannot index type '%s'", type_to_string(obj_type));
            result = type_error();
        }
        break;
    }

    default:
        result = type_none();
        break;
    }

    node->checked_type = result;
    return result;
}

/* ===== Statement checking ===== */

static void define_pattern_binding(Checker *c, AstPattern *pat, Type *type, bool is_mutable) {
    if (!pat) return;
    switch (pat->kind) {
    case PAT_BIND: {
        const char *bname = str_intern_range(c->interns, pat->as.bind.name,
                                             pat->as.bind.len);
        Symbol *existing = symtab_define(&c->symtab, bname, SYM_VAR,
                                         type, is_mutable, pat->as.bind.loc);
        if (!existing) {
            check_error(c, pat->as.bind.loc, "redefinition of '%.*s'",
                        pat->as.bind.len, pat->as.bind.name);
        }
        break;
    }
    case PAT_WILDCARD:
        break;
    case PAT_TUPLE: {
        Type *resolved = type_resolve(type);
        if (resolved->kind == TY_TUPLE) {
            int n = pat->as.tuple.count;
            if (n != resolved->as.tuple.count) {
                check_error(c, pat->loc, "tuple pattern has %d elements, type has %d",
                            n, resolved->as.tuple.count);
            }
            int count = n < resolved->as.tuple.count ? n : resolved->as.tuple.count;
            for (int i = 0; i < count; i++) {
                define_pattern_binding(c, pat->as.tuple.items[i],
                                       resolved->as.tuple.elems[i], is_mutable);
            }
        } else if (resolved->kind != TY_ERROR) {
            check_error(c, pat->loc, "tuple pattern used with non-tuple type '%s'",
                        type_to_string(resolved));
        }
        break;
    }
    default:
        break;
    }
}

/* Check a pattern against an expected type, binding variables into the current scope */
static void check_pattern(Checker *c, AstPattern *pat, Type *expected_type) {
    if (!pat) return;
    switch (pat->kind) {
    case PAT_BIND:
        define_pattern_binding(c, pat, expected_type, false);
        break;
    case PAT_WILDCARD:
        break;
    case PAT_LITERAL: {
        Type *lit_type = synth_expr(c, pat->as.literal);
        unify(c, expected_type, lit_type, pat->loc);
        break;
    }
    case PAT_TUPLE: {
        Type *resolved = type_resolve(expected_type);
        if (resolved->kind == TY_TUPLE) {
            int n = pat->as.tuple.count;
            int count = n < resolved->as.tuple.count ? n : resolved->as.tuple.count;
            for (int i = 0; i < count; i++) {
                check_pattern(c, pat->as.tuple.items[i], resolved->as.tuple.elems[i]);
            }
        } else if (resolved->kind != TY_ERROR) {
            check_error(c, pat->loc, "tuple pattern used with non-tuple type '%s'",
                        type_to_string(resolved));
        }
        break;
    }
    case PAT_OR: {
        for (int i = 0; i < pat->as.or_pats.count; i++) {
            check_pattern(c, pat->as.or_pats.items[i], expected_type);
        }
        break;
    }
    case PAT_BIND_AS: {
        check_pattern(c, pat->as.bind_as.pattern, expected_type);
        const char *name = str_intern_range(c->interns, pat->as.bind_as.name.name,
                                            pat->as.bind_as.name.len);
        symtab_define(&c->symtab, name, SYM_VAR, expected_type, false, pat->as.bind_as.name.loc);
        break;
    }
    case PAT_VARIANT: {
        /* Look up the variant constructor */
        if (pat->as.variant.path.seg_count >= 1) {
            const char *vname = str_intern_range(c->interns,
                pat->as.variant.path.segments[0].name,
                pat->as.variant.path.segments[0].len);
            Symbol *sym = symtab_lookup(&c->symtab, vname);
            if (sym) {
                Type *sym_type = type_resolve(sym->type);
                if (sym_type->kind == TY_FN) {
                    /* Variant constructor — check payload patterns */
                    int payload_count = pat->as.variant.args.count;
                    int param_count = sym_type->as.fn.param_count;
                    if (payload_count != param_count) {
                        check_error(c, pat->loc, "variant pattern has %d fields, expected %d",
                                    payload_count, param_count);
                    }
                    int n = payload_count < param_count ? payload_count : param_count;
                    for (int i = 0; i < n; i++) {
                        check_pattern(c, pat->as.variant.args.items[i],
                                      sym_type->as.fn.param_types[i]);
                    }
                }
                /* Unify variant return type with expected type */
                if (sym_type->kind == TY_FN) {
                    unify(c, expected_type, sym_type->as.fn.ret, pat->loc);
                } else {
                    unify(c, expected_type, sym_type, pat->loc);
                }
            } else {
                check_error(c, pat->loc, "unknown variant '%.*s'",
                            pat->as.variant.path.segments[0].len,
                            pat->as.variant.path.segments[0].name);
            }
        }
        break;
    }
    default:
        break;
    }
}

static void check_stmt(Checker *c, AstNode *node) {
    if (!node) return;

    switch (node->kind) {
    case NODE_LET:
    case NODE_VAR: {
        bool is_mutable = (node->kind == NODE_VAR);
        Type *ty = NULL;

        if (node->as.let.type) {
            ty = resolve_ast_type(c, node->as.let.type);
        }

        if (node->as.let.value) {
            Type *val_type = synth_expr(c, node->as.let.value);
            if (ty) {
                unify(c, ty, val_type, node->loc);
            } else {
                ty = val_type;
            }
        }

        if (!ty) ty = type_error();
        define_pattern_binding(c, node->as.let.pattern, ty, is_mutable);
        node->checked_type = ty;
        break;
    }

    case NODE_ASSIGN: {
        Type *target_type = synth_expr(c, node->as.assign.target);
        Type *val_type = synth_expr(c, node->as.assign.value);

        /* Check mutability */
        if (node->as.assign.target->kind == NODE_FIELD_ACCESS) {
            /* Check if the object is mutable */
            AstNode *obj = node->as.assign.target->as.field_access.object;
            if (obj->kind == NODE_IDENT) {
                const char *oname = str_intern_range(c->interns, obj->as.ident.name, obj->as.ident.len);
                Symbol *sym = symtab_lookup(&c->symtab, oname);
                if (sym && !sym->is_mutable) {
                    check_error(c, node->loc, "cannot assign to field of immutable variable '%.*s'",
                                obj->as.ident.len, obj->as.ident.name);
                }
            }
        } else if (node->as.assign.target->kind == NODE_INDEX) {
            /* List element mutation is allowed even on immutable bindings,
             * because it modifies the shared heap buffer, not the binding.
             * Only require mutability for non-list indexed types. */
            AstNode *obj = node->as.assign.target->as.index.object;
            Type *obj_type = obj->checked_type ? type_resolve(obj->checked_type) : NULL;
            if (obj->kind == NODE_IDENT && (!obj_type || obj_type->kind != TY_LIST)) {
                const char *oname = str_intern_range(c->interns, obj->as.ident.name, obj->as.ident.len);
                Symbol *sym = symtab_lookup(&c->symtab, oname);
                if (sym && !sym->is_mutable) {
                    check_error(c, node->loc, "cannot assign to element of immutable variable '%.*s'",
                                obj->as.ident.len, obj->as.ident.name);
                }
            }
        } else if (node->as.assign.target->kind == NODE_IDENT) {
            const char *aname = str_intern_range(c->interns, node->as.assign.target->as.ident.name,
                                                 node->as.assign.target->as.ident.len);
            Symbol *sym = symtab_lookup(&c->symtab, aname);
            if (sym && !sym->is_mutable) {
                check_error(c, node->loc, "cannot assign to immutable variable '%.*s'",
                            node->as.assign.target->as.ident.len,
                            node->as.assign.target->as.ident.name);
            }
        }

        /* For compound assign (+=, -=, etc.), check operand types */
        if (node->as.assign.op != TOK_ASSIGN) {
            TokenKind op = node->as.assign.op;
            if (op == TOK_AMP_ASSIGN || op == TOK_PIPE_ASSIGN ||
                op == TOK_CARET_ASSIGN || op == TOK_SHL_ASSIGN ||
                op == TOK_SHR_ASSIGN) {
                if (type_resolve(target_type)->kind != TY_INT && target_type->kind != TY_ERROR) {
                    check_error(c, node->loc, "bitwise compound assignment on non-integer type");
                }
            } else if (op == TOK_AND_ASSIGN || op == TOK_OR_ASSIGN) {
                unify(c, target_type, type_bool(), node->loc);
            } else if (op == TOK_PERCENT_ASSIGN) {
                if (!type_is_numeric(target_type) && target_type->kind != TY_ERROR) {
                    check_error(c, node->loc, "compound assignment on non-numeric type");
                }
            } else {
                if (!type_is_numeric(target_type) && target_type->kind != TY_ERROR) {
                    check_error(c, node->loc, "compound assignment on non-numeric type");
                }
            }
        }

        unify(c, target_type, val_type, node->loc);
        node->checked_type = type_none();
        break;
    }

    case NODE_RETURN: {
        if (node->as.unary_stmt.value) {
            Type *val_type = synth_expr(c, node->as.unary_stmt.value);
            if (c->current_ret_type) {
                unify(c, c->current_ret_type, val_type, node->loc);
            }
        } else {
            if (c->current_ret_type && !type_equals(c->current_ret_type, type_none())) {
                check_error(c, node->loc, "return without value in function returning '%s'",
                            type_to_string(c->current_ret_type));
            }
        }
        node->checked_type = type_never();
        break;
    }

    case NODE_EXPR_STMT: {
        synth_expr(c, node->as.expr_stmt.expr);
        node->checked_type = type_none();
        break;
    }

    case NODE_IF:
    case NODE_WHILE:
    case NODE_LOOP:
    case NODE_FOR:
    case NODE_MATCH:
    case NODE_BLOCK:
    case NODE_DO_BLOCK:
        synth_expr(c, node);
        break;

    case NODE_BREAK:
    case NODE_CONTINUE:
        node->checked_type = type_never();
        break;

    case NODE_ASSERT: {
        Type *cond_type = synth_expr(c, node->as.assert_stmt.cond);
        unify(c, cond_type, type_bool(), node->loc);
        if (node->as.assert_stmt.message) {
            synth_expr(c, node->as.assert_stmt.message);
        }
        node->checked_type = type_none();
        break;
    }

    case NODE_DEFER:
        if (node->as.defer.body) {
            synth_expr(c, node->as.defer.body);
        }
        node->checked_type = type_none();
        break;

    default:
        /* For unhandled statement kinds, try as expression */
        synth_expr(c, node);
        break;
    }
}

static void check_block(Checker *c, NodeList block) {
    for (int i = 0; i < block.count; i++) {
        check_stmt(c, block.items[i]);
    }
}

/* ===== Declaration gathering (Pass 1) ===== */

static void gather_extern_fn(Checker *c, AstNode *node) {
    const char *name = str_intern_range(c->interns, node->as.extern_fn.name.name,
                                        node->as.extern_fn.name.len);

    /* Build parameter types */
    int param_count = 0;
    bool is_variadic = false;
    Type **param_types = NULL;

    /* Count non-variadic params */
    for (int i = 0; i < node->as.extern_fn.param_count; i++) {
        if (node->as.extern_fn.params[i].is_variadic) {
            is_variadic = true;
        } else {
            param_count++;
        }
    }

    if (param_count > 0) {
        param_types = (Type **)arena_alloc(c->arena, param_count * sizeof(Type *));
        int idx = 0;
        for (int i = 0; i < node->as.extern_fn.param_count; i++) {
            if (!node->as.extern_fn.params[i].is_variadic) {
                param_types[idx++] = resolve_ast_type(c, node->as.extern_fn.params[i].type);
            }
        }
    }

    Type *ret = node->as.extern_fn.ret_type ?
                resolve_ast_type(c, node->as.extern_fn.ret_type) : type_none();

    Type *fn_type = type_fn(c->arena, param_types, param_count, ret, is_variadic);

    Symbol *sym = symtab_define(&c->symtab, name, SYM_EXTERN_FN, fn_type, false,
                                node->as.extern_fn.name.loc);
    if (!sym) {
        check_error(c, node->loc, "redefinition of extern function '%.*s'",
                    node->as.extern_fn.name.len, name);
    }
}

static void gather_fn_decl(Checker *c, AstNode *node) {
    const char *name = str_intern_range(c->interns, node->as.fn_decl.name.name,
                                        node->as.fn_decl.name.len);

    /* If generic, push type params into scope as TY_PARAM */
    bool is_generic = (node->as.fn_decl.type_params.count > 0);
    if (is_generic) {
        symtab_push_scope(&c->symtab);
        for (int i = 0; i < node->as.fn_decl.type_params.count; i++) {
            TypeParam *tp = &node->as.fn_decl.type_params.items[i];
            const char *tpname = str_intern_range(c->interns, tp->name.name, tp->name.len);
            Type *param_type = type_param(c->arena, tpname);
            symtab_define(&c->symtab, tpname, SYM_TYPE, param_type, false, tp->name.loc);
        }
    }

    /* Build parameter types */
    int param_count = node->as.fn_decl.params.count;
    Type **param_types = NULL;
    if (param_count > 0) {
        param_types = (Type **)arena_alloc(c->arena, param_count * sizeof(Type *));
        for (int i = 0; i < param_count; i++) {
            Param *p = &node->as.fn_decl.params.items[i];
            param_types[i] = p->type ? resolve_ast_type(c, p->type) : type_error();
        }
    }

    Type *ret = node->as.fn_decl.ret_type ?
                resolve_ast_type(c, node->as.fn_decl.ret_type) : type_none();

    if (is_generic) {
        symtab_pop_scope(&c->symtab);
    }

    Type *fn_type = type_fn(c->arena, param_types, param_count, ret, false);

    Symbol *sym = symtab_define(&c->symtab, name, SYM_FN, fn_type, false,
                                node->as.fn_decl.name.loc);
    if (!sym) {
        check_error(c, node->loc, "redefinition of function '%.*s'",
                    node->as.fn_decl.name.len, name);
    }
}

/* ===== Body checking (Pass 2) ===== */

static void check_fn_body(Checker *c, AstNode *node) {
    if (node->as.fn_decl.is_signature) return;

    bool is_generic = (node->as.fn_decl.type_params.count > 0);

    const char *fname = str_intern_range(c->interns, node->as.fn_decl.name.name,
                                         node->as.fn_decl.name.len);
    Symbol *sym = symtab_lookup(&c->symtab, fname);
    if (!sym) return;

    Type *fn_type = type_resolve(sym->type);
    c->current_ret_type = fn_type->as.fn.ret;

    symtab_push_scope(&c->symtab);

    /* For generic functions, push type params into scope */
    if (is_generic) {
        for (int i = 0; i < node->as.fn_decl.type_params.count; i++) {
            TypeParam *tp = &node->as.fn_decl.type_params.items[i];
            const char *tpname = str_intern_range(c->interns, tp->name.name, tp->name.len);
            Type *param_type = type_param(c->arena, tpname);
            symtab_define(&c->symtab, tpname, SYM_TYPE, param_type, false, tp->name.loc);
        }
    }

    /* Bind parameters */
    for (int i = 0; i < node->as.fn_decl.params.count; i++) {
        Param *p = &node->as.fn_decl.params.items[i];
        if (p->pattern && p->pattern->kind == PAT_BIND) {
            const char *pname = str_intern_range(c->interns, p->pattern->as.bind.name,
                                                 p->pattern->as.bind.len);
            symtab_define(&c->symtab, pname, SYM_PARAM,
                          fn_type->as.fn.param_types[i], false, p->pattern->as.bind.loc);
        }
    }

    /* Check body */
    check_block(c, node->as.fn_decl.body);

    symtab_pop_scope(&c->symtab);
    c->current_ret_type = NULL;
}

/* ===== Top-level checking ===== */

void checker_init(Checker *c, Arena *arena, InternTable *interns) {
    memset(c, 0, sizeof(Checker));
    c->arena = arena;
    c->interns = interns;
    symtab_init(&c->symtab, arena);

    /* Intern built-in type names */
    c->int_name = str_intern(interns, "Int");
    c->float_name = str_intern(interns, "Float");
    c->bool_name = str_intern(interns, "Bool");
    c->char_name = str_intern(interns, "Char");
    c->string_name = str_intern(interns, "String");
    c->none_name = str_intern(interns, "None");

    /* Register built-in types in global scope */
    SrcLoc builtin_loc = { .file = "<builtin>", .line = 0, .col = 0 };
    symtab_define(&c->symtab, c->int_name, SYM_TYPE, type_int(), false, builtin_loc);
    symtab_define(&c->symtab, c->float_name, SYM_TYPE, type_float(), false, builtin_loc);
    symtab_define(&c->symtab, c->bool_name, SYM_TYPE, type_bool(), false, builtin_loc);
    symtab_define(&c->symtab, c->char_name, SYM_TYPE, type_char(), false, builtin_loc);
    symtab_define(&c->symtab, c->string_name, SYM_TYPE, type_string(), false, builtin_loc);
    symtab_define(&c->symtab, c->none_name, SYM_TYPE, type_none(), false, builtin_loc);
}

void checker_check(Checker *c, AstNode *program) {
    if (!program || program->kind != NODE_PROGRAM) return;

    /* Pass 1: gather all declarations */
    for (int i = 0; i < program->as.program.count; i++) {
        AstNode *item = program->as.program.items[i];
        switch (item->kind) {
        case NODE_FN_DECL:
            gather_fn_decl(c, item);
            break;
        case NODE_EXTERN_BLOCK:
            for (int j = 0; j < item->as.extern_block.decls.count; j++) {
                AstNode *decl = item->as.extern_block.decls.items[j];
                if (decl->kind == NODE_EXTERN_FN) {
                    gather_extern_fn(c, decl);
                }
            }
            break;
        case NODE_CONST_DECL: {
            const char *name = str_intern_range(c->interns, item->as.const_decl.name.name,
                                                item->as.const_decl.name.len);
            Type *ty = NULL;
            if (item->as.const_decl.type) {
                ty = resolve_ast_type(c, item->as.const_decl.type);
            }
            if (item->as.const_decl.value) {
                Type *val_ty = synth_expr(c, item->as.const_decl.value);
                if (ty) {
                    unify(c, ty, val_ty, item->loc);
                } else {
                    ty = val_ty;
                }
            }
            if (!ty) ty = type_error();
            symtab_define(&c->symtab, name, SYM_CONST, ty, false,
                          item->as.const_decl.name.loc);
            break;
        }
        case NODE_TYPE_DECL: {
            const char *tname = str_intern_range(c->interns, item->as.type_decl.name.name,
                                                 item->as.type_decl.name.len);
            if (item->as.type_decl.body_kind == TYPE_BODY_ALIAS) {
                Type *aliased = resolve_ast_type(c, item->as.type_decl.body.alias);
                symtab_define(&c->symtab, tname, SYM_TYPE, aliased, false,
                              item->as.type_decl.name.loc);
            } else if (item->as.type_decl.body_kind == TYPE_BODY_RECORD) {
                int fc = item->as.type_decl.body.record.count;
                const char **fnames = (const char **)arena_alloc(c->arena, fc * sizeof(const char *));
                Type **ftypes = (Type **)arena_alloc(c->arena, fc * sizeof(Type *));
                for (int j = 0; j < fc; j++) {
                    FieldDef *fd = &item->as.type_decl.body.record.fields[j];
                    fnames[j] = str_intern_range(c->interns, fd->name.name, fd->name.len);
                    ftypes[j] = fd->type ? resolve_ast_type(c, fd->type) : type_error();
                }
                Type *rec_type = type_record(c->arena, tname, fnames, ftypes, fc);
                symtab_define(&c->symtab, tname, SYM_TYPE, rec_type, false,
                              item->as.type_decl.name.loc);

                /* Calls to the type name resolve through SYM_TYPE in NODE_CALL */
            } else if (item->as.type_decl.body_kind == TYPE_BODY_ENUM) {
                int vc = item->as.type_decl.body.enumt.count;
                VariantInfo *variants = (VariantInfo *)arena_alloc(c->arena, vc * sizeof(VariantInfo));
                memset(variants, 0, vc * sizeof(VariantInfo));

                /* First pass: fill in variant info (types only) */
                for (int j = 0; j < vc; j++) {
                    VariantDef *vd = &item->as.type_decl.body.enumt.variants[j];
                    variants[j].name = str_intern_range(c->interns, vd->name.name, vd->name.len);
                    variants[j].payload_count = vd->field_count;
                    variants[j].is_record_variant = vd->is_record;

                    if (vd->field_count > 0) {
                        variants[j].payload_types = (Type **)arena_alloc(c->arena, vd->field_count * sizeof(Type *));
                        variants[j].payload_names = vd->is_record ?
                            (const char **)arena_alloc(c->arena, vd->field_count * sizeof(const char *)) : NULL;
                        for (int k = 0; k < vd->field_count; k++) {
                            variants[j].payload_types[k] = vd->fields[k].type ?
                                resolve_ast_type(c, vd->fields[k].type) : type_error();
                            if (vd->is_record) {
                                variants[j].payload_names[k] = str_intern_range(c->interns,
                                    vd->fields[k].name.name, vd->fields[k].name.len);
                            }
                        }
                    }
                }

                /* Create enum type with filled-in variants */
                Type *enum_type = type_enum(c->arena, tname, variants, vc);
                symtab_define(&c->symtab, tname, SYM_TYPE, enum_type, false,
                              item->as.type_decl.name.loc);

                /* Second pass: register variant constructors */
                for (int j = 0; j < vc; j++) {
                    VariantDef *vd = &item->as.type_decl.body.enumt.variants[j];
                    const char *vname = variants[j].name;
                    if (vd->field_count > 0) {
                        Type *ctor = type_fn(c->arena, variants[j].payload_types,
                                            vd->field_count, enum_type, false);
                        symtab_define(&c->symtab, vname, SYM_FN, ctor, false, vd->name.loc);
                    } else {
                        symtab_define(&c->symtab, vname, SYM_CONST, enum_type, false, vd->name.loc);
                    }
                }
            }
            break;
        }
        case NODE_TRAIT_DECL: {
            /* Register trait name (for trait bounds — currently just stores the name) */
            const char *tname = str_intern_range(c->interns, item->as.trait_decl.name.name,
                                                  item->as.trait_decl.name.len);
            /* We register the trait as a SYM_TYPE with type_none() for now.
               A proper implementation would store the method signatures. */
            symtab_define(&c->symtab, tname, SYM_TYPE, type_none(), false,
                          item->as.trait_decl.name.loc);
            break;
        }
        case NODE_METHOD_DECL: {
            /* Register method as TypeName_methodName */
            const char *type_name = str_intern_range(c->interns,
                item->as.method_decl.type_path.segments[0].name,
                item->as.method_decl.type_path.segments[0].len);
            const char *method_name = str_intern_range(c->interns,
                item->as.method_decl.method_name.name,
                item->as.method_decl.method_name.len);

            /* Look up the type to get 'self' type */
            Symbol *type_sym = symtab_lookup(&c->symtab, type_name);
            Type *self_type = (type_sym && type_sym->kind == SYM_TYPE) ? type_sym->type : type_error();

            /* Build function type from params (self is already in the param list) */
            int param_count = item->as.method_decl.params.count;
            Type **param_types = (Type **)arena_alloc(c->arena, param_count * sizeof(Type *));
            for (int j = 0; j < param_count; j++) {
                Param *p = &item->as.method_decl.params.items[j];
                if (p->is_self) {
                    param_types[j] = type_ptr(c->arena, self_type);
                } else {
                    param_types[j] = p->type ? resolve_ast_type(c, p->type) : type_error();
                }
            }
            Type *ret = item->as.method_decl.ret_type ?
                        resolve_ast_type(c, item->as.method_decl.ret_type) : type_none();
            Type *fn_type = type_fn(c->arena, param_types, param_count, ret, false);

            /* Mangle name: TypeName_methodName */
            char mangled[256];
            snprintf(mangled, sizeof(mangled), "%s_%s", type_name, method_name);
            const char *interned_mangled = str_intern(c->interns, mangled);

            symtab_define(&c->symtab, interned_mangled, SYM_FN, fn_type, false,
                          item->as.method_decl.method_name.loc);

            /* Store mangled name on the node for codegen */
            item->checked_type = fn_type;
            break;
        }
        default:
            break;
        }
    }

    /* Pass 2: check function bodies */
    for (int i = 0; i < program->as.program.count; i++) {
        AstNode *item = program->as.program.items[i];
        if (item->kind == NODE_FN_DECL) {
            check_fn_body(c, item);
        }
        if (item->kind == NODE_METHOD_DECL) {
            /* Check method body */
            const char *type_name = str_intern_range(c->interns,
                item->as.method_decl.type_path.segments[0].name,
                item->as.method_decl.type_path.segments[0].len);
            Symbol *type_sym = symtab_lookup(&c->symtab, type_name);
            Type *self_type = (type_sym && type_sym->kind == SYM_TYPE) ? type_sym->type : type_error();

            Type *fn_type = item->checked_type ? type_resolve(item->checked_type) : NULL;
            if (!fn_type || fn_type->kind != TY_FN) continue;

            symtab_push_scope(&c->symtab);

            /* Bind parameters (self gets *Type, others use their declared types) */
            for (int j = 0; j < item->as.method_decl.params.count; j++) {
                Param *p = &item->as.method_decl.params.items[j];
                if (p->is_self) {
                    const char *self_name = str_intern(c->interns, "self");
                    symtab_define(&c->symtab, self_name, SYM_PARAM,
                                  type_ptr(c->arena, self_type), true,
                                  item->as.method_decl.method_name.loc);
                } else if (p->pattern && p->pattern->kind == PAT_BIND) {
                    const char *pname = str_intern_range(c->interns,
                        p->pattern->as.bind.name, p->pattern->as.bind.len);
                    Type *pt = p->type ? resolve_ast_type(c, p->type) : type_error();
                    symtab_define(&c->symtab, pname, SYM_PARAM, pt, false,
                                  p->pattern->loc);
                }
            }

            /* Set return type context and check body */
            Type *saved_ret = c->current_ret_type;
            c->current_ret_type = fn_type->as.fn.ret;
            for (int j = 0; j < item->as.method_decl.body.count; j++) {
                check_stmt(c, item->as.method_decl.body.items[j]);
            }
            c->current_ret_type = saved_ret;

            symtab_pop_scope(&c->symtab);
        }
    }
}
