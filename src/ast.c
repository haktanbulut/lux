#include "ast.h"
#include <stdio.h>
#include <string.h>

AstNode *ast_new(Arena *a, AstNodeKind kind, SrcLoc loc) {
    AstNode *n = (AstNode *)arena_alloc(a, sizeof(AstNode));
    memset(n, 0, sizeof(AstNode));
    n->kind = kind;
    n->loc = loc;
    return n;
}

AstType *ast_new_type(Arena *a, AstTypeKind kind, SrcLoc loc) {
    AstType *t = (AstType *)arena_alloc(a, sizeof(AstType));
    memset(t, 0, sizeof(AstType));
    t->kind = kind;
    t->loc = loc;
    return t;
}

AstPattern *ast_new_pattern(Arena *a, AstPatternKind kind, SrcLoc loc) {
    AstPattern *p = (AstPattern *)arena_alloc(a, sizeof(AstPattern));
    memset(p, 0, sizeof(AstPattern));
    p->kind = kind;
    p->loc = loc;
    return p;
}

AstNode **ast_node_list(Arena *a, AstNode **vec, int count) {
    if (count == 0) return NULL;
    AstNode **list = (AstNode **)arena_alloc(a, count * sizeof(AstNode *));
    memcpy(list, vec, count * sizeof(AstNode *));
    return list;
}

AstType **ast_type_list(Arena *a, AstType **vec, int count) {
    if (count == 0) return NULL;
    AstType **list = (AstType **)arena_alloc(a, count * sizeof(AstType *));
    memcpy(list, vec, count * sizeof(AstType *));
    return list;
}

AstPattern **ast_pattern_list(Arena *a, AstPattern **vec, int count) {
    if (count == 0) return NULL;
    AstPattern **list = (AstPattern **)arena_alloc(a, count * sizeof(AstPattern *));
    memcpy(list, vec, count * sizeof(AstPattern *));
    return list;
}

/* ===== Pretty Printer ===== */

static void print_indent(int indent) {
    for (int i = 0; i < indent; i++) printf("  ");
}

static void print_ident(Ident id) {
    printf("%.*s", id.len, id.name);
}

void ast_print_type(AstType *type) {
    if (!type) { printf("?"); return; }
    switch (type->kind) {
        case TYPE_PATH:
            for (int i = 0; i < type->as.path.path.seg_count; i++) {
                if (i > 0) printf(".");
                print_ident(type->as.path.path.segments[i]);
            }
            if (type->as.path.args.count > 0) {
                printf("[");
                for (int i = 0; i < type->as.path.args.count; i++) {
                    if (i > 0) printf(", ");
                    ast_print_type(type->as.path.args.items[i]);
                }
                printf("]");
            }
            break;
        case TYPE_FN:
            printf("fn(");
            for (int i = 0; i < type->as.fn.params.count; i++) {
                if (i > 0) printf(", ");
                ast_print_type(type->as.fn.params.items[i]);
            }
            printf(")");
            if (type->as.fn.ret) {
                printf(" -> ");
                ast_print_type(type->as.fn.ret);
            }
            break;
        case TYPE_TUPLE:
            printf("(");
            for (int i = 0; i < type->as.tuple.count; i++) {
                if (i > 0) printf(", ");
                ast_print_type(type->as.tuple.items[i]);
            }
            printf(")");
            break;
        case TYPE_LIST:
            printf("[");
            ast_print_type(type->as.list_elem);
            printf("]");
            break;
        case TYPE_MAP:
            printf("{");
            ast_print_type(type->as.map.key);
            printf(": ");
            ast_print_type(type->as.map.val);
            printf("}");
            break;
        case TYPE_UNION:
            for (int i = 0; i < type->as.union_types.count; i++) {
                if (i > 0) printf(" | ");
                ast_print_type(type->as.union_types.items[i]);
            }
            break;
        case TYPE_SELF:
            printf("Self");
            break;
        case TYPE_POINTER:
            printf("*");
            ast_print_type(type->as.pointer_elem);
            break;
    }
}

void ast_print_pattern(AstPattern *pat) {
    if (!pat) { printf("?"); return; }
    switch (pat->kind) {
        case PAT_WILDCARD: printf("_"); break;
        case PAT_BIND: print_ident(pat->as.bind); break;
        case PAT_LITERAL: ast_print(pat->as.literal, 0); break;
        case PAT_VARIANT:
            for (int i = 0; i < pat->as.variant.path.seg_count; i++) {
                if (i > 0) printf(".");
                print_ident(pat->as.variant.path.segments[i]);
            }
            if (pat->as.variant.args.count > 0) {
                printf("(");
                for (int i = 0; i < pat->as.variant.args.count; i++) {
                    if (i > 0) printf(", ");
                    ast_print_pattern(pat->as.variant.args.items[i]);
                }
                printf(")");
            }
            break;
        case PAT_TUPLE:
            printf("(");
            for (int i = 0; i < pat->as.tuple.count; i++) {
                if (i > 0) printf(", ");
                ast_print_pattern(pat->as.tuple.items[i]);
            }
            printf(")");
            break;
        case PAT_LIST:
            printf("[");
            for (int i = 0; i < pat->as.list.count; i++) {
                if (i > 0) printf(", ");
                ast_print_pattern(pat->as.list.items[i]);
            }
            printf("]");
            break;
        case PAT_LIST_REST:
            printf("[");
            for (int i = 0; i < pat->as.list_rest.elems.count; i++) {
                if (i > 0) printf(", ");
                ast_print_pattern(pat->as.list_rest.elems.items[i]);
            }
            printf(", ..");
            if (pat->as.list_rest.rest_name) print_ident(*pat->as.list_rest.rest_name);
            printf("]");
            break;
        case PAT_RECORD:
            printf("{");
            for (int i = 0; i < pat->as.record.count; i++) {
                if (i > 0) printf(", ");
                print_ident(pat->as.record.fields[i].name);
                if (pat->as.record.fields[i].pattern) {
                    printf(": ");
                    ast_print_pattern(pat->as.record.fields[i].pattern);
                }
            }
            printf("}");
            break;
        case PAT_OR:
            for (int i = 0; i < pat->as.or_pats.count; i++) {
                if (i > 0) printf(" | ");
                ast_print_pattern(pat->as.or_pats.items[i]);
            }
            break;
        case PAT_BIND_AS:
            print_ident(pat->as.bind_as.name);
            printf(" @ ");
            ast_print_pattern(pat->as.bind_as.pattern);
            break;
        case PAT_REST:
            printf("..");
            if (pat->as.rest_name) print_ident(*pat->as.rest_name);
            break;
    }
}

static void print_block(NodeList block, int indent) {
    for (int i = 0; i < block.count; i++) {
        ast_print(block.items[i], indent);
    }
}

void ast_print(AstNode *node, int indent) {
    if (!node) return;
    print_indent(indent);

    switch (node->kind) {
    case NODE_PROGRAM:
        printf("(program\n");
        print_block(node->as.program, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;

    case NODE_IMPORT:
        printf("(import ");
        for (int i = 0; i < node->as.import.path_count; i++) {
            if (i > 0) printf(".");
            print_ident(node->as.import.path[i]);
        }
        if (node->as.import.is_glob) {
            printf(".*");
        } else if (node->as.import.name_count > 0) {
            printf(".{");
            for (int i = 0; i < node->as.import.name_count; i++) {
                if (i > 0) printf(", ");
                print_ident(node->as.import.names[i].name);
                if (node->as.import.names[i].alias) {
                    printf(" as ");
                    print_ident(*node->as.import.names[i].alias);
                }
            }
            printf("}");
        }
        printf(")\n");
        break;

    case NODE_TYPE_DECL:
        printf("(type ");
        print_ident(node->as.type_decl.name);
        if (node->as.type_decl.type_params.count > 0) {
            printf("[");
            for (int i = 0; i < node->as.type_decl.type_params.count; i++) {
                if (i > 0) printf(", ");
                print_ident(node->as.type_decl.type_params.items[i].name);
            }
            printf("]");
        }
        printf("\n");
        switch (node->as.type_decl.body_kind) {
            case TYPE_BODY_RECORD:
                for (int i = 0; i < node->as.type_decl.body.record.count; i++) {
                    print_indent(indent + 1);
                    FieldDef *f = &node->as.type_decl.body.record.fields[i];
                    print_ident(f->name);
                    printf(": ");
                    ast_print_type(f->type);
                    printf("\n");
                }
                break;
            case TYPE_BODY_ENUM:
                for (int i = 0; i < node->as.type_decl.body.enumt.count; i++) {
                    print_indent(indent + 1);
                    VariantDef *v = &node->as.type_decl.body.enumt.variants[i];
                    print_ident(v->name);
                    if (v->field_count > 0) {
                        printf(v->is_record ? " { " : "(");
                        for (int j = 0; j < v->field_count; j++) {
                            if (j > 0) printf(", ");
                            if (v->fields[j].name.name) {
                                print_ident(v->fields[j].name);
                                printf(": ");
                            }
                            ast_print_type(v->fields[j].type);
                        }
                        printf(v->is_record ? " }" : ")");
                    }
                    printf("\n");
                }
                break;
            case TYPE_BODY_ALIAS:
                print_indent(indent + 1);
                printf("= ");
                ast_print_type(node->as.type_decl.body.alias);
                printf("\n");
                break;
        }
        print_indent(indent);
        printf(")\n");
        break;

    case NODE_TRAIT_DECL:
        printf("(trait ");
        print_ident(node->as.trait_decl.name);
        printf("\n");
        print_block(node->as.trait_decl.members, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;

    case NODE_FN_DECL:
        printf("(%s ", node->as.fn_decl.is_signature ? "fn-sig" : "fn");
        print_ident(node->as.fn_decl.name);
        printf("(");
        for (int i = 0; i < node->as.fn_decl.params.count; i++) {
            if (i > 0) printf(", ");
            Param *p = &node->as.fn_decl.params.items[i];
            if (p->is_self) { printf("self"); continue; }
            if (p->pattern) ast_print_pattern(p->pattern);
            if (p->type) { printf(": "); ast_print_type(p->type); }
        }
        printf(")");
        if (node->as.fn_decl.ret_type) {
            printf(" -> ");
            ast_print_type(node->as.fn_decl.ret_type);
        }
        if (!node->as.fn_decl.is_signature) {
            printf("\n");
            print_block(node->as.fn_decl.body, indent + 1);
            print_indent(indent);
        }
        printf(")\n");
        break;

    case NODE_METHOD_DECL:
        printf("(method ");
        for (int i = 0; i < node->as.method_decl.type_path.seg_count; i++) {
            if (i > 0) printf(".");
            print_ident(node->as.method_decl.type_path.segments[i]);
        }
        printf(".");
        print_ident(node->as.method_decl.method_name);
        printf("(");
        for (int i = 0; i < node->as.method_decl.params.count; i++) {
            if (i > 0) printf(", ");
            Param *p = &node->as.method_decl.params.items[i];
            if (p->is_self) { printf("self"); continue; }
            if (p->pattern) ast_print_pattern(p->pattern);
            if (p->type) { printf(": "); ast_print_type(p->type); }
        }
        printf(")");
        if (node->as.method_decl.ret_type) {
            printf(" -> ");
            ast_print_type(node->as.method_decl.ret_type);
        }
        printf("\n");
        print_block(node->as.method_decl.body, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;

    case NODE_CONST_DECL:
        printf("(const ");
        print_ident(node->as.const_decl.name);
        if (node->as.const_decl.type) {
            printf(": ");
            ast_print_type(node->as.const_decl.type);
        }
        printf(" = ");
        ast_print(node->as.const_decl.value, 0);
        printf(")\n");
        break;

    case NODE_EXTERN_BLOCK:
        printf("(extern \"%.*s\"\n", node->as.extern_block.abi_len,
               node->as.extern_block.abi);
        print_block(node->as.extern_block.decls, indent + 1);
        print_indent(indent);
        printf(")\n");
        break;

    case NODE_EXTERN_FN:
        printf("(extern-fn ");
        print_ident(node->as.extern_fn.name);
        printf("(");
        for (int i = 0; i < node->as.extern_fn.param_count; i++) {
            if (i > 0) printf(", ");
            ExternParam *p = &node->as.extern_fn.params[i];
            if (p->is_variadic) { printf("..."); continue; }
            if (p->name) { print_ident(*p->name); printf(": "); }
            ast_print_type(p->type);
        }
        printf(")");
        if (node->as.extern_fn.ret_type) {
            printf(" -> ");
            ast_print_type(node->as.extern_fn.ret_type);
        }
        printf(")\n");
        break;

    case NODE_LET:
        printf("(let ");
        ast_print_pattern(node->as.let.pattern);
        if (node->as.let.type) { printf(": "); ast_print_type(node->as.let.type); }
        printf(" = ");
        ast_print(node->as.let.value, 0);
        printf(")\n");
        break;

    case NODE_VAR:
        printf("(var ");
        ast_print_pattern(node->as.let.pattern);
        if (node->as.let.type) { printf(": "); ast_print_type(node->as.let.type); }
        printf(" = ");
        ast_print(node->as.let.value, 0);
        printf(")\n");
        break;

    case NODE_ASSIGN:
        printf("(assign %s ", token_kind_str(node->as.assign.op));
        ast_print(node->as.assign.target, 0);
        printf(" ");
        ast_print(node->as.assign.value, 0);
        printf(")\n");
        break;

    case NODE_RETURN:
        printf("(return");
        if (node->as.unary_stmt.value) { printf(" "); ast_print(node->as.unary_stmt.value, 0); }
        printf(")\n");
        break;

    case NODE_BREAK:
        printf("(break");
        if (node->as.break_stmt.label) { printf(" '"); print_ident(*node->as.break_stmt.label); }
        if (node->as.break_stmt.value) { printf(" "); ast_print(node->as.break_stmt.value, 0); }
        printf(")\n");
        break;

    case NODE_CONTINUE:
        printf("(continue");
        if (node->as.continue_stmt.label) { printf(" '"); print_ident(*node->as.continue_stmt.label); }
        printf(")\n");
        break;

    case NODE_DEFER:
        printf("(defer ");
        ast_print(node->as.defer.body, 0);
        printf(")\n");
        break;

    case NODE_ASSERT:
        printf("(assert ");
        ast_print(node->as.assert_stmt.cond, 0);
        if (node->as.assert_stmt.message) {
            printf(" ");
            ast_print(node->as.assert_stmt.message, 0);
        }
        printf(")\n");
        break;

    case NODE_EXPR_STMT:
        ast_print(node->as.expr_stmt.expr, indent);
        break;

    case NODE_BINARY:
        printf("(%s ", token_kind_str(node->as.binary.op));
        ast_print(node->as.binary.left, 0);
        printf(" ");
        ast_print(node->as.binary.right, 0);
        printf(")");
        break;

    case NODE_UNARY:
        printf("(%s ", token_kind_str(node->as.unary.op));
        ast_print(node->as.unary.operand, 0);
        printf(")");
        break;

    case NODE_CALL:
        printf("(call ");
        ast_print(node->as.call.callee, 0);
        for (int i = 0; i < node->as.call.args.count; i++) {
            printf(" ");
            Arg *a = &node->as.call.args.items[i];
            if (a->name) { print_ident(*a->name); printf(":"); }
            ast_print(a->value, 0);
        }
        printf(")");
        break;

    case NODE_FIELD_ACCESS:
        printf("(. ");
        ast_print(node->as.field_access.object, 0);
        printf(" ");
        print_ident(node->as.field_access.field);
        printf(")");
        break;

    case NODE_INDEX:
        printf("([] ");
        ast_print(node->as.index.object, 0);
        printf(" ");
        ast_print(node->as.index.index, 0);
        printf(")");
        break;

    case NODE_TRY:
        printf("(? ");
        ast_print(node->as.postfix.operand, 0);
        printf(")");
        break;

    case NODE_MUTATE:
        printf("(! ");
        ast_print(node->as.postfix.operand, 0);
        printf(")");
        break;

    case NODE_PIPE:
        printf("(|> ");
        ast_print(node->as.pipe.left, 0);
        printf(" ");
        ast_print(node->as.pipe.right, 0);
        printf(")");
        break;

    case NODE_IF:
        if (node->as.if_expr.is_inline) {
            printf("(if ");
            ast_print(node->as.if_expr.cond, 0);
            printf(" then ");
            ast_print(node->as.if_expr.then_expr, 0);
            printf(" else ");
            ast_print(node->as.if_expr.else_expr, 0);
            printf(")");
        } else {
            printf("(if ");
            ast_print(node->as.if_expr.cond, 0);
            printf("\n");
            print_block(node->as.if_expr.then_block, indent + 1);
            for (int i = 0; i < node->as.if_expr.elif_count; i++) {
                print_indent(indent);
                printf("elif ");
                ast_print(node->as.if_expr.elifs[i].cond, 0);
                printf("\n");
                print_block(node->as.if_expr.elifs[i].block, indent + 1);
            }
            if (node->as.if_expr.else_block.count > 0) {
                print_indent(indent);
                printf("else\n");
                print_block(node->as.if_expr.else_block, indent + 1);
            }
            print_indent(indent);
            printf(")");
        }
        break;

    case NODE_MATCH:
        printf("(match ");
        ast_print(node->as.match.subject, 0);
        printf("\n");
        for (int i = 0; i < node->as.match.arm_count; i++) {
            print_indent(indent + 1);
            ast_print_pattern(node->as.match.arms[i].pattern);
            if (node->as.match.arms[i].guard) {
                printf(" if ");
                ast_print(node->as.match.arms[i].guard, 0);
            }
            printf(" -> ");
            ast_print(node->as.match.arms[i].body, 0);
            printf("\n");
        }
        print_indent(indent);
        printf(")");
        break;

    case NODE_FOR:
        printf("(for ");
        if (node->as.for_expr.label) { printf("'"); print_ident(*node->as.for_expr.label); printf(" "); }
        ast_print_pattern(node->as.for_expr.pattern);
        printf(" in ");
        ast_print(node->as.for_expr.iter, 0);
        printf("\n");
        print_block(node->as.for_expr.body, indent + 1);
        print_indent(indent);
        printf(")");
        break;

    case NODE_WHILE:
        printf("(while ");
        if (node->as.while_expr.label) { printf("'"); print_ident(*node->as.while_expr.label); printf(" "); }
        ast_print(node->as.while_expr.cond, 0);
        printf("\n");
        print_block(node->as.while_expr.body, indent + 1);
        print_indent(indent);
        printf(")");
        break;

    case NODE_LOOP:
        printf("(loop");
        if (node->as.loop_expr.label) { printf(" '"); print_ident(*node->as.loop_expr.label); }
        printf("\n");
        print_block(node->as.loop_expr.body, indent + 1);
        print_indent(indent);
        printf(")");
        break;

    case NODE_SPAWN:
        printf("(spawn ");
        ast_print(node->as.unary_stmt.value, 0);
        printf(")");
        break;

    case NODE_AWAIT:
        printf("(await ");
        ast_print(node->as.unary_stmt.value, 0);
        printf(")");
        break;

    case NODE_SCOPE:
        printf("(scope ");
        ast_print(node->as.unary_stmt.value, 0);
        printf(")");
        break;

    case NODE_COMPTIME:
        printf("(comptime ");
        if (node->as.unary_stmt.value) {
            ast_print(node->as.unary_stmt.value, 0);
        }
        printf(")");
        break;

    case NODE_LAMBDA:
        printf("(lambda (");
        for (int i = 0; i < node->as.lambda.params.count; i++) {
            if (i > 0) printf(", ");
            Param *p = &node->as.lambda.params.items[i];
            if (p->pattern) ast_print_pattern(p->pattern);
            if (p->type) { printf(": "); ast_print_type(p->type); }
        }
        printf(") ");
        if (node->as.lambda.body_expr) {
            ast_print(node->as.lambda.body_expr, 0);
        } else {
            printf("\n");
            print_block(node->as.lambda.body_block, indent + 1);
            print_indent(indent);
        }
        printf(")");
        break;

    case NODE_BLOCK:
    case NODE_DO_BLOCK:
        printf("(do\n");
        print_block(node->as.block, indent + 1);
        print_indent(indent);
        printf(")");
        break;

    case NODE_LITERAL:
        printf("%.*s", node->as.literal.len, node->as.literal.value);
        break;

    case NODE_IDENT:
        print_ident(node->as.ident);
        break;

    case NODE_UPPER_IDENT:
        for (int i = 0; i < node->as.type_path.seg_count; i++) {
            if (i > 0) printf(".");
            print_ident(node->as.type_path.segments[i]);
        }
        break;

    case NODE_TUPLE:
        printf("(tuple");
        for (int i = 0; i < node->as.list.count; i++) {
            printf(" ");
            ast_print(node->as.list.items[i], 0);
        }
        printf(")");
        break;

    case NODE_LIST:
        printf("[");
        for (int i = 0; i < node->as.list.count; i++) {
            if (i > 0) printf(", ");
            ast_print(node->as.list.items[i], 0);
        }
        printf("]");
        break;

    case NODE_STRING_INTERP:
        printf("(str-interp ");
        for (int i = 0; i < node->as.list.count; i++) {
            if (i > 0) printf(" ");
            ast_print(node->as.list.items[i], 0);
        }
        printf(")");
        break;

    case NODE_LIST_COMP:
        printf("(list-comp ");
        ast_print(node->as.list_comp.expr, 0);
        printf(" for ");
        ast_print_pattern(node->as.list_comp.pattern);
        printf(" in ");
        ast_print(node->as.list_comp.iter, 0);
        if (node->as.list_comp.filter) {
            printf(" if ");
            ast_print(node->as.list_comp.filter, 0);
        }
        printf(")");
        break;

    case NODE_MAP:
        printf("{");
        for (int i = 0; i < node->as.map.count; i++) {
            if (i > 0) printf(", ");
            MapEntry *e = &node->as.map.entries[i];
            if (e->is_spread) {
                printf("..");
                ast_print(e->value, 0);
            } else if (e->key) {
                ast_print(e->key, 0);
                printf(": ");
                ast_print(e->value, 0);
            } else {
                ast_print(e->value, 0);
            }
        }
        printf("}");
        break;

    case NODE_MAP_COMP:
        printf("(map-comp ");
        ast_print(node->as.map_comp.key_expr, 0);
        printf(": ");
        ast_print(node->as.map_comp.val_expr, 0);
        printf(" for ");
        ast_print_pattern(node->as.map_comp.pattern);
        printf(" in ");
        ast_print(node->as.map_comp.iter, 0);
        if (node->as.map_comp.filter) {
            printf(" if ");
            ast_print(node->as.map_comp.filter, 0);
        }
        printf(")");
        break;

    case NODE_WITH:
        printf("(with ");
        ast_print(node->as.with.base, 0);
        printf(" {");
        for (int i = 0; i < node->as.with.update_count; i++) {
            if (i > 0) printf(", ");
            print_ident(node->as.with.updates[i].name);
            if (node->as.with.updates[i].value) {
                printf(": ");
                ast_print(node->as.with.updates[i].value, 0);
            }
        }
        printf("})");
        break;

    case NODE_RANGE:
        printf("(%s ", node->as.range.inclusive ? "..=" : "..");
        ast_print(node->as.range.start, 0);
        printf(" ");
        ast_print(node->as.range.end, 0);
        printf(")");
        break;

    case NODE_ATTRIBUTE:
        printf("(@");
        print_ident(node->as.attribute.name);
        if (node->as.attribute.arg_count > 0) {
            printf("(");
            for (int i = 0; i < node->as.attribute.arg_count; i++) {
                if (i > 0) printf(", ");
                if (node->as.attribute.args[i].name) {
                    print_ident(*node->as.attribute.args[i].name);
                    printf(": ");
                }
                ast_print(node->as.attribute.args[i].value, 0);
            }
            printf(")");
        }
        printf(")");
        break;

    case NODE_PLACEHOLDER:
        printf("_");
        break;
    }
}
