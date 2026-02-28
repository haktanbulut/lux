#include "codegen.h"
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Target.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ===== Intern helper for codegen ===== */

static const char *cg_intern(Codegen *cg, const char *name, int len) {
    return str_intern_range(cg->interns, name, len);
}

/* ===== Value scope management ===== */

static unsigned hash_name_ptr(const char *p) {
    uintptr_t v = (uintptr_t)p;
    v = (v >> 3) * 2654435761U;
    return (unsigned)(v & (VALUEMAP_BUCKETS - 1));
}

static void push_value_scope(Codegen *cg) {
    ValueScope *vs = (ValueScope *)arena_alloc(cg->arena, sizeof(ValueScope));
    memset(vs, 0, sizeof(ValueScope));
    vs->parent = cg->values;
    cg->values = vs;
}

static void pop_value_scope(Codegen *cg) {
    if (cg->values) cg->values = cg->values->parent;
}

static void set_value(Codegen *cg, const char *name, LLVMValueRef val) {
    unsigned h = hash_name_ptr(name);
    ValueEntry *e = (ValueEntry *)arena_alloc(cg->arena, sizeof(ValueEntry));
    e->name = name;
    e->value = val;
    e->next = cg->values->buckets[h];
    cg->values->buckets[h] = e;
}

static LLVMValueRef get_value(Codegen *cg, const char *name) {
    unsigned h = hash_name_ptr(name);
    for (ValueScope *vs = cg->values; vs; vs = vs->parent) {
        for (ValueEntry *e = vs->buckets[h]; e; e = e->next) {
            if (e->name == name) return e->value;
        }
    }
    return NULL;
}

/* ===== Loop stack ===== */

static void push_loop(Codegen *cg, LLVMBasicBlockRef cond_bb, LLVMBasicBlockRef exit_bb) {
    LoopCtx *lc = (LoopCtx *)arena_alloc(cg->arena, sizeof(LoopCtx));
    lc->cond_bb = cond_bb;
    lc->exit_bb = exit_bb;
    lc->parent = cg->loop_stack;
    cg->loop_stack = lc;
}

static void pop_loop(Codegen *cg) {
    if (cg->loop_stack) cg->loop_stack = cg->loop_stack->parent;
}

/* ===== Type mapping ===== */

/* Resolve TY_PARAM through the active substitution map */
static Type *cg_resolve_param(Codegen *cg, Type *t) {
    t = type_resolve(t);
    if (!t) return NULL;
    if (t->kind == TY_PARAM && cg->type_substs) {
        for (int i = 0; i < cg->type_subst_count; i++) {
            if (t->as.param.name == cg->type_substs[i].name)
                return cg->type_substs[i].concrete;
        }
    }
    return t;
}

static LLVMTypeRef llvm_type(Codegen *cg, Type *t) {
    t = type_resolve(t);
    if (!t) return LLVMVoidTypeInContext(cg->ctx);

    /* Resolve type parameters through substitution map */
    if (t->kind == TY_PARAM) {
        t = cg_resolve_param(cg, t);
        if (!t || t->kind == TY_PARAM) return LLVMInt64TypeInContext(cg->ctx); /* fallback */
    }

    switch (t->kind) {
    case TY_INT:    return LLVMInt64TypeInContext(cg->ctx);
    case TY_FLOAT:  return LLVMDoubleTypeInContext(cg->ctx);
    case TY_BOOL:   return LLVMInt1TypeInContext(cg->ctx);
    case TY_CHAR:   return LLVMInt8TypeInContext(cg->ctx);
    case TY_STRING: return LLVMPointerTypeInContext(cg->ctx, 0);
    case TY_NONE:   return LLVMVoidTypeInContext(cg->ctx);
    case TY_NEVER:  return LLVMVoidTypeInContext(cg->ctx);
    case TY_PTR:    return LLVMPointerTypeInContext(cg->ctx, 0);
    case TY_FN:
        /* Function values are always pointers (opaque ptr) */
        return LLVMPointerTypeInContext(cg->ctx, 0);
    case TY_LIST: {
        /* List representation: { i64 len, i64 cap, ptr data } */
        LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
        LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
        LLVMTypeRef fields[] = { i64_ty, i64_ty, ptr_ty };
        return LLVMStructTypeInContext(cg->ctx, fields, 3, 0);
    }
    case TY_MAP: {
        /* Map representation: { i64 len, i64 cap, ptr keys, ptr vals } */
        LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
        LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
        LLVMTypeRef fields[] = { i64_ty, i64_ty, ptr_ty, ptr_ty };
        return LLVMStructTypeInContext(cg->ctx, fields, 4, 0);
    }
    case TY_ENUM: {
        /* Check if we already have a named struct for this */
        LLVMTypeRef existing = LLVMGetTypeByName2(cg->ctx, t->as.enumt.name);
        if (existing) return existing;

        /* Compute max payload size */
        LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
        unsigned long long max_size = 0;
        for (int i = 0; i < t->as.enumt.variant_count; i++) {
            VariantInfo *vi = &t->as.enumt.variants[i];
            unsigned long long size = 0;
            for (int j = 0; j < vi->payload_count; j++) {
                LLVMTypeRef pt = llvm_type(cg, vi->payload_types[j]);
                size += LLVMABISizeOfType(td, pt);
            }
            if (size > max_size) max_size = size;
        }

        LLVMTypeRef st = LLVMStructCreateNamed(cg->ctx, t->as.enumt.name);
        LLVMTypeRef tag_ty = LLVMInt32TypeInContext(cg->ctx);
        if (max_size > 0) {
            LLVMTypeRef payload_ty = LLVMArrayType2(LLVMInt8TypeInContext(cg->ctx), max_size);
            LLVMTypeRef fields[] = { tag_ty, payload_ty };
            LLVMStructSetBody(st, fields, 2, 0);
        } else {
            LLVMStructSetBody(st, &tag_ty, 1, 0);
        }
        return st;
    }
    case TY_VARIANT: {
        /* A variant has the same representation as its parent enum */
        return llvm_type(cg, t->as.variant.parent_enum);
    }
    case TY_RECORD: {
        /* Check if we already have a named struct for this */
        LLVMTypeRef existing = LLVMGetTypeByName2(cg->ctx, t->as.record.name);
        if (existing) return existing;

        int n = t->as.record.field_count;
        LLVMTypeRef st = LLVMStructCreateNamed(cg->ctx, t->as.record.name);
        LLVMTypeRef *fields = (LLVMTypeRef *)malloc(n * sizeof(LLVMTypeRef));
        for (int i = 0; i < n; i++) {
            fields[i] = llvm_type(cg, t->as.record.field_types[i]);
        }
        LLVMStructSetBody(st, fields, n, 0);
        free(fields);
        return st;
    }
    case TY_TUPLE: {
        int n = t->as.tuple.count;
        LLVMTypeRef *elems = (LLVMTypeRef *)malloc(n * sizeof(LLVMTypeRef));
        for (int i = 0; i < n; i++) {
            elems[i] = llvm_type(cg, t->as.tuple.elems[i]);
        }
        LLVMTypeRef st = LLVMStructTypeInContext(cg->ctx, elems, n, 0);
        free(elems);
        return st;
    }
    case TY_TASK: {
        /* Task representation: { ptr task_handle, ptr data } */
        LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
        LLVMTypeRef fields[] = { ptr_ty, ptr_ty };
        return LLVMStructTypeInContext(cg->ctx, fields, 2, 0);
    }
    case TY_ERROR:
        return LLVMInt64TypeInContext(cg->ctx);
    default:
        return LLVMInt64TypeInContext(cg->ctx);
    }
}

/* Return the LLVM function signature type for a TY_FN (for LLVMBuildCall2 etc.) */
static LLVMTypeRef llvm_fn_sig_type(Codegen *cg, Type *t) {
    t = type_resolve(t);
    if (t->kind != TY_FN) return NULL;
    LLVMTypeRef ret = (t->as.fn.ret->kind == TY_NONE || t->as.fn.ret->kind == TY_NEVER)
                      ? LLVMVoidTypeInContext(cg->ctx)
                      : llvm_type(cg, t->as.fn.ret);
    int n = t->as.fn.param_count;
    LLVMTypeRef *params = NULL;
    if (n > 0) {
        params = (LLVMTypeRef *)malloc(n * sizeof(LLVMTypeRef));
        for (int i = 0; i < n; i++)
            params[i] = llvm_type(cg, t->as.fn.param_types[i]);
    }
    LLVMTypeRef fn_ty = LLVMFunctionType(ret, params, n, t->as.fn.is_variadic);
    free(params);
    return fn_ty;
}

/* ===== Check if basic block has terminator ===== */

static bool block_has_terminator(LLVMBasicBlockRef bb) {
    return LLVMGetBasicBlockTerminator(bb) != NULL;
}

/* ===== Runtime helpers (malloc, realloc) ===== */

static LLVMValueRef get_or_declare_malloc(Codegen *cg) {
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "malloc");
    if (fn) return fn;
    LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
    LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
    LLVMTypeRef fn_ty = LLVMFunctionType(ptr_ty, &i64_ty, 1, 0);
    return LLVMAddFunction(cg->module, "malloc", fn_ty);
}

static LLVMValueRef get_or_declare_realloc(Codegen *cg) {
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "realloc");
    if (fn) return fn;
    LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
    LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
    LLVMTypeRef params[] = { ptr_ty, i64_ty };
    LLVMTypeRef fn_ty = LLVMFunctionType(ptr_ty, params, 2, 0);
    return LLVMAddFunction(cg->module, "realloc", fn_ty);
}

static LLVMValueRef get_or_declare_free(Codegen *cg) {
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "free");
    if (fn) return fn;
    LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
    LLVMTypeRef void_ty = LLVMVoidTypeInContext(cg->ctx);
    LLVMTypeRef fn_ty = LLVMFunctionType(void_ty, &ptr_ty, 1, 0);
    return LLVMAddFunction(cg->module, "free", fn_ty);
}

/* Coroutine runtime: lux_coro_spawn(void (*thunk)(void*), void *data) -> ptr */
static LLVMValueRef get_or_declare_coro_spawn(Codegen *cg) {
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "lux_coro_spawn");
    if (fn) return fn;
    LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
    LLVMTypeRef params[] = { ptr_ty, ptr_ty };
    LLVMTypeRef fn_ty = LLVMFunctionType(ptr_ty, params, 2, 0);
    return LLVMAddFunction(cg->module, "lux_coro_spawn", fn_ty);
}

/* Coroutine runtime: lux_coro_await(LuxTask *task) -> void */
static LLVMValueRef get_or_declare_coro_await(Codegen *cg) {
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "lux_coro_await");
    if (fn) return fn;
    LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
    LLVMTypeRef void_ty = LLVMVoidTypeInContext(cg->ctx);
    LLVMTypeRef fn_ty = LLVMFunctionType(void_ty, &ptr_ty, 1, 0);
    return LLVMAddFunction(cg->module, "lux_coro_await", fn_ty);
}

/* Coroutine runtime: lux_coro_destroy(LuxTask *task) -> void */
static LLVMValueRef get_or_declare_coro_destroy(Codegen *cg) {
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "lux_coro_destroy");
    if (fn) return fn;
    LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
    LLVMTypeRef void_ty = LLVMVoidTypeInContext(cg->ctx);
    LLVMTypeRef fn_ty = LLVMFunctionType(void_ty, &ptr_ty, 1, 0);
    return LLVMAddFunction(cg->module, "lux_coro_destroy", fn_ty);
}

/* ===== Expression codegen ===== */

static LLVMValueRef codegen_expr(Codegen *cg, AstNode *node);
static void codegen_stmt(Codegen *cg, AstNode *node);
static void codegen_block(Codegen *cg, NodeList block);
static LLVMValueRef create_entry_alloca(Codegen *cg, LLVMTypeRef ty, const char *name);
static void codegen_pattern_bind(Codegen *cg, AstPattern *pat, LLVMValueRef val, Type *type);
static Type *cg_resolve_ast_type(Codegen *cg, AstType *at);
static AstNode *find_generic_fn(Codegen *cg, const char *name);
static LLVMValueRef codegen_monomorphize(Codegen *cg, AstNode *generic_fn,
                                          Type **concrete_types, int type_count);
static void codegen_pattern_test(Codegen *cg, AstPattern *pat, LLVMValueRef val, Type *type,
                                 LLVMBasicBlockRef match_bb, LLVMBasicBlockRef fail_bb);

static LLVMValueRef codegen_expr(Codegen *cg, AstNode *node) {
    if (!node) return NULL;

    switch (node->kind) {
    case NODE_LITERAL: {
        switch (node->as.literal.kind) {
        case LIT_INT: {
            long long val = strtoll(node->as.literal.value, NULL, 0);
            return LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), (unsigned long long)val, 1);
        }
        case LIT_FLOAT: {
            double val = strtod(node->as.literal.value, NULL);
            return LLVMConstReal(LLVMDoubleTypeInContext(cg->ctx), val);
        }
        case LIT_BOOL: {
            int val = (node->as.literal.len == 4 &&
                       memcmp(node->as.literal.value, "true", 4) == 0) ? 1 : 0;
            return LLVMConstInt(LLVMInt1TypeInContext(cg->ctx), val, 0);
        }
        case LIT_STRING: {
            /* String literal — skip surrounding quotes */
            const char *s = node->as.literal.value;
            int len = node->as.literal.len;
            if (len >= 2 && s[0] == '"' && s[len - 1] == '"') {
                s++;
                len -= 2;
            }
            /* Process escape sequences */
            char *buf = (char *)malloc(len + 1);
            int out = 0;
            for (int i = 0; i < len; i++) {
                if (s[i] == '\\' && i + 1 < len) {
                    switch (s[i + 1]) {
                    case 'n': buf[out++] = '\n'; i++; break;
                    case 't': buf[out++] = '\t'; i++; break;
                    case '\\': buf[out++] = '\\'; i++; break;
                    case '"': buf[out++] = '"'; i++; break;
                    default: buf[out++] = s[i]; break;
                    }
                } else {
                    buf[out++] = s[i];
                }
            }
            buf[out] = '\0';
            LLVMValueRef str = LLVMBuildGlobalStringPtr(cg->builder, buf, ".str");
            free(buf);
            return str;
        }
        case LIT_CHAR:
            return LLVMConstInt(LLVMInt8TypeInContext(cg->ctx),
                                (unsigned char)node->as.literal.value[1], 0);
        case LIT_NONE:
            return NULL;
        }
        return NULL;
    }

    case NODE_IDENT: {
        const char *name = cg_intern(cg, node->as.ident.name, node->as.ident.len);
        LLVMValueRef alloca_val = get_value(cg, name);
        if (!alloca_val) {
            /* Could be a function reference */
            LLVMValueRef fn = LLVMGetNamedFunction(cg->module, name);
            if (fn) return fn;
            fprintf(stderr, "codegen: undefined variable '%s'\n", name);
            return LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);
        }
        /* Load from alloca — llvm_type(TY_FN) returns ptr, so fn ptrs work too */
        Type *ty = node->checked_type ? cg_resolve_param(cg, node->checked_type) : NULL;
        LLVMTypeRef load_ty = ty ? llvm_type(cg, ty) : LLVMInt64TypeInContext(cg->ctx);
        return LLVMBuildLoad2(cg->builder, load_ty, alloca_val, name);
    }

    case NODE_BINARY: {
        LLVMValueRef left = codegen_expr(cg, node->as.binary.left);
        LLVMValueRef right = codegen_expr(cg, node->as.binary.right);
        if (!left || !right) return LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);

        Type *op_type = node->as.binary.left->checked_type ?
                        cg_resolve_param(cg, node->as.binary.left->checked_type) : type_int();
        bool is_float = (op_type->kind == TY_FLOAT);

        switch (node->as.binary.op) {
        case TOK_PLUS:
            return is_float ? LLVMBuildFAdd(cg->builder, left, right, "fadd")
                            : LLVMBuildAdd(cg->builder, left, right, "add");
        case TOK_MINUS:
            return is_float ? LLVMBuildFSub(cg->builder, left, right, "fsub")
                            : LLVMBuildSub(cg->builder, left, right, "sub");
        case TOK_STAR:
            return is_float ? LLVMBuildFMul(cg->builder, left, right, "fmul")
                            : LLVMBuildMul(cg->builder, left, right, "mul");
        case TOK_SLASH:
            return is_float ? LLVMBuildFDiv(cg->builder, left, right, "fdiv")
                            : LLVMBuildSDiv(cg->builder, left, right, "sdiv");
        case TOK_PERCENT:
            return is_float ? LLVMBuildFRem(cg->builder, left, right, "frem")
                            : LLVMBuildSRem(cg->builder, left, right, "srem");

        case TOK_EQ:
            return is_float ? LLVMBuildFCmp(cg->builder, LLVMRealOEQ, left, right, "feq")
                            : LLVMBuildICmp(cg->builder, LLVMIntEQ, left, right, "eq");
        case TOK_NEQ:
            return is_float ? LLVMBuildFCmp(cg->builder, LLVMRealONE, left, right, "fne")
                            : LLVMBuildICmp(cg->builder, LLVMIntNE, left, right, "ne");
        case TOK_LT:
            return is_float ? LLVMBuildFCmp(cg->builder, LLVMRealOLT, left, right, "flt")
                            : LLVMBuildICmp(cg->builder, LLVMIntSLT, left, right, "lt");
        case TOK_GT:
            return is_float ? LLVMBuildFCmp(cg->builder, LLVMRealOGT, left, right, "fgt")
                            : LLVMBuildICmp(cg->builder, LLVMIntSGT, left, right, "gt");
        case TOK_LE:
            return is_float ? LLVMBuildFCmp(cg->builder, LLVMRealOLE, left, right, "fle")
                            : LLVMBuildICmp(cg->builder, LLVMIntSLE, left, right, "le");
        case TOK_GE:
            return is_float ? LLVMBuildFCmp(cg->builder, LLVMRealOGE, left, right, "fge")
                            : LLVMBuildICmp(cg->builder, LLVMIntSGE, left, right, "ge");

        case TOK_AND:
            return LLVMBuildAnd(cg->builder, left, right, "and");
        case TOK_OR:
            return LLVMBuildOr(cg->builder, left, right, "or");

        case TOK_AMP:
            return LLVMBuildAnd(cg->builder, left, right, "band");
        case TOK_PIPE:
            return LLVMBuildOr(cg->builder, left, right, "bor");
        case TOK_CARET:
            return LLVMBuildXor(cg->builder, left, right, "bxor");
        case TOK_SHL:
            return LLVMBuildShl(cg->builder, left, right, "shl");
        case TOK_SHR:
            return LLVMBuildAShr(cg->builder, left, right, "shr");

        default:
            return LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);
        }
    }

    case NODE_UNARY: {
        LLVMValueRef operand = codegen_expr(cg, node->as.unary.operand);
        if (!operand) return LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);

        Type *op_type = node->as.unary.operand->checked_type ?
                        cg_resolve_param(cg, node->as.unary.operand->checked_type) : type_int();

        switch (node->as.unary.op) {
        case TOK_MINUS:
            if (op_type->kind == TY_FLOAT) {
                return LLVMBuildFNeg(cg->builder, operand, "fneg");
            }
            return LLVMBuildNeg(cg->builder, operand, "neg");
        case TOK_NOT:
            return LLVMBuildNot(cg->builder, operand, "not");
        case TOK_TILDE:
            return LLVMBuildNot(cg->builder, operand, "bnot");
        default:
            return operand;
        }
    }

    case NODE_CALL: {
        /* Check if this is a record constructor */
        Type *callee_check_type = node->as.call.callee->checked_type ?
                                  type_resolve(node->as.call.callee->checked_type) : NULL;
        if (callee_check_type && callee_check_type->kind == TY_RECORD) {
            LLVMTypeRef struct_ty = llvm_type(cg, callee_check_type);
            LLVMValueRef rec_val = LLVMGetUndef(struct_ty);
            for (int i = 0; i < node->as.call.args.count &&
                 i < callee_check_type->as.record.field_count; i++) {
                LLVMValueRef fval = codegen_expr(cg, node->as.call.args.items[i].value);
                if (fval) {
                    rec_val = LLVMBuildInsertValue(cg->builder, rec_val, fval, i, "rec.ins");
                }
            }
            return rec_val;
        }

        /* Check if this is an enum variant constructor: fn(...) -> EnumType */
        Type *enum_ret = NULL;
        if (callee_check_type && callee_check_type->kind == TY_FN) {
            Type *ret = type_resolve(callee_check_type->as.fn.ret);
            if (ret && ret->kind == TY_ENUM) enum_ret = ret;
        }
        if (enum_ret) {
            const char *vname = NULL;
            if (node->as.call.callee->kind == NODE_UPPER_IDENT &&
                node->as.call.callee->as.type_path.seg_count >= 1) {
                vname = cg_intern(cg,
                    node->as.call.callee->as.type_path.segments[0].name,
                    node->as.call.callee->as.type_path.segments[0].len);
            } else if (node->as.call.callee->kind == NODE_IDENT) {
                vname = cg_intern(cg, node->as.call.callee->as.ident.name,
                                  node->as.call.callee->as.ident.len);
            }

            Type *enum_type = enum_ret;
            int variant_idx = -1;

            if (enum_type && vname) {
                for (int i = 0; i < enum_type->as.enumt.variant_count; i++) {
                    if (enum_type->as.enumt.variants[i].name == vname) {
                        variant_idx = i;
                        break;
                    }
                }
            }

            if (enum_type && variant_idx >= 0) {
                LLVMTypeRef enum_llvm_ty = llvm_type(cg, enum_type);
                LLVMValueRef alloca_val = create_entry_alloca(cg, enum_llvm_ty, "enum.tmp");

                /* Store tag */
                LLVMTypeRef i32_ty = LLVMInt32TypeInContext(cg->ctx);
                LLVMValueRef tag_ptr = LLVMBuildStructGEP2(cg->builder, enum_llvm_ty,
                                                            alloca_val, 0, "tag.ptr");
                LLVMBuildStore(cg->builder, LLVMConstInt(i32_ty, variant_idx, 0), tag_ptr);

                /* Store payload fields */
                VariantInfo *vi = &enum_type->as.enumt.variants[variant_idx];
                if (vi->payload_count > 0) {
                    LLVMValueRef payload_ptr = LLVMBuildStructGEP2(cg->builder, enum_llvm_ty,
                                                                    alloca_val, 1, "payload.ptr");
                    /* Cast to the appropriate type and store each field */
                    unsigned offset = 0;
                    LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
                    for (int i = 0; i < vi->payload_count && i < node->as.call.args.count; i++) {
                        LLVMValueRef arg = codegen_expr(cg, node->as.call.args.items[i].value);
                        if (arg) {
                            LLVMTypeRef field_ty = llvm_type(cg, vi->payload_types[i]);
                            LLVMValueRef idx = LLVMConstInt(LLVMInt32TypeInContext(cg->ctx), offset, 0);
                            LLVMValueRef gep = LLVMBuildGEP2(cg->builder,
                                LLVMInt8TypeInContext(cg->ctx), payload_ptr, &idx, 1, "field.byte");
                            LLVMValueRef field_ptr = LLVMBuildBitCast(cg->builder, gep,
                                LLVMPointerTypeInContext(cg->ctx, 0), "field.ptr");
                            LLVMBuildStore(cg->builder, arg, field_ptr);
                            offset += (unsigned)LLVMABISizeOfType(td, field_ty);
                        }
                    }
                }

                return LLVMBuildLoad2(cg->builder, enum_llvm_ty, alloca_val, "enum.val");
            }
        }

        /* Check for method call: obj.method(args) */
        if (node->as.call.callee->kind == NODE_FIELD_ACCESS) {
            AstNode *obj_node = node->as.call.callee->as.field_access.object;
            Type *obj_type = obj_node->checked_type ? type_resolve(obj_node->checked_type) : NULL;
            const char *field_name = cg_intern(cg,
                node->as.call.callee->as.field_access.field.name,
                node->as.call.callee->as.field_access.field.len);

            /* Try to find mangled method name */
            const char *type_name_str = NULL;
            if (obj_type && obj_type->kind == TY_RECORD) {
                type_name_str = obj_type->as.record.name;
            } else if (obj_type && obj_type->kind == TY_ENUM) {
                type_name_str = obj_type->as.enumt.name;
            }

            if (type_name_str) {
                char mangled[256];
                snprintf(mangled, sizeof(mangled), "%s_%s", type_name_str, field_name);
                const char *interned = str_intern(cg->interns, mangled);
                LLVMValueRef method_fn = LLVMGetNamedFunction(cg->module, interned);

                if (method_fn) {
                    LLVMTypeRef method_fn_ty = LLVMGlobalGetValueType(method_fn);

                    /* Build args: &obj + user args */
                    int user_argc = node->as.call.args.count;
                    int total_argc = user_argc + 1;
                    LLVMValueRef *args = (LLVMValueRef *)malloc(total_argc * sizeof(LLVMValueRef));

                    /* Get pointer to object (alloca) */
                    LLVMValueRef obj_val = codegen_expr(cg, obj_node);
                    if (obj_val) {
                        LLVMTypeRef obj_llvm_ty = llvm_type(cg, obj_type);
                        LLVMValueRef obj_alloca = create_entry_alloca(cg, obj_llvm_ty, "self.tmp");
                        LLVMBuildStore(cg->builder, obj_val, obj_alloca);
                        args[0] = obj_alloca;
                    } else {
                        args[0] = LLVMConstNull(LLVMPointerTypeInContext(cg->ctx, 0));
                    }

                    for (int i = 0; i < user_argc; i++) {
                        args[i + 1] = codegen_expr(cg, node->as.call.args.items[i].value);
                        if (!args[i + 1]) {
                            args[i + 1] = LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);
                        }
                    }

                    Type *ret_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
                    bool is_void = !ret_type || ret_type->kind == TY_NONE;
                    LLVMValueRef result = LLVMBuildCall2(cg->builder, method_fn_ty, method_fn,
                                                          args, total_argc,
                                                          is_void ? "" : "method.call");
                    free(args);
                    return result;
                }
            }
        }

        /* Check for generic function call — monomorphize if needed */
        if (node->as.call.callee->kind == NODE_IDENT) {
            const char *callee_name = cg_intern(cg, node->as.call.callee->as.ident.name,
                                                 node->as.call.callee->as.ident.len);
            AstNode *generic_fn = find_generic_fn(cg, callee_name);
            if (generic_fn) {
                /* Infer concrete type substitutions from argument types */
                Type *callee_type = node->as.call.callee->checked_type ?
                                    type_resolve(node->as.call.callee->checked_type) : NULL;
                int tp_count = generic_fn->as.fn_decl.type_params.count;
                Type **concrete = (Type **)malloc(tp_count * sizeof(Type *));
                /* Initialize to NULL */
                for (int i = 0; i < tp_count; i++) concrete[i] = NULL;

                if (callee_type && callee_type->kind == TY_FN) {
                    /* Match arg checked_types against generic param types to infer */
                    int argc = node->as.call.args.count;
                    for (int ai = 0; ai < argc && ai < callee_type->as.fn.param_count; ai++) {
                        Type *param_t = type_resolve(callee_type->as.fn.param_types[ai]);
                        Type *arg_t = node->as.call.args.items[ai].value->checked_type;
                        if (!arg_t) continue;
                        arg_t = type_resolve(arg_t);
                        if (param_t->kind == TY_PARAM) {
                            /* Find which type param this is */
                            for (int ti = 0; ti < tp_count; ti++) {
                                TypeParam *tp = &generic_fn->as.fn_decl.type_params.items[ti];
                                const char *tpname = cg_intern(cg, tp->name.name, tp->name.len);
                                if (param_t->as.param.name == tpname && !concrete[ti]) {
                                    concrete[ti] = arg_t;
                                }
                            }
                        }
                    }
                }

                /* Default unresolved type params to Int */
                for (int i = 0; i < tp_count; i++) {
                    if (!concrete[i]) concrete[i] = type_int();
                }

                /* Monomorphize */
                LLVMValueRef mono_fn = codegen_monomorphize(cg, generic_fn, concrete, tp_count);
                LLVMTypeRef mono_fn_ty = LLVMGlobalGetValueType(mono_fn);

                /* Build args */
                int argc = node->as.call.args.count;
                LLVMValueRef *args = NULL;
                if (argc > 0) {
                    args = (LLVMValueRef *)malloc(argc * sizeof(LLVMValueRef));
                    for (int i = 0; i < argc; i++) {
                        args[i] = codegen_expr(cg, node->as.call.args.items[i].value);
                        if (!args[i]) args[i] = LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);
                    }
                }

                Type *ret_type = node->checked_type ? cg_resolve_param(cg, node->checked_type) : NULL;
                bool is_void = !ret_type || ret_type->kind == TY_NONE;
                LLVMValueRef result = LLVMBuildCall2(cg->builder, mono_fn_ty, mono_fn,
                                                      args, argc, is_void ? "" : "mono.call");
                free(args);
                free(concrete);
                return result;
            }
        }

        /* Get callee */
        LLVMValueRef callee = NULL;
        LLVMTypeRef fn_ty = NULL;

        if (node->as.call.callee->kind == NODE_IDENT ||
            node->as.call.callee->kind == NODE_UPPER_IDENT) {
            const char *callee_name;
            if (node->as.call.callee->kind == NODE_IDENT) {
                callee_name = cg_intern(cg, node->as.call.callee->as.ident.name,
                                        node->as.call.callee->as.ident.len);
            } else {
                callee_name = cg_intern(cg,
                    node->as.call.callee->as.type_path.segments[0].name,
                    node->as.call.callee->as.type_path.segments[0].len);
            }
            callee = LLVMGetNamedFunction(cg->module, callee_name);
            if (callee) {
                fn_ty = LLVMGlobalGetValueType(callee);
            }
        }

        if (!callee) {
            callee = codegen_expr(cg, node->as.call.callee);
        }
        if (!callee) return LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);

        if (!fn_ty) {
            /* Reconstruct from checked_type — need actual fn signature, not ptr */
            Type *ct = node->as.call.callee->checked_type ?
                       type_resolve(node->as.call.callee->checked_type) : NULL;
            if (ct && ct->kind == TY_FN) {
                fn_ty = llvm_fn_sig_type(cg, ct);
            } else {
                fn_ty = LLVMGlobalGetValueType(callee);
            }
        }

        int argc = node->as.call.args.count;
        LLVMValueRef *args = NULL;
        if (argc > 0) {
            args = (LLVMValueRef *)malloc(argc * sizeof(LLVMValueRef));
            for (int i = 0; i < argc; i++) {
                args[i] = codegen_expr(cg, node->as.call.args.items[i].value);
                if (!args[i]) {
                    args[i] = LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), 0, 0);
                }
            }
        }

        /* Determine if return is void */
        Type *ret_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        bool is_void = !ret_type || ret_type->kind == TY_NONE;

        LLVMValueRef result = LLVMBuildCall2(cg->builder, fn_ty, callee, args, argc,
                                              is_void ? "" : "call");
        free(args);
        return result;
    }

    case NODE_IF: {
        if (node->as.if_expr.is_inline) {
            /* Inline if: value-producing */
            LLVMValueRef cond = codegen_expr(cg, node->as.if_expr.cond);
            /* Ensure cond is i1 */
            Type *cond_type = node->as.if_expr.cond->checked_type ?
                              type_resolve(node->as.if_expr.cond->checked_type) : NULL;
            if (cond_type && cond_type->kind != TY_BOOL) {
                cond = LLVMBuildICmp(cg->builder, LLVMIntNE, cond,
                                     LLVMConstInt(LLVMTypeOf(cond), 0, 0), "tobool");
            }

            LLVMBasicBlockRef then_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "then");
            LLVMBasicBlockRef else_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "else");
            LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "merge");

            LLVMBuildCondBr(cg->builder, cond, then_bb, else_bb);

            LLVMPositionBuilderAtEnd(cg->builder, then_bb);
            LLVMValueRef then_val = codegen_expr(cg, node->as.if_expr.then_expr);
            LLVMBasicBlockRef then_end = LLVMGetInsertBlock(cg->builder);
            if (!block_has_terminator(then_end))
                LLVMBuildBr(cg->builder, merge_bb);

            LLVMPositionBuilderAtEnd(cg->builder, else_bb);
            LLVMValueRef else_val = codegen_expr(cg, node->as.if_expr.else_expr);
            LLVMBasicBlockRef else_end = LLVMGetInsertBlock(cg->builder);
            if (!block_has_terminator(else_end))
                LLVMBuildBr(cg->builder, merge_bb);

            LLVMPositionBuilderAtEnd(cg->builder, merge_bb);

            if (then_val && else_val) {
                LLVMTypeRef phi_ty = LLVMTypeOf(then_val);
                LLVMValueRef phi = LLVMBuildPhi(cg->builder, phi_ty, "iftmp");
                LLVMValueRef incoming_vals[] = { then_val, else_val };
                LLVMBasicBlockRef incoming_bbs[] = { then_end, else_end };
                LLVMAddIncoming(phi, incoming_vals, incoming_bbs, 2);
                return phi;
            }
            return NULL;
        } else {
            /* Block if: statement-like, with elif chains */
            bool has_else = node->as.if_expr.else_block.count > 0;
            int elif_count = node->as.if_expr.elif_count;
            LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "ifend");

            /* Main if condition */
            LLVMValueRef cond = codegen_expr(cg, node->as.if_expr.cond);
            Type *cond_type = node->as.if_expr.cond->checked_type ?
                              type_resolve(node->as.if_expr.cond->checked_type) : NULL;
            if (cond_type && cond_type->kind != TY_BOOL) {
                cond = LLVMBuildICmp(cg->builder, LLVMIntNE, cond,
                                     LLVMConstInt(LLVMTypeOf(cond), 0, 0), "tobool");
            }

            LLVMBasicBlockRef then_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "then");
            LLVMBasicBlockRef next_bb;

            if (elif_count > 0) {
                next_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "elif.cond");
            } else if (has_else) {
                next_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "else");
            } else {
                next_bb = merge_bb;
            }

            LLVMBuildCondBr(cg->builder, cond, then_bb, next_bb);

            /* Then block */
            LLVMPositionBuilderAtEnd(cg->builder, then_bb);
            push_value_scope(cg);
            codegen_block(cg, node->as.if_expr.then_block);
            pop_value_scope(cg);
            if (!block_has_terminator(LLVMGetInsertBlock(cg->builder)))
                LLVMBuildBr(cg->builder, merge_bb);

            /* Elif chains */
            for (int i = 0; i < elif_count; i++) {
                LLVMPositionBuilderAtEnd(cg->builder, next_bb);
                LLVMValueRef elif_cond = codegen_expr(cg, node->as.if_expr.elifs[i].cond);
                Type *elif_cond_type = node->as.if_expr.elifs[i].cond->checked_type ?
                                       type_resolve(node->as.if_expr.elifs[i].cond->checked_type) : NULL;
                if (elif_cond_type && elif_cond_type->kind != TY_BOOL) {
                    elif_cond = LLVMBuildICmp(cg->builder, LLVMIntNE, elif_cond,
                                              LLVMConstInt(LLVMTypeOf(elif_cond), 0, 0), "tobool");
                }

                LLVMBasicBlockRef elif_body = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "elif.body");

                if (i + 1 < elif_count) {
                    next_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "elif.cond");
                } else if (has_else) {
                    next_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "else");
                } else {
                    next_bb = merge_bb;
                }

                LLVMBuildCondBr(cg->builder, elif_cond, elif_body, next_bb);

                LLVMPositionBuilderAtEnd(cg->builder, elif_body);
                push_value_scope(cg);
                codegen_block(cg, node->as.if_expr.elifs[i].block);
                pop_value_scope(cg);
                if (!block_has_terminator(LLVMGetInsertBlock(cg->builder)))
                    LLVMBuildBr(cg->builder, merge_bb);
            }

            /* Else block */
            if (has_else) {
                LLVMPositionBuilderAtEnd(cg->builder, next_bb);
                push_value_scope(cg);
                codegen_block(cg, node->as.if_expr.else_block);
                pop_value_scope(cg);
                if (!block_has_terminator(LLVMGetInsertBlock(cg->builder)))
                    LLVMBuildBr(cg->builder, merge_bb);
            }

            LLVMPositionBuilderAtEnd(cg->builder, merge_bb);
            return NULL;
        }
    }

    case NODE_WHILE: {
        LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "while.cond");
        LLVMBasicBlockRef body_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "while.body");
        LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "while.exit");

        LLVMBuildBr(cg->builder, cond_bb);

        LLVMPositionBuilderAtEnd(cg->builder, cond_bb);
        LLVMValueRef cond = codegen_expr(cg, node->as.while_expr.cond);
        Type *cond_type = node->as.while_expr.cond->checked_type ?
                          type_resolve(node->as.while_expr.cond->checked_type) : NULL;
        if (cond_type && cond_type->kind != TY_BOOL) {
            cond = LLVMBuildICmp(cg->builder, LLVMIntNE, cond,
                                 LLVMConstInt(LLVMTypeOf(cond), 0, 0), "tobool");
        }
        LLVMBuildCondBr(cg->builder, cond, body_bb, exit_bb);

        LLVMPositionBuilderAtEnd(cg->builder, body_bb);
        push_loop(cg, cond_bb, exit_bb);
        push_value_scope(cg);
        codegen_block(cg, node->as.while_expr.body);
        pop_value_scope(cg);
        pop_loop(cg);
        if (!block_has_terminator(LLVMGetInsertBlock(cg->builder)))
            LLVMBuildBr(cg->builder, cond_bb);

        LLVMPositionBuilderAtEnd(cg->builder, exit_bb);
        return NULL;
    }

    case NODE_LOOP: {
        LLVMBasicBlockRef body_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "loop.body");
        LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "loop.exit");

        LLVMBuildBr(cg->builder, body_bb);

        LLVMPositionBuilderAtEnd(cg->builder, body_bb);
        push_loop(cg, NULL, exit_bb);
        push_value_scope(cg);
        codegen_block(cg, node->as.loop_expr.body);
        pop_value_scope(cg);
        pop_loop(cg);
        if (!block_has_terminator(LLVMGetInsertBlock(cg->builder)))
            LLVMBuildBr(cg->builder, body_bb);

        LLVMPositionBuilderAtEnd(cg->builder, exit_bb);
        return NULL;
    }

    case NODE_UPPER_IDENT: {
        if (node->as.type_path.seg_count >= 1) {
            const char *name = cg_intern(cg,
                node->as.type_path.segments[0].name,
                node->as.type_path.segments[0].len);

            /* Check if it's a no-payload enum variant */
            Type *checked = node->checked_type ? type_resolve(node->checked_type) : NULL;
            if (checked && checked->kind == TY_ENUM) {
                /* Find variant index */
                for (int i = 0; i < checked->as.enumt.variant_count; i++) {
                    if (checked->as.enumt.variants[i].name == name) {
                        LLVMTypeRef enum_ty = llvm_type(cg, checked);
                        LLVMValueRef alloca_val = create_entry_alloca(cg, enum_ty, "enum.tmp");
                        LLVMTypeRef i32_ty = LLVMInt32TypeInContext(cg->ctx);
                        LLVMValueRef tag_ptr = LLVMBuildStructGEP2(cg->builder, enum_ty,
                                                                    alloca_val, 0, "tag.ptr");
                        LLVMBuildStore(cg->builder, LLVMConstInt(i32_ty, i, 0), tag_ptr);
                        return LLVMBuildLoad2(cg->builder, enum_ty, alloca_val, "enum.val");
                    }
                }
            }

            /* Function reference */
            LLVMValueRef fn = LLVMGetNamedFunction(cg->module, name);
            if (fn) return fn;
        }
        return NULL;
    }

    case NODE_FIELD_ACCESS: {
        Type *obj_type = node->as.field_access.object->checked_type ?
                         type_resolve(node->as.field_access.object->checked_type) : NULL;

        /* Auto-dereference pointer */
        bool is_ptr = false;
        if (obj_type && obj_type->kind == TY_PTR) {
            obj_type = type_resolve(obj_type->as.ptr.elem);
            is_ptr = true;
        }

        if (obj_type && obj_type->kind == TY_LIST) {
            const char *fname = cg_intern(cg, node->as.field_access.field.name,
                                          node->as.field_access.field.len);
            const char *len_str = str_intern(cg->interns, "len");
            if (fname == len_str) {
                /* list.len — extract field 0 (i64 len) from {i64 len, i64 cap, ptr data} */
                LLVMValueRef obj = codegen_expr(cg, node->as.field_access.object);
                if (!obj) return NULL;
                return LLVMBuildExtractValue(cg->builder, obj, 0, "list.len");
            }
            return NULL;
        }

        if (obj_type && obj_type->kind == TY_RECORD) {
            const char *fname = cg_intern(cg, node->as.field_access.field.name,
                                          node->as.field_access.field.len);
            /* Find field index */
            int field_idx = -1;
            for (int i = 0; i < obj_type->as.record.field_count; i++) {
                if (obj_type->as.record.field_names[i] == fname) {
                    field_idx = i;
                    break;
                }
            }
            if (field_idx < 0) return NULL;

            LLVMTypeRef struct_ty = llvm_type(cg, obj_type);
            LLVMTypeRef field_ty = llvm_type(cg, obj_type->as.record.field_types[field_idx]);

            if (is_ptr) {
                /* self is a pointer — load the pointer, then GEP into it */
                LLVMValueRef ptr_val = codegen_expr(cg, node->as.field_access.object);
                if (!ptr_val) return NULL;
                LLVMValueRef gep = LLVMBuildStructGEP2(cg->builder, struct_ty,
                                                        ptr_val, field_idx, "field.ptr");
                return LLVMBuildLoad2(cg->builder, field_ty, gep, fname);
            }

            /* Get the alloca for the object */
            if (node->as.field_access.object->kind == NODE_IDENT) {
                const char *obj_name = cg_intern(cg,
                    node->as.field_access.object->as.ident.name,
                    node->as.field_access.object->as.ident.len);
                LLVMValueRef obj_alloca = get_value(cg, obj_name);
                if (obj_alloca) {
                    LLVMValueRef gep = LLVMBuildStructGEP2(cg->builder, struct_ty,
                                                            obj_alloca, field_idx, "field.ptr");
                    return LLVMBuildLoad2(cg->builder, field_ty, gep, fname);
                }
            }
            /* Fallback: extract from value */
            LLVMValueRef obj = codegen_expr(cg, node->as.field_access.object);
            if (obj) return LLVMBuildExtractValue(cg->builder, obj, field_idx, fname);
        }
        return NULL;
    }

    case NODE_WITH: {
        /* Record update: copy base, then overwrite updated fields */
        Type *base_type = node->as.with.base->checked_type ?
                          type_resolve(node->as.with.base->checked_type) : NULL;
        if (!base_type || base_type->kind != TY_RECORD) return NULL;

        LLVMValueRef base_val = codegen_expr(cg, node->as.with.base);
        if (!base_val) return NULL;

        /* Start with the base struct value */
        LLVMValueRef result_val = base_val;

        /* Insert updated field values */
        for (int i = 0; i < node->as.with.update_count; i++) {
            FieldUpdate *fu = &node->as.with.updates[i];
            const char *fname = cg_intern(cg, fu->name.name, fu->name.len);
            LLVMValueRef new_val = codegen_expr(cg, fu->value);
            if (!new_val) continue;

            /* Find field index */
            for (int j = 0; j < base_type->as.record.field_count; j++) {
                if (base_type->as.record.field_names[j] == fname) {
                    result_val = LLVMBuildInsertValue(cg->builder, result_val, new_val,
                                                      j, "with.upd");
                    break;
                }
            }
        }
        return result_val;
    }

    case NODE_STRING_INTERP: {
        /* String interpolation: allocate buffer, snprintf each part */
        int part_count = node->as.list.count;

        /* We'll use snprintf to build the string piece by piece.
         * Allocate a buffer, then append each part. */
        LLVMValueRef snprintf_fn = LLVMGetNamedFunction(cg->module, "snprintf");
        if (!snprintf_fn) {
            LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
            LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
            LLVMTypeRef i32_ty = LLVMInt32TypeInContext(cg->ctx);
            LLVMTypeRef params[] = { ptr_ty, i64_ty, ptr_ty };
            LLVMTypeRef fn_ty = LLVMFunctionType(i32_ty, params, 3, 1);
            snprintf_fn = LLVMAddFunction(cg->module, "snprintf", fn_ty);
        }
        LLVMTypeRef snprintf_fn_ty = LLVMGlobalGetValueType(snprintf_fn);

        LLVMValueRef strlen_fn = LLVMGetNamedFunction(cg->module, "strlen");
        if (!strlen_fn) {
            LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
            LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
            LLVMTypeRef fn_ty = LLVMFunctionType(i64_ty, &ptr_ty, 1, 0);
            strlen_fn = LLVMAddFunction(cg->module, "strlen", fn_ty);
        }

        LLVMValueRef malloc_fn = get_or_declare_malloc(cg);
        LLVMTypeRef malloc_fn_ty = LLVMGlobalGetValueType(malloc_fn);

        /* Allocate a 1024-byte buffer */
        LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
        LLVMValueRef buf_size = LLVMConstInt(i64_ty, 1024, 0);
        LLVMValueRef buf = LLVMBuildCall2(cg->builder, malloc_fn_ty, malloc_fn,
                                           &buf_size, 1, "interp.buf");

        /* Track current position in buffer */
        LLVMValueRef pos_alloca = create_entry_alloca(cg, i64_ty, "interp.pos");
        LLVMBuildStore(cg->builder, LLVMConstInt(i64_ty, 0, 0), pos_alloca);

        for (int i = 0; i < part_count; i++) {
            AstNode *part = node->as.list.items[i];
            LLVMValueRef pos = LLVMBuildLoad2(cg->builder, i64_ty, pos_alloca, "pos");
            LLVMValueRef buf_offset = LLVMBuildGEP2(cg->builder,
                LLVMInt8TypeInContext(cg->ctx), buf, &pos, 1, "buf.off");
            LLVMValueRef remaining = LLVMBuildSub(cg->builder, buf_size, pos, "rem");

            if (part->kind == NODE_LITERAL && part->as.literal.kind == LIT_STRING) {
                /* String literal part — use %s format */
                const char *raw = part->as.literal.value;
                int raw_len = part->as.literal.len;
                /* Strip quotes from first/last parts */
                if (raw_len > 0 && raw[0] == '"') { raw++; raw_len--; }
                if (raw_len > 0 && raw[raw_len - 1] == '"') { raw_len--; }
                if (raw_len == 0) continue; /* skip empty parts */
                /* Process escape sequences */
                char *escaped = (char *)malloc(raw_len + 1);
                int out = 0;
                for (int j = 0; j < raw_len; j++) {
                    if (raw[j] == '\\' && j + 1 < raw_len) {
                        switch (raw[j + 1]) {
                        case 'n': escaped[out++] = '\n'; j++; break;
                        case 't': escaped[out++] = '\t'; j++; break;
                        case '\\': escaped[out++] = '\\'; j++; break;
                        case '"': escaped[out++] = '"'; j++; break;
                        default: escaped[out++] = raw[j]; break;
                        }
                    } else {
                        escaped[out++] = raw[j];
                    }
                }
                escaped[out] = '\0';
                LLVMValueRef str_val = LLVMBuildGlobalStringPtr(cg->builder, escaped, "str.part");
                LLVMValueRef fmt = LLVMBuildGlobalStringPtr(cg->builder, "%s", "fmt.s");
                LLVMValueRef args[] = { buf_offset, remaining, fmt, str_val };
                LLVMValueRef written = LLVMBuildCall2(cg->builder, snprintf_fn_ty,
                                                       snprintf_fn, args, 4, "written");
                /* Update position */
                LLVMValueRef written64 = LLVMBuildSExt(cg->builder, written, i64_ty, "w64");
                LLVMValueRef new_pos = LLVMBuildAdd(cg->builder, pos, written64, "new.pos");
                LLVMBuildStore(cg->builder, new_pos, pos_alloca);
                free(escaped);
            } else {
                /* Expression part — determine format based on type */
                Type *expr_type = part->checked_type ? type_resolve(part->checked_type) : NULL;
                LLVMValueRef expr_val = codegen_expr(cg, part);
                if (!expr_val) continue;

                const char *fmt_str = "%d"; /* default */
                if (expr_type) {
                    switch (expr_type->kind) {
                    case TY_INT: fmt_str = "%ld"; break;
                    case TY_FLOAT: fmt_str = "%g"; break;
                    case TY_BOOL: fmt_str = "%d"; break;
                    case TY_CHAR: fmt_str = "%c"; break;
                    case TY_STRING: fmt_str = "%s"; break;
                    default: fmt_str = "%ld"; break;
                    }
                }
                LLVMValueRef fmt = LLVMBuildGlobalStringPtr(cg->builder, fmt_str, "fmt.expr");
                LLVMValueRef args[] = { buf_offset, remaining, fmt, expr_val };
                LLVMValueRef written = LLVMBuildCall2(cg->builder, snprintf_fn_ty,
                                                       snprintf_fn, args, 4, "written");
                LLVMValueRef written64 = LLVMBuildSExt(cg->builder, written, i64_ty, "w64");
                LLVMValueRef new_pos = LLVMBuildAdd(cg->builder, pos, written64, "new.pos");
                LLVMBuildStore(cg->builder, new_pos, pos_alloca);
            }
        }
        return buf;
    }

    case NODE_TUPLE: {
        int count = node->as.list.count;
        Type *tuple_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        if (!tuple_type || tuple_type->kind != TY_TUPLE) return NULL;

        LLVMTypeRef struct_ty = llvm_type(cg, tuple_type);
        LLVMValueRef tuple_val = LLVMGetUndef(struct_ty);

        for (int i = 0; i < count; i++) {
            LLVMValueRef elem = codegen_expr(cg, node->as.list.items[i]);
            if (elem) {
                tuple_val = LLVMBuildInsertValue(cg->builder, tuple_val, elem, i, "tuple.ins");
            }
        }
        return tuple_val;
    }

    case NODE_LIST: {
        Type *list_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        if (!list_type || list_type->kind != TY_LIST) return NULL;

        Type *elem_type = list_type->as.list.elem;
        LLVMTypeRef elem_llvm_ty = llvm_type(cg, elem_type);
        LLVMTypeRef list_llvm_ty = llvm_type(cg, list_type);
        LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
        int count = node->as.list.count;

        /* Allocate data: malloc(count * sizeof(elem)) */
        LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
        unsigned long long elem_size = LLVMABISizeOfType(td, elem_llvm_ty);
        LLVMValueRef total_size = LLVMConstInt(i64_ty, (unsigned long long)count * elem_size, 0);

        LLVMValueRef malloc_fn = get_or_declare_malloc(cg);
        LLVMTypeRef malloc_fn_ty = LLVMGlobalGetValueType(malloc_fn);
        LLVMValueRef data_ptr = LLVMBuildCall2(cg->builder, malloc_fn_ty, malloc_fn,
                                                &total_size, 1, "list.data");

        /* Store each element */
        for (int i = 0; i < count; i++) {
            LLVMValueRef elem_val = codegen_expr(cg, node->as.list.items[i]);
            if (elem_val) {
                LLVMValueRef idx = LLVMConstInt(LLVMInt32TypeInContext(cg->ctx), (unsigned long long)i * elem_size, 0);
                LLVMValueRef gep = LLVMBuildGEP2(cg->builder,
                    LLVMInt8TypeInContext(cg->ctx), data_ptr, &idx, 1, "elem.ptr");
                LLVMBuildStore(cg->builder, elem_val, gep);
            }
        }

        /* Build list struct: { len, cap, data } */
        LLVMValueRef list_val = LLVMGetUndef(list_llvm_ty);
        list_val = LLVMBuildInsertValue(cg->builder, list_val,
                                         LLVMConstInt(i64_ty, count, 0), 0, "list.len");
        list_val = LLVMBuildInsertValue(cg->builder, list_val,
                                         LLVMConstInt(i64_ty, count, 0), 1, "list.cap");
        list_val = LLVMBuildInsertValue(cg->builder, list_val, data_ptr, 2, "list.data");
        return list_val;
    }

    case NODE_MAP: {
        Type *map_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        if (!map_type || map_type->kind != TY_MAP) return NULL;

        Type *key_type = map_type->as.map.key;
        Type *val_type = map_type->as.map.val;
        LLVMTypeRef key_llvm_ty = llvm_type(cg, key_type);
        LLVMTypeRef val_llvm_ty = llvm_type(cg, val_type);
        LLVMTypeRef map_llvm_ty = llvm_type(cg, map_type);
        LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
        int count = node->as.map.count;

        LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
        unsigned long long key_size = LLVMABISizeOfType(td, key_llvm_ty);
        unsigned long long val_size = LLVMABISizeOfType(td, val_llvm_ty);

        /* Allocate key and value arrays */
        LLVMValueRef malloc_fn = get_or_declare_malloc(cg);
        LLVMTypeRef malloc_fn_ty = LLVMGlobalGetValueType(malloc_fn);

        LLVMValueRef key_total = LLVMConstInt(i64_ty, (unsigned long long)count * key_size, 0);
        LLVMValueRef keys_ptr = LLVMBuildCall2(cg->builder, malloc_fn_ty, malloc_fn,
                                                &key_total, 1, "map.keys");

        LLVMValueRef val_total = LLVMConstInt(i64_ty, (unsigned long long)count * val_size, 0);
        LLVMValueRef vals_ptr = LLVMBuildCall2(cg->builder, malloc_fn_ty, malloc_fn,
                                                &val_total, 1, "map.vals");

        /* Store each key-value pair */
        for (int i = 0; i < count; i++) {
            MapEntry *e = &node->as.map.entries[i];
            if (e->is_spread) continue;
            LLVMValueRef k = codegen_expr(cg, e->key);
            LLVMValueRef v = codegen_expr(cg, e->value);
            if (k) {
                LLVMValueRef kidx = LLVMConstInt(LLVMInt32TypeInContext(cg->ctx), (unsigned long long)i * key_size, 0);
                LLVMValueRef kgep = LLVMBuildGEP2(cg->builder,
                    LLVMInt8TypeInContext(cg->ctx), keys_ptr, &kidx, 1, "key.ptr");
                LLVMBuildStore(cg->builder, k, kgep);
            }
            if (v) {
                LLVMValueRef vidx = LLVMConstInt(LLVMInt32TypeInContext(cg->ctx), (unsigned long long)i * val_size, 0);
                LLVMValueRef vgep = LLVMBuildGEP2(cg->builder,
                    LLVMInt8TypeInContext(cg->ctx), vals_ptr, &vidx, 1, "val.ptr");
                LLVMBuildStore(cg->builder, v, vgep);
            }
        }

        /* Build map struct: { len, cap, keys, vals } */
        LLVMValueRef map_val = LLVMGetUndef(map_llvm_ty);
        map_val = LLVMBuildInsertValue(cg->builder, map_val,
                                        LLVMConstInt(i64_ty, count, 0), 0, "map.len");
        map_val = LLVMBuildInsertValue(cg->builder, map_val,
                                        LLVMConstInt(i64_ty, count, 0), 1, "map.cap");
        map_val = LLVMBuildInsertValue(cg->builder, map_val, keys_ptr, 2, "map.keys");
        map_val = LLVMBuildInsertValue(cg->builder, map_val, vals_ptr, 3, "map.vals");
        return map_val;
    }

    case NODE_INDEX: {
        Type *obj_type = node->as.index.object->checked_type ?
                         type_resolve(node->as.index.object->checked_type) : NULL;

        if (obj_type && obj_type->kind == TY_TUPLE) {
            /* Tuple indexing with integer literal */
            LLVMValueRef obj = codegen_expr(cg, node->as.index.object);
            if (!obj) return NULL;
            int idx = (int)strtol(node->as.index.index->as.literal.value, NULL, 10);
            return LLVMBuildExtractValue(cg->builder, obj, idx, "tuple.idx");
        }

        if (obj_type && obj_type->kind == TY_LIST) {
            /* List indexing: get data pointer, GEP, load */
            Type *elem_type = obj_type->as.list.elem;
            LLVMTypeRef elem_llvm_ty = llvm_type(cg, elem_type);
            LLVMTypeRef list_llvm_ty = llvm_type(cg, obj_type);

            /* Need the list as an alloca to GEP into it */
            LLVMValueRef obj = codegen_expr(cg, node->as.index.object);
            if (!obj) return NULL;
            LLVMValueRef list_alloca = create_entry_alloca(cg, list_llvm_ty, "list.tmp");
            LLVMBuildStore(cg->builder, obj, list_alloca);

            /* Get data pointer: list.data (field 2) */
            LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
            LLVMValueRef data_gep = LLVMBuildStructGEP2(cg->builder, list_llvm_ty,
                                                          list_alloca, 2, "data.gep");
            LLVMValueRef data_ptr = LLVMBuildLoad2(cg->builder, ptr_ty, data_gep, "data.ptr");

            /* Compute byte offset: index * sizeof(elem) */
            LLVMValueRef idx = codegen_expr(cg, node->as.index.index);
            if (!idx) return NULL;
            LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
            unsigned long long elem_size = LLVMABISizeOfType(td, elem_llvm_ty);
            LLVMValueRef byte_offset = LLVMBuildMul(cg->builder, idx,
                LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), elem_size, 0), "byte.off");
            /* Truncate to i32 for GEP index (byte addressing) */
            LLVMValueRef byte_off32 = LLVMBuildTrunc(cg->builder, byte_offset,
                LLVMInt32TypeInContext(cg->ctx), "off32");
            LLVMValueRef elem_ptr = LLVMBuildGEP2(cg->builder,
                LLVMInt8TypeInContext(cg->ctx), data_ptr, &byte_off32, 1, "elem.ptr");
            return LLVMBuildLoad2(cg->builder, elem_llvm_ty, elem_ptr, "elem.val");
        }

        if (obj_type && obj_type->kind == TY_MAP) {
            /* Map indexing: linear scan for key match */
            Type *key_type = obj_type->as.map.key;
            Type *val_type = obj_type->as.map.val;
            LLVMTypeRef key_llvm_ty = llvm_type(cg, key_type);
            LLVMTypeRef val_llvm_ty = llvm_type(cg, val_type);
            LLVMTypeRef map_llvm_ty = llvm_type(cg, obj_type);
            LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
            LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);

            LLVMValueRef obj = codegen_expr(cg, node->as.index.object);
            if (!obj) return NULL;
            LLVMValueRef map_alloca = create_entry_alloca(cg, map_llvm_ty, "map.tmp");
            LLVMBuildStore(cg->builder, obj, map_alloca);

            /* Get len, keys_ptr, vals_ptr */
            LLVMValueRef len_gep = LLVMBuildStructGEP2(cg->builder, map_llvm_ty, map_alloca, 0, "len.gep");
            LLVMValueRef len = LLVMBuildLoad2(cg->builder, i64_ty, len_gep, "map.len");
            LLVMValueRef keys_gep = LLVMBuildStructGEP2(cg->builder, map_llvm_ty, map_alloca, 2, "keys.gep");
            LLVMValueRef keys_ptr = LLVMBuildLoad2(cg->builder, ptr_ty, keys_gep, "keys.ptr");
            LLVMValueRef vals_gep = LLVMBuildStructGEP2(cg->builder, map_llvm_ty, map_alloca, 3, "vals.gep");
            LLVMValueRef vals_ptr = LLVMBuildLoad2(cg->builder, ptr_ty, vals_gep, "vals.ptr");

            LLVMValueRef search_key = codegen_expr(cg, node->as.index.index);
            if (!search_key) return NULL;

            LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
            unsigned long long key_size = LLVMABISizeOfType(td, key_llvm_ty);
            unsigned long long val_size = LLVMABISizeOfType(td, val_llvm_ty);

            /* Linear scan loop */
            LLVMValueRef iter_alloca = create_entry_alloca(cg, i64_ty, "map.iter");
            LLVMValueRef result_alloca = create_entry_alloca(cg, val_llvm_ty, "map.result");
            LLVMBuildStore(cg->builder, LLVMConstInt(i64_ty, 0, 0), iter_alloca);
            LLVMBuildStore(cg->builder, LLVMConstNull(val_llvm_ty), result_alloca);

            LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "map.cond");
            LLVMBasicBlockRef body_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "map.body");
            LLVMBasicBlockRef found_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "map.found");
            LLVMBasicBlockRef inc_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "map.inc");
            LLVMBasicBlockRef end_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "map.end");

            LLVMBuildBr(cg->builder, cond_bb);
            LLVMPositionBuilderAtEnd(cg->builder, cond_bb);
            LLVMValueRef i = LLVMBuildLoad2(cg->builder, i64_ty, iter_alloca, "i");
            LLVMValueRef cmp = LLVMBuildICmp(cg->builder, LLVMIntSLT, i, len, "map.cmp");
            LLVMBuildCondBr(cg->builder, cmp, body_bb, end_bb);

            LLVMPositionBuilderAtEnd(cg->builder, body_bb);
            LLVMValueRef key_byte_off = LLVMBuildMul(cg->builder, i,
                LLVMConstInt(i64_ty, key_size, 0), "koff");
            LLVMValueRef koff32 = LLVMBuildTrunc(cg->builder, key_byte_off,
                LLVMInt32TypeInContext(cg->ctx), "koff32");
            LLVMValueRef kptr = LLVMBuildGEP2(cg->builder,
                LLVMInt8TypeInContext(cg->ctx), keys_ptr, &koff32, 1, "k.ptr");
            LLVMValueRef cur_key = LLVMBuildLoad2(cg->builder, key_llvm_ty, kptr, "cur.key");

            LLVMValueRef match;
            if (key_type->kind == TY_INT || key_type->kind == TY_CHAR || key_type->kind == TY_BOOL) {
                match = LLVMBuildICmp(cg->builder, LLVMIntEQ, cur_key, search_key, "key.match");
            } else if (key_type->kind == TY_FLOAT) {
                match = LLVMBuildFCmp(cg->builder, LLVMRealOEQ, cur_key, search_key, "key.match");
            } else {
                match = LLVMBuildICmp(cg->builder, LLVMIntEQ, cur_key, search_key, "key.match");
            }
            LLVMBuildCondBr(cg->builder, match, found_bb, inc_bb);

            LLVMPositionBuilderAtEnd(cg->builder, found_bb);
            LLVMValueRef val_byte_off = LLVMBuildMul(cg->builder, i,
                LLVMConstInt(i64_ty, val_size, 0), "voff");
            LLVMValueRef voff32 = LLVMBuildTrunc(cg->builder, val_byte_off,
                LLVMInt32TypeInContext(cg->ctx), "voff32");
            LLVMValueRef vptr = LLVMBuildGEP2(cg->builder,
                LLVMInt8TypeInContext(cg->ctx), vals_ptr, &voff32, 1, "v.ptr");
            LLVMValueRef found_val = LLVMBuildLoad2(cg->builder, val_llvm_ty, vptr, "found.val");
            LLVMBuildStore(cg->builder, found_val, result_alloca);
            LLVMBuildBr(cg->builder, end_bb);

            LLVMPositionBuilderAtEnd(cg->builder, inc_bb);
            LLVMValueRef next_i = LLVMBuildAdd(cg->builder, i, LLVMConstInt(i64_ty, 1, 0), "next.i");
            LLVMBuildStore(cg->builder, next_i, iter_alloca);
            LLVMBuildBr(cg->builder, cond_bb);

            LLVMPositionBuilderAtEnd(cg->builder, end_bb);
            return LLVMBuildLoad2(cg->builder, val_llvm_ty, result_alloca, "map.val");
        }

        return NULL;
    }

    case NODE_PIPE: {
        /* Pipe was desugared by checker: right side is now a call with left prepended */
        AstNode *right = node->as.pipe.right;
        if (right->kind == NODE_CALL) {
            return codegen_expr(cg, right);
        } else {
            /* Simple: f(a) */
            LLVMValueRef left_val = codegen_expr(cg, node->as.pipe.left);
            LLVMValueRef fn_val = codegen_expr(cg, right);
            if (!fn_val || !left_val) return NULL;

            Type *fn_type = right->checked_type ? type_resolve(right->checked_type) : NULL;
            LLVMTypeRef fn_ty = (fn_type && fn_type->kind == TY_FN)
                                ? llvm_fn_sig_type(cg, fn_type) : NULL;
            if (!fn_ty) return NULL;

            Type *ret_type = fn_type->as.fn.ret;
            bool is_void = !ret_type || ret_type->kind == TY_NONE;
            LLVMValueRef args[] = { left_val };
            return LLVMBuildCall2(cg->builder, fn_ty, fn_val, args, 1,
                                  is_void ? "" : "pipe");
        }
    }

    case NODE_MATCH: {
        LLVMValueRef subject = codegen_expr(cg, node->as.match.subject);
        if (!subject) return NULL;
        Type *subject_type = node->as.match.subject->checked_type ?
                             type_resolve(node->as.match.subject->checked_type) : NULL;

        int arm_count = node->as.match.arm_count;
        Type *result_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        bool has_value = result_type && result_type->kind != TY_NONE;

        LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "match.end");
        /* Unreachable block for when no match (last arm fallthrough) */
        LLVMBasicBlockRef unreach_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "match.unreach");

        /* Collect (arm_bb, arm_val, arm_end_bb) for PHI */
        LLVMValueRef *arm_vals = NULL;
        LLVMBasicBlockRef *arm_end_bbs = NULL;
        if (has_value) {
            arm_vals = (LLVMValueRef *)malloc(arm_count * sizeof(LLVMValueRef));
            arm_end_bbs = (LLVMBasicBlockRef *)malloc(arm_count * sizeof(LLVMBasicBlockRef));
        }
        int phi_count = 0;

        for (int i = 0; i < arm_count; i++) {
            MatchArm *arm = &node->as.match.arms[i];
            LLVMBasicBlockRef arm_body = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "match.arm");
            LLVMBasicBlockRef next_arm = (i + 1 < arm_count) ?
                LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "match.next") : unreach_bb;

            /* Test pattern */
            codegen_pattern_test(cg, arm->pattern, subject, subject_type, arm_body, next_arm);

            /* Arm body */
            LLVMPositionBuilderAtEnd(cg->builder, arm_body);
            push_value_scope(cg);
            codegen_pattern_bind(cg, arm->pattern, subject, subject_type);

            LLVMValueRef arm_val = codegen_expr(cg, arm->body);
            LLVMBasicBlockRef arm_end = LLVMGetInsertBlock(cg->builder);
            pop_value_scope(cg);

            if (!block_has_terminator(arm_end)) {
                if (has_value && arm_val) {
                    arm_vals[phi_count] = arm_val;
                    arm_end_bbs[phi_count] = arm_end;
                    phi_count++;
                }
                LLVMBuildBr(cg->builder, merge_bb);
            }

            if (i + 1 < arm_count) {
                LLVMPositionBuilderAtEnd(cg->builder, next_arm);
            }
        }

        /* Unreachable block */
        LLVMPositionBuilderAtEnd(cg->builder, unreach_bb);
        LLVMBuildUnreachable(cg->builder);

        LLVMPositionBuilderAtEnd(cg->builder, merge_bb);

        LLVMValueRef phi = NULL;
        if (has_value && phi_count > 0) {
            LLVMTypeRef phi_ty = llvm_type(cg, result_type);
            phi = LLVMBuildPhi(cg->builder, phi_ty, "match.val");
            LLVMAddIncoming(phi, arm_vals, arm_end_bbs, phi_count);
        }
        free(arm_vals);
        free(arm_end_bbs);
        return phi;
    }

    case NODE_LAMBDA: {
        Type *fn_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        if (!fn_type || fn_type->kind != TY_FN) return NULL;

        /* Generate a unique function name */
        char lambda_name[64];
        snprintf(lambda_name, sizeof(lambda_name), "__lux_lambda_%d", cg->lambda_counter++);

        int param_count = node->as.lambda.params.count;
        LLVMTypeRef *param_llvm_types = NULL;
        if (param_count > 0) {
            param_llvm_types = (LLVMTypeRef *)malloc(param_count * sizeof(LLVMTypeRef));
            for (int i = 0; i < param_count; i++) {
                param_llvm_types[i] = llvm_type(cg, fn_type->as.fn.param_types[i]);
            }
        }

        LLVMTypeRef ret_ty = llvm_type(cg, fn_type->as.fn.ret);
        LLVMTypeRef lambda_fn_ty = LLVMFunctionType(ret_ty, param_llvm_types, param_count, 0);
        LLVMValueRef lambda_fn = LLVMAddFunction(cg->module, lambda_name, lambda_fn_ty);

        /* Save current function context */
        LLVMValueRef saved_fn = cg->current_fn;
        Type *saved_ret = cg->current_ret_type;
        DeferEntry *saved_defers = cg->defer_stack;
        LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(cg->builder);

        cg->current_fn = lambda_fn;
        cg->current_ret_type = fn_type->as.fn.ret;
        cg->defer_stack = NULL;

        LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cg->ctx, lambda_fn, "entry");
        LLVMPositionBuilderAtEnd(cg->builder, entry);
        push_value_scope(cg);

        /* Bind parameters */
        for (int i = 0; i < param_count; i++) {
            Param *p = &node->as.lambda.params.items[i];
            if (!p->pattern || p->pattern->kind != PAT_BIND) continue;
            const char *pname = cg_intern(cg, p->pattern->as.bind.name, p->pattern->as.bind.len);
            LLVMValueRef alloca_val = LLVMBuildAlloca(cg->builder, param_llvm_types[i], pname);
            LLVMBuildStore(cg->builder, LLVMGetParam(lambda_fn, i), alloca_val);
            set_value(cg, pname, alloca_val);
        }

        /* Generate body */
        if (node->as.lambda.body_expr) {
            LLVMValueRef body_val = codegen_expr(cg, node->as.lambda.body_expr);
            if (body_val && fn_type->as.fn.ret->kind != TY_NONE) {
                LLVMBuildRet(cg->builder, body_val);
            } else {
                LLVMBuildRetVoid(cg->builder);
            }
        } else {
            codegen_block(cg, node->as.lambda.body_block);
            if (!block_has_terminator(LLVMGetInsertBlock(cg->builder))) {
                if (fn_type->as.fn.ret->kind == TY_NONE) {
                    LLVMBuildRetVoid(cg->builder);
                } else {
                    LLVMBuildRet(cg->builder, LLVMConstNull(ret_ty));
                }
            }
        }

        pop_value_scope(cg);
        free(param_llvm_types);

        /* Verify */
        LLVMVerifyFunction(lambda_fn, LLVMPrintMessageAction);

        /* Restore context */
        cg->current_fn = saved_fn;
        cg->current_ret_type = saved_ret;
        cg->defer_stack = saved_defers;
        LLVMPositionBuilderAtEnd(cg->builder, saved_bb);

        return lambda_fn;
    }

    case NODE_FOR: {
        /* For loop over a range: for i in start..end / start..=end */
        AstNode *iter = node->as.for_expr.iter;
        if (iter && iter->kind == NODE_RANGE) {
            LLVMValueRef start_val = codegen_expr(cg, iter->as.range.start);
            LLVMValueRef end_val = codegen_expr(cg, iter->as.range.end);
            bool inclusive = iter->as.range.inclusive;

            LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);

            LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "for.cond");
            LLVMBasicBlockRef body_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "for.body");
            LLVMBasicBlockRef inc_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "for.inc");
            LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "for.exit");

            /* Alloca for loop variable */
            LLVMValueRef iter_alloca = create_entry_alloca(cg, i64_ty, "for.iter");
            LLVMBuildStore(cg->builder, start_val, iter_alloca);
            LLVMBuildBr(cg->builder, cond_bb);

            /* Condition */
            LLVMPositionBuilderAtEnd(cg->builder, cond_bb);
            LLVMValueRef cur = LLVMBuildLoad2(cg->builder, i64_ty, iter_alloca, "cur");
            LLVMValueRef cond = inclusive ?
                LLVMBuildICmp(cg->builder, LLVMIntSLE, cur, end_val, "for.cmp") :
                LLVMBuildICmp(cg->builder, LLVMIntSLT, cur, end_val, "for.cmp");
            LLVMBuildCondBr(cg->builder, cond, body_bb, exit_bb);

            /* Body */
            LLVMPositionBuilderAtEnd(cg->builder, body_bb);
            push_loop(cg, inc_bb, exit_bb);
            push_value_scope(cg);

            /* Bind loop variable */
            if (node->as.for_expr.pattern) {
                LLVMValueRef iter_val = LLVMBuildLoad2(cg->builder, i64_ty, iter_alloca, "i");
                codegen_pattern_bind(cg, node->as.for_expr.pattern, iter_val, type_int());
            }

            codegen_block(cg, node->as.for_expr.body);
            pop_value_scope(cg);
            pop_loop(cg);

            if (!block_has_terminator(LLVMGetInsertBlock(cg->builder)))
                LLVMBuildBr(cg->builder, inc_bb);

            /* Increment */
            LLVMPositionBuilderAtEnd(cg->builder, inc_bb);
            LLVMValueRef next = LLVMBuildAdd(cg->builder,
                LLVMBuildLoad2(cg->builder, i64_ty, iter_alloca, "cur"),
                LLVMConstInt(i64_ty, 1, 0), "inc");
            LLVMBuildStore(cg->builder, next, iter_alloca);
            LLVMBuildBr(cg->builder, cond_bb);

            LLVMPositionBuilderAtEnd(cg->builder, exit_bb);
        }
        return NULL;
    }

    case NODE_BLOCK:
    case NODE_DO_BLOCK: {
        push_value_scope(cg);
        codegen_block(cg, node->as.block);
        pop_value_scope(cg);
        return NULL;
    }

    case NODE_SCOPE: {
        if (node->as.unary_stmt.value) {
            return codegen_expr(cg, node->as.unary_stmt.value);
        }
        return NULL;
    }

    case NODE_SPAWN: {
        /*
         * spawn fn(args...) — create a coroutine
         *
         * Strategy:
         * 1. Build a "thunk struct" type: { ret_type, arg0_type, arg1_type, ... }
         * 2. Generate a thunk function: void __lux_thunk_N(void *data)
         *    that unpacks args from data, calls the function, stores result at data[0]
         * 3. malloc the thunk struct, store args
         * 4. Call lux_coro_spawn(thunk_fn, data_ptr) -> task handle
         * 5. Return { task_handle, data_ptr } as the Task value
         */
        AstNode *inner = node->as.unary_stmt.value;
        if (!inner || inner->kind != NODE_CALL) {
            fprintf(stderr, "codegen: spawn requires a function call\n");
            return NULL;
        }

        AstNode *callee_node = inner->as.call.callee;
        int argc = inner->as.call.args.count;

        /* Resolve the function's return type */
        Type *task_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        Type *ret_type = (task_type && task_type->kind == TY_TASK) ?
                         task_type->as.task.elem : type_int();
        LLVMTypeRef ret_llvm_ty = llvm_type(cg, ret_type);
        bool ret_is_void = (ret_type->kind == TY_NONE || ret_type->kind == TY_NEVER);

        LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);

        /* Build thunk struct type: { ret_type, arg0_type, arg1_type, ... } */
        int field_count = (ret_is_void ? 0 : 1) + argc;
        LLVMTypeRef *fields = (LLVMTypeRef *)malloc(field_count * sizeof(LLVMTypeRef));
        int fi = 0;
        if (!ret_is_void) {
            fields[fi++] = ret_llvm_ty;
        }
        /* Evaluate and collect argument values + types */
        LLVMValueRef *arg_vals = (LLVMValueRef *)malloc(argc * sizeof(LLVMValueRef));
        for (int i = 0; i < argc; i++) {
            arg_vals[i] = codegen_expr(cg, inner->as.call.args.items[i].value);
            Type *arg_type = inner->as.call.args.items[i].value->checked_type;
            fields[fi++] = arg_type ? llvm_type(cg, arg_type) : LLVMInt64TypeInContext(cg->ctx);
        }
        LLVMTypeRef thunk_struct_ty = LLVMStructTypeInContext(cg->ctx, fields, field_count, 0);

        /* Malloc the thunk struct */
        LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
        unsigned long long struct_size = LLVMABISizeOfType(td, thunk_struct_ty);
        LLVMValueRef malloc_fn = get_or_declare_malloc(cg);
        LLVMTypeRef malloc_fn_ty = LLVMGlobalGetValueType(malloc_fn);
        LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
        LLVMValueRef size_val = LLVMConstInt(i64_ty, struct_size, 0);
        LLVMValueRef data_ptr = LLVMBuildCall2(cg->builder, malloc_fn_ty, malloc_fn,
                                                &size_val, 1, "spawn.data");

        /* Store args into the thunk struct (skip field 0 if it's the result slot) */
        for (int i = 0; i < argc; i++) {
            int idx = ret_is_void ? i : i + 1;
            LLVMValueRef gep = LLVMBuildStructGEP2(cg->builder, thunk_struct_ty,
                                                     data_ptr, idx, "spawn.arg.ptr");
            LLVMBuildStore(cg->builder, arg_vals[i], gep);
        }

        /* Generate the thunk function: void __lux_thunk_N(ptr %data) */
        char thunk_name[64];
        snprintf(thunk_name, sizeof(thunk_name), "__lux_thunk_%d", cg->spawn_counter++);
        LLVMTypeRef void_ty = LLVMVoidTypeInContext(cg->ctx);
        LLVMTypeRef thunk_fn_ty = LLVMFunctionType(void_ty, &ptr_ty, 1, 0);
        LLVMValueRef thunk_fn = LLVMAddFunction(cg->module, thunk_name, thunk_fn_ty);
        LLVMSetLinkage(thunk_fn, LLVMPrivateLinkage);

        /* Save current builder state */
        LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(cg->builder);
        LLVMValueRef saved_fn = cg->current_fn;
        Type *saved_ret = cg->current_ret_type;
        DeferEntry *saved_defer = cg->defer_stack;

        /* Build thunk body */
        cg->current_fn = thunk_fn;
        cg->current_ret_type = type_none();
        cg->defer_stack = NULL;

        LLVMBasicBlockRef thunk_entry = LLVMAppendBasicBlockInContext(cg->ctx, thunk_fn, "entry");
        LLVMPositionBuilderAtEnd(cg->builder, thunk_entry);

        LLVMValueRef thunk_data = LLVMGetParam(thunk_fn, 0);

        /* Load args from thunk struct */
        LLVMValueRef *call_args = (LLVMValueRef *)malloc(argc * sizeof(LLVMValueRef));
        for (int i = 0; i < argc; i++) {
            int idx = ret_is_void ? i : i + 1;
            LLVMValueRef gep = LLVMBuildStructGEP2(cg->builder, thunk_struct_ty,
                                                     thunk_data, idx, "thunk.arg.ptr");
            LLVMTypeRef arg_ty = LLVMStructGetTypeAtIndex(thunk_struct_ty, idx);
            call_args[i] = LLVMBuildLoad2(cg->builder, arg_ty, gep, "thunk.arg");
        }

        /* Resolve the callee function in the thunk */
        const char *callee_name = NULL;
        if (callee_node->kind == NODE_IDENT) {
            callee_name = cg_intern(cg, callee_node->as.ident.name, callee_node->as.ident.len);
        } else if (callee_node->kind == NODE_FIELD_ACCESS) {
            /* Method call on a type: Type.method — get mangled name */
            callee_name = cg_intern(cg, callee_node->as.field_access.field.name,
                                     callee_node->as.field_access.field.len);
        }

        LLVMValueRef callee_fn = callee_name ? LLVMGetNamedFunction(cg->module, callee_name) : NULL;
        if (!callee_fn && callee_name) {
            /* Try to find it — may be extern or declared elsewhere */
            fprintf(stderr, "codegen: spawn: cannot find function '%s'\n", callee_name);
            LLVMBuildRetVoid(cg->builder);
        } else if (callee_fn) {
            /* Get function type for call */
            LLVMTypeRef callee_llvm_ty = LLVMGlobalGetValueType(callee_fn);

            if (ret_is_void) {
                LLVMBuildCall2(cg->builder, callee_llvm_ty, callee_fn,
                               call_args, argc, "");
            } else {
                LLVMValueRef result = LLVMBuildCall2(cg->builder, callee_llvm_ty, callee_fn,
                                                     call_args, argc, "thunk.result");
                /* Store result at field 0 */
                LLVMValueRef res_gep = LLVMBuildStructGEP2(cg->builder, thunk_struct_ty,
                                                            thunk_data, 0, "thunk.res.ptr");
                LLVMBuildStore(cg->builder, result, res_gep);
            }
            LLVMBuildRetVoid(cg->builder);
        }

        free(call_args);

        /* Restore builder state */
        cg->current_fn = saved_fn;
        cg->current_ret_type = saved_ret;
        cg->defer_stack = saved_defer;
        LLVMPositionBuilderAtEnd(cg->builder, saved_bb);

        /* Call lux_coro_spawn(thunk_fn, data_ptr) */
        LLVMValueRef spawn_fn = get_or_declare_coro_spawn(cg);
        LLVMTypeRef spawn_fn_ty = LLVMGlobalGetValueType(spawn_fn);
        LLVMValueRef spawn_args[] = { thunk_fn, data_ptr };
        LLVMValueRef task_handle = LLVMBuildCall2(cg->builder, spawn_fn_ty, spawn_fn,
                                                   spawn_args, 2, "spawn.task");

        /* Build Task struct: { ptr task_handle, ptr data_ptr } */
        LLVMTypeRef task_struct_ty = llvm_type(cg, task_type);
        LLVMValueRef task_val = LLVMGetUndef(task_struct_ty);
        task_val = LLVMBuildInsertValue(cg->builder, task_val, task_handle, 0, "task.handle");
        task_val = LLVMBuildInsertValue(cg->builder, task_val, data_ptr, 1, "task.data");

        free(fields);
        free(arg_vals);
        return task_val;
    }

    case NODE_AWAIT: {
        /*
         * await task_expr — run coroutine to completion, return result
         *
         * 1. Extract task_handle and data_ptr from Task struct
         * 2. Call lux_coro_await(task_handle)
         * 3. Load result from data_ptr at offset 0
         * 4. Destroy task and free data
         */
        AstNode *inner = node->as.unary_stmt.value;
        LLVMValueRef task_val = codegen_expr(cg, inner);
        if (!task_val) return NULL;

        /* Get the result type from the inner expression's Task type */
        Type *inner_type = inner->checked_type ? type_resolve(inner->checked_type) : NULL;
        Type *ret_type = (inner_type && inner_type->kind == TY_TASK) ?
                         inner_type->as.task.elem : type_int();
        bool ret_is_void = (ret_type->kind == TY_NONE || ret_type->kind == TY_NEVER);

        /* Extract task handle and data pointer */
        LLVMValueRef task_handle = LLVMBuildExtractValue(cg->builder, task_val, 0, "await.handle");
        LLVMValueRef data_ptr = LLVMBuildExtractValue(cg->builder, task_val, 1, "await.data");

        /* Call lux_coro_await(task_handle) */
        LLVMValueRef await_fn = get_or_declare_coro_await(cg);
        LLVMTypeRef await_fn_ty = LLVMGlobalGetValueType(await_fn);
        LLVMBuildCall2(cg->builder, await_fn_ty, await_fn, &task_handle, 1, "");

        /* Load result from data_ptr (result is at offset 0 of thunk struct) */
        LLVMValueRef result = NULL;
        if (!ret_is_void) {
            LLVMTypeRef ret_llvm_ty = llvm_type(cg, ret_type);
            result = LLVMBuildLoad2(cg->builder, ret_llvm_ty, data_ptr, "await.result");
        }

        /* Cleanup: destroy task and free data */
        LLVMValueRef destroy_fn = get_or_declare_coro_destroy(cg);
        LLVMTypeRef destroy_fn_ty = LLVMGlobalGetValueType(destroy_fn);
        LLVMBuildCall2(cg->builder, destroy_fn_ty, destroy_fn, &task_handle, 1, "");

        LLVMValueRef free_fn = get_or_declare_free(cg);
        LLVMTypeRef free_fn_ty = LLVMGlobalGetValueType(free_fn);
        LLVMBuildCall2(cg->builder, free_fn_ty, free_fn, &data_ptr, 1, "");

        return result;
    }

    case NODE_LIST_COMP: {
        /* [expr for pattern in iter] — only supports range iterators for now */
        AstNode *iter_node = node->as.list_comp.iter;
        Type *result_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
        Type *elem_type = (result_type && result_type->kind == TY_LIST) ?
                          result_type->as.list.elem : type_int();

        /* For range iterators: compute start, end, allocate list */
        if (iter_node->kind == NODE_RANGE) {
            LLVMValueRef start = codegen_expr(cg, iter_node->as.range.start);
            LLVMValueRef end = codegen_expr(cg, iter_node->as.range.end);
            if (!start || !end) return NULL;

            LLVMTypeRef i64_ty = LLVMInt64TypeInContext(cg->ctx);
            LLVMTypeRef elem_llvm_ty = llvm_type(cg, elem_type);

            /* Compute size = end - start (+ 1 if inclusive) */
            LLVMValueRef size = LLVMBuildSub(cg->builder, end, start, "comp.size");
            if (iter_node->as.range.inclusive) {
                size = LLVMBuildAdd(cg->builder, size,
                                    LLVMConstInt(i64_ty, 1, 0), "comp.size.inc");
            }

            /* Allocate data: malloc(size * elem_size) */
            LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
            unsigned long long elem_sz = LLVMABISizeOfType(td, elem_llvm_ty);
            LLVMValueRef byte_size = LLVMBuildMul(cg->builder, size,
                LLVMConstInt(i64_ty, elem_sz, 0), "comp.bytes");
            LLVMValueRef malloc_fn = get_or_declare_malloc(cg);
            LLVMTypeRef malloc_fn_ty = LLVMGlobalGetValueType(malloc_fn);
            LLVMValueRef data_ptr = LLVMBuildCall2(cg->builder, malloc_fn_ty, malloc_fn,
                                                    &byte_size, 1, "comp.data");

            /* Create loop: for i in start..end */
            LLVMValueRef counter = create_entry_alloca(cg, i64_ty, "comp.i");
            LLVMBuildStore(cg->builder, start, counter);
            LLVMValueRef out_idx = create_entry_alloca(cg, i64_ty, "comp.out");
            LLVMBuildStore(cg->builder, LLVMConstInt(i64_ty, 0, 0), out_idx);

            LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "comp.cond");
            LLVMBasicBlockRef body_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "comp.body");
            LLVMBasicBlockRef done_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "comp.done");

            LLVMBuildBr(cg->builder, cond_bb);
            LLVMPositionBuilderAtEnd(cg->builder, cond_bb);
            LLVMValueRef cur = LLVMBuildLoad2(cg->builder, i64_ty, counter, "comp.cur");
            LLVMValueRef cmp = iter_node->as.range.inclusive
                ? LLVMBuildICmp(cg->builder, LLVMIntSLE, cur, end, "comp.cmp")
                : LLVMBuildICmp(cg->builder, LLVMIntSLT, cur, end, "comp.cmp");
            LLVMBuildCondBr(cg->builder, cmp, body_bb, done_bb);

            LLVMPositionBuilderAtEnd(cg->builder, body_bb);
            push_value_scope(cg);

            /* Bind the pattern variable to the current counter value */
            if (node->as.list_comp.pattern && node->as.list_comp.pattern->kind == PAT_BIND) {
                const char *pname = cg_intern(cg,
                    node->as.list_comp.pattern->as.bind.name,
                    node->as.list_comp.pattern->as.bind.len);
                LLVMValueRef pat_alloca = create_entry_alloca(cg, i64_ty, pname);
                LLVMBuildStore(cg->builder, cur, pat_alloca);
                set_value(cg, pname, pat_alloca);
            }

            /* Check filter */
            LLVMBasicBlockRef store_bb = NULL;
            LLVMBasicBlockRef inc_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "comp.inc");
            if (node->as.list_comp.filter) {
                store_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "comp.store");
                LLVMValueRef filt = codegen_expr(cg, node->as.list_comp.filter);
                LLVMBuildCondBr(cg->builder, filt, store_bb, inc_bb);
                LLVMPositionBuilderAtEnd(cg->builder, store_bb);
            }

            /* Compute and store element */
            LLVMValueRef val = codegen_expr(cg, node->as.list_comp.expr);
            LLVMValueRef cur_out = LLVMBuildLoad2(cg->builder, i64_ty, out_idx, "comp.oidx");
            LLVMValueRef byte_off = LLVMBuildMul(cg->builder, cur_out,
                LLVMConstInt(i64_ty, elem_sz, 0), "comp.boff");
            LLVMValueRef elem_ptr = LLVMBuildGEP2(cg->builder,
                LLVMInt8TypeInContext(cg->ctx), data_ptr, &byte_off, 1, "comp.eptr");
            LLVMBuildStore(cg->builder, val, elem_ptr);
            /* Increment output index */
            LLVMValueRef next_out = LLVMBuildAdd(cg->builder, cur_out,
                LLVMConstInt(i64_ty, 1, 0), "comp.nout");
            LLVMBuildStore(cg->builder, next_out, out_idx);

            pop_value_scope(cg);

            LLVMBuildBr(cg->builder, inc_bb);
            LLVMPositionBuilderAtEnd(cg->builder, inc_bb);
            /* Increment counter */
            LLVMValueRef next = LLVMBuildAdd(cg->builder, cur,
                LLVMConstInt(i64_ty, 1, 0), "comp.next");
            LLVMBuildStore(cg->builder, next, counter);
            LLVMBuildBr(cg->builder, cond_bb);

            LLVMPositionBuilderAtEnd(cg->builder, done_bb);
            /* Build list struct: { len, cap, data } */
            LLVMValueRef final_len = LLVMBuildLoad2(cg->builder, i64_ty, out_idx, "comp.flen");
            LLVMTypeRef list_ty = llvm_type(cg, result_type);
            LLVMValueRef list_val = LLVMGetUndef(list_ty);
            list_val = LLVMBuildInsertValue(cg->builder, list_val, final_len, 0, "list.len");
            list_val = LLVMBuildInsertValue(cg->builder, list_val, size, 1, "list.cap");
            list_val = LLVMBuildInsertValue(cg->builder, list_val, data_ptr, 2, "list.data");
            return list_val;
        }
        return NULL;
    }

    default:
        return NULL;
    }
}

/* ===== Statement codegen ===== */

/* Create an alloca in the entry block of the current function */
static LLVMValueRef create_entry_alloca(Codegen *cg, LLVMTypeRef ty, const char *name) {
    LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(cg->builder);
    LLVMBasicBlockRef entry_bb = LLVMGetEntryBasicBlock(cg->current_fn);
    LLVMValueRef first_instr = LLVMGetFirstInstruction(entry_bb);
    if (first_instr) {
        LLVMPositionBuilderBefore(cg->builder, first_instr);
    } else {
        LLVMPositionBuilderAtEnd(cg->builder, entry_bb);
    }
    LLVMValueRef alloca_val = LLVMBuildAlloca(cg->builder, ty, name);
    LLVMPositionBuilderAtEnd(cg->builder, current_bb);
    return alloca_val;
}

/* Bind a pattern to a value — used in let/var, match arms, for loops */
static void codegen_pattern_bind(Codegen *cg, AstPattern *pat, LLVMValueRef val, Type *type) {
    if (!pat || !val) return;

    switch (pat->kind) {
    case PAT_BIND: {
        const char *name = cg_intern(cg, pat->as.bind.name, pat->as.bind.len);
        LLVMTypeRef llvm_ty = llvm_type(cg, type);
        LLVMValueRef alloca_val = create_entry_alloca(cg, llvm_ty, name);
        LLVMBuildStore(cg->builder, val, alloca_val);
        set_value(cg, name, alloca_val);
        break;
    }
    case PAT_WILDCARD:
        break;
    case PAT_TUPLE: {
        Type *resolved = type_resolve(type);
        if (resolved && resolved->kind == TY_TUPLE) {
            int count = pat->as.tuple.count;
            for (int i = 0; i < count && i < resolved->as.tuple.count; i++) {
                LLVMValueRef elem = LLVMBuildExtractValue(cg->builder, val, i, "tup.el");
                codegen_pattern_bind(cg, pat->as.tuple.items[i],
                                     elem, resolved->as.tuple.elems[i]);
            }
        }
        break;
    }
    case PAT_VARIANT: {
        Type *resolved = type_resolve(type);
        if (!resolved || resolved->kind != TY_ENUM) break;

        const char *vname = cg_intern(cg,
            pat->as.variant.path.segments[0].name,
            pat->as.variant.path.segments[0].len);
        int variant_idx = -1;
        for (int i = 0; i < resolved->as.enumt.variant_count; i++) {
            if (resolved->as.enumt.variants[i].name == vname) {
                variant_idx = i;
                break;
            }
        }
        if (variant_idx < 0 || pat->as.variant.args.count == 0) break;

        VariantInfo *vi = &resolved->as.enumt.variants[variant_idx];

        /* Store enum value to memory to get at payload */
        LLVMTypeRef enum_ty = llvm_type(cg, resolved);
        LLVMValueRef tmp = create_entry_alloca(cg, enum_ty, "bind.enum");
        LLVMBuildStore(cg->builder, val, tmp);

        /* Get payload pointer */
        LLVMValueRef payload_ptr = LLVMBuildStructGEP2(cg->builder, enum_ty, tmp, 1, "payload.ptr");

        /* Extract each payload field */
        LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
        unsigned offset = 0;
        int n = pat->as.variant.args.count;
        if (n > vi->payload_count) n = vi->payload_count;
        for (int i = 0; i < n; i++) {
            LLVMTypeRef field_ty = llvm_type(cg, vi->payload_types[i]);
            LLVMValueRef idx = LLVMConstInt(LLVMInt32TypeInContext(cg->ctx), offset, 0);
            LLVMValueRef gep = LLVMBuildGEP2(cg->builder,
                LLVMInt8TypeInContext(cg->ctx), payload_ptr, &idx, 1, "field.byte");
            LLVMValueRef field_ptr = LLVMBuildBitCast(cg->builder, gep,
                LLVMPointerTypeInContext(cg->ctx, 0), "field.ptr");
            LLVMValueRef field_val = LLVMBuildLoad2(cg->builder, field_ty, field_ptr, "field.val");

            codegen_pattern_bind(cg, pat->as.variant.args.items[i], field_val, vi->payload_types[i]);
            offset += (unsigned)LLVMABISizeOfType(td, field_ty);
        }
        break;
    }
    case PAT_BIND_AS: {
        /* Bind the whole value to the name */
        const char *name = cg_intern(cg, pat->as.bind_as.name.name, pat->as.bind_as.name.len);
        LLVMTypeRef llvm_ty = llvm_type(cg, type);
        LLVMValueRef alloca_val = create_entry_alloca(cg, llvm_ty, name);
        LLVMBuildStore(cg->builder, val, alloca_val);
        set_value(cg, name, alloca_val);
        /* Also bind the inner pattern */
        codegen_pattern_bind(cg, pat->as.bind_as.pattern, val, type);
        break;
    }
    default:
        break;
    }
}

/* Emit pattern test: branch to match_bb if pattern matches, fail_bb otherwise */
static void codegen_pattern_test(Codegen *cg, AstPattern *pat, LLVMValueRef val, Type *type,
                                 LLVMBasicBlockRef match_bb, LLVMBasicBlockRef fail_bb) {
    if (!pat) {
        LLVMBuildBr(cg->builder, match_bb);
        return;
    }

    switch (pat->kind) {
    case PAT_BIND:
    case PAT_WILDCARD:
        /* Always matches */
        LLVMBuildBr(cg->builder, match_bb);
        break;

    case PAT_LITERAL: {
        LLVMValueRef pat_val = codegen_expr(cg, pat->as.literal);
        if (!pat_val || !val) {
            LLVMBuildBr(cg->builder, fail_bb);
            break;
        }
        Type *resolved = type_resolve(type);
        LLVMValueRef cmp;
        if (resolved && resolved->kind == TY_FLOAT) {
            cmp = LLVMBuildFCmp(cg->builder, LLVMRealOEQ, val, pat_val, "pat.cmp");
        } else {
            cmp = LLVMBuildICmp(cg->builder, LLVMIntEQ, val, pat_val, "pat.cmp");
        }
        LLVMBuildCondBr(cg->builder, cmp, match_bb, fail_bb);
        break;
    }

    case PAT_TUPLE: {
        Type *resolved = type_resolve(type);
        if (!resolved || resolved->kind != TY_TUPLE) {
            LLVMBuildBr(cg->builder, fail_bb);
            break;
        }
        /* Test each sub-pattern; chain through intermediate blocks */
        int count = pat->as.tuple.count;
        if (count == 0) {
            LLVMBuildBr(cg->builder, match_bb);
            break;
        }
        /* Create intermediate blocks for elements 1..count-1 */
        for (int i = 0; i < count; i++) {
            LLVMBasicBlockRef next;
            if (i + 1 < count) {
                next = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "pat.tup");
            } else {
                next = match_bb;
            }
            LLVMValueRef elem = LLVMBuildExtractValue(cg->builder, val, i, "tup.el");
            codegen_pattern_test(cg, pat->as.tuple.items[i], elem,
                                resolved->as.tuple.elems[i], next, fail_bb);
            if (i + 1 < count) {
                LLVMPositionBuilderAtEnd(cg->builder, next);
            }
        }
        break;
    }

    case PAT_OR: {
        /* Try each alternative: succeed on first match */
        int count = pat->as.or_pats.count;
        for (int i = 0; i < count; i++) {
            LLVMBasicBlockRef next = (i + 1 < count) ?
                LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "pat.or") : fail_bb;
            codegen_pattern_test(cg, pat->as.or_pats.items[i], val, type, match_bb, next);
            if (i + 1 < count) {
                LLVMPositionBuilderAtEnd(cg->builder, next);
            }
        }
        break;
    }

    case PAT_BIND_AS:
        /* Test the inner pattern */
        codegen_pattern_test(cg, pat->as.bind_as.pattern, val, type, match_bb, fail_bb);
        break;

    case PAT_VARIANT: {
        /* Check the tag matches */
        Type *resolved = type_resolve(type);
        if (!resolved || resolved->kind != TY_ENUM) {
            LLVMBuildBr(cg->builder, fail_bb);
            break;
        }
        /* Find variant index */
        const char *vname = cg_intern(cg,
            pat->as.variant.path.segments[0].name,
            pat->as.variant.path.segments[0].len);
        int variant_idx = -1;
        for (int i = 0; i < resolved->as.enumt.variant_count; i++) {
            if (resolved->as.enumt.variants[i].name == vname) {
                variant_idx = i;
                break;
            }
        }
        if (variant_idx < 0) {
            LLVMBuildBr(cg->builder, fail_bb);
            break;
        }

        /* Extract tag from the enum value - need to store to memory first */
        LLVMTypeRef enum_ty = llvm_type(cg, resolved);
        LLVMValueRef tmp = create_entry_alloca(cg, enum_ty, "pat.enum");
        LLVMBuildStore(cg->builder, val, tmp);
        LLVMValueRef tag_ptr = LLVMBuildStructGEP2(cg->builder, enum_ty, tmp, 0, "tag.ptr");
        LLVMTypeRef i32_ty = LLVMInt32TypeInContext(cg->ctx);
        LLVMValueRef tag = LLVMBuildLoad2(cg->builder, i32_ty, tag_ptr, "tag");

        LLVMValueRef cmp = LLVMBuildICmp(cg->builder, LLVMIntEQ, tag,
            LLVMConstInt(i32_ty, variant_idx, 0), "tag.cmp");

        if (pat->as.variant.args.count > 0) {
            /* Need to test payload patterns too */
            LLVMBasicBlockRef payload_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "pat.payload");
            LLVMBuildCondBr(cg->builder, cmp, payload_bb, fail_bb);
            LLVMPositionBuilderAtEnd(cg->builder, payload_bb);
            /* For now, just match on tag — payload patterns are bound in codegen_pattern_bind */
            LLVMBuildBr(cg->builder, match_bb);
        } else {
            LLVMBuildCondBr(cg->builder, cmp, match_bb, fail_bb);
        }
        break;
    }

    default:
        /* Unknown pattern — always match (best effort) */
        LLVMBuildBr(cg->builder, match_bb);
        break;
    }
}

static void codegen_stmt(Codegen *cg, AstNode *node) {
    if (!node) return;

    switch (node->kind) {
    case NODE_LET:
    case NODE_VAR: {
        if (!node->as.let.pattern) break;

        Type *ty = node->checked_type ? type_resolve(node->checked_type) : type_int();

        if (node->as.let.pattern->kind == PAT_BIND) {
            const char *name = cg_intern(cg, node->as.let.pattern->as.bind.name,
                                         node->as.let.pattern->as.bind.len);
            LLVMTypeRef llvm_ty = llvm_type(cg, ty);
            LLVMValueRef alloca_val = create_entry_alloca(cg, llvm_ty, name);

            if (node->as.let.value) {
                LLVMValueRef val = codegen_expr(cg, node->as.let.value);
                if (val) LLVMBuildStore(cg->builder, val, alloca_val);
            }
            set_value(cg, name, alloca_val);
        } else {
            /* Destructuring pattern (tuple, record, etc.) */
            if (node->as.let.value) {
                LLVMValueRef val = codegen_expr(cg, node->as.let.value);
                if (val) codegen_pattern_bind(cg, node->as.let.pattern, val, ty);
            }
        }
        break;
    }

    case NODE_ASSIGN: {
        LLVMValueRef val = codegen_expr(cg, node->as.assign.value);
        if (!val) break;

        if (node->as.assign.target->kind == NODE_FIELD_ACCESS) {
            /* Field assignment: record.field = value */
            AstNode *fa = node->as.assign.target;
            Type *obj_type = fa->as.field_access.object->checked_type ?
                             type_resolve(fa->as.field_access.object->checked_type) : NULL;
            if (obj_type && obj_type->kind == TY_RECORD &&
                fa->as.field_access.object->kind == NODE_IDENT) {
                const char *obj_name = cg_intern(cg,
                    fa->as.field_access.object->as.ident.name,
                    fa->as.field_access.object->as.ident.len);
                LLVMValueRef obj_alloca = get_value(cg, obj_name);
                if (obj_alloca) {
                    const char *fname = cg_intern(cg, fa->as.field_access.field.name,
                                                  fa->as.field_access.field.len);
                    int field_idx = -1;
                    for (int i = 0; i < obj_type->as.record.field_count; i++) {
                        if (obj_type->as.record.field_names[i] == fname) {
                            field_idx = i;
                            break;
                        }
                    }
                    if (field_idx >= 0) {
                        LLVMTypeRef struct_ty = llvm_type(cg, obj_type);
                        LLVMValueRef gep = LLVMBuildStructGEP2(cg->builder, struct_ty,
                                                                obj_alloca, field_idx, "field.ptr");
                        if (node->as.assign.op == TOK_ASSIGN) {
                            LLVMBuildStore(cg->builder, val, gep);
                        }
                    }
                }
            }
        } else if (node->as.assign.target->kind == NODE_INDEX) {
            /* List index assignment: list[i] = value */
            AstNode *idx_node = node->as.assign.target;
            Type *obj_type = idx_node->as.index.object->checked_type ?
                             type_resolve(idx_node->as.index.object->checked_type) : NULL;
            if (obj_type && obj_type->kind == TY_LIST) {
                Type *elem_type = obj_type->as.list.elem;
                LLVMTypeRef elem_llvm_ty = llvm_type(cg, elem_type);
                LLVMTypeRef list_llvm_ty = llvm_type(cg, obj_type);
                LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);

                /* Get the list alloca */
                LLVMValueRef list_alloca = NULL;
                if (idx_node->as.index.object->kind == NODE_IDENT) {
                    const char *obj_name = cg_intern(cg,
                        idx_node->as.index.object->as.ident.name,
                        idx_node->as.index.object->as.ident.len);
                    list_alloca = get_value(cg, obj_name);
                }
                if (list_alloca) {
                    /* Load data pointer (field 2) */
                    LLVMValueRef data_gep = LLVMBuildStructGEP2(cg->builder, list_llvm_ty,
                                                                  list_alloca, 2, "data.gep");
                    LLVMValueRef data_ptr = LLVMBuildLoad2(cg->builder, ptr_ty, data_gep, "data.ptr");

                    /* Compute byte offset */
                    LLVMValueRef idx = codegen_expr(cg, idx_node->as.index.index);
                    if (idx) {
                        LLVMTargetDataRef td = LLVMGetModuleDataLayout(cg->module);
                        unsigned long long elem_size = LLVMABISizeOfType(td, elem_llvm_ty);
                        LLVMValueRef byte_offset = LLVMBuildMul(cg->builder, idx,
                            LLVMConstInt(LLVMInt64TypeInContext(cg->ctx), elem_size, 0), "byte.off");
                        LLVMValueRef byte_off32 = LLVMBuildTrunc(cg->builder, byte_offset,
                            LLVMInt32TypeInContext(cg->ctx), "off32");
                        LLVMValueRef elem_ptr = LLVMBuildGEP2(cg->builder,
                            LLVMInt8TypeInContext(cg->ctx), data_ptr, &byte_off32, 1, "elem.ptr");

                        /* Store value */
                        LLVMBuildStore(cg->builder, val, elem_ptr);
                    }
                }
            }
        } else if (node->as.assign.target->kind == NODE_IDENT) {
            const char *name = cg_intern(cg, node->as.assign.target->as.ident.name,
                                         node->as.assign.target->as.ident.len);
            LLVMValueRef alloca_val = get_value(cg, name);
            if (!alloca_val) break;

            if (node->as.assign.op == TOK_ASSIGN) {
                LLVMBuildStore(cg->builder, val, alloca_val);
            } else {
                /* Compound assign */
                Type *ty = node->as.assign.target->checked_type ?
                           type_resolve(node->as.assign.target->checked_type) : type_int();
                LLVMTypeRef load_ty = llvm_type(cg, ty);
                LLVMValueRef current = LLVMBuildLoad2(cg->builder, load_ty, alloca_val, "cur");
                LLVMValueRef result = NULL;
                bool is_float = (ty->kind == TY_FLOAT);

                switch (node->as.assign.op) {
                case TOK_PLUS_ASSIGN:
                    result = is_float ? LLVMBuildFAdd(cg->builder, current, val, "add")
                                      : LLVMBuildAdd(cg->builder, current, val, "add");
                    break;
                case TOK_MINUS_ASSIGN:
                    result = is_float ? LLVMBuildFSub(cg->builder, current, val, "sub")
                                      : LLVMBuildSub(cg->builder, current, val, "sub");
                    break;
                case TOK_STAR_ASSIGN:
                    result = is_float ? LLVMBuildFMul(cg->builder, current, val, "mul")
                                      : LLVMBuildMul(cg->builder, current, val, "mul");
                    break;
                case TOK_SLASH_ASSIGN:
                    result = is_float ? LLVMBuildFDiv(cg->builder, current, val, "div")
                                      : LLVMBuildSDiv(cg->builder, current, val, "div");
                    break;
                case TOK_PERCENT_ASSIGN:
                    result = is_float ? LLVMBuildFRem(cg->builder, current, val, "rem")
                                      : LLVMBuildSRem(cg->builder, current, val, "rem");
                    break;
                case TOK_AMP_ASSIGN:
                    result = LLVMBuildAnd(cg->builder, current, val, "band");
                    break;
                case TOK_PIPE_ASSIGN:
                    result = LLVMBuildOr(cg->builder, current, val, "bor");
                    break;
                case TOK_CARET_ASSIGN:
                    result = LLVMBuildXor(cg->builder, current, val, "bxor");
                    break;
                case TOK_SHL_ASSIGN:
                    result = LLVMBuildShl(cg->builder, current, val, "shl");
                    break;
                case TOK_SHR_ASSIGN:
                    result = LLVMBuildAShr(cg->builder, current, val, "shr");
                    break;
                case TOK_AND_ASSIGN:
                    result = LLVMBuildAnd(cg->builder, current, val, "land");
                    break;
                case TOK_OR_ASSIGN:
                    result = LLVMBuildOr(cg->builder, current, val, "lor");
                    break;
                default:
                    result = val;
                    break;
                }
                if (result) LLVMBuildStore(cg->builder, result, alloca_val);
            }
        }
        break;
    }

    case NODE_RETURN: {
        /* Emit deferred expressions in reverse order before returning */
        for (DeferEntry *d = cg->defer_stack; d; d = d->next) {
            codegen_expr(cg, d->body);
        }
        if (node->as.unary_stmt.value) {
            LLVMValueRef val = codegen_expr(cg, node->as.unary_stmt.value);
            if (val) {
                LLVMBuildRet(cg->builder, val);
            } else {
                LLVMBuildRetVoid(cg->builder);
            }
        } else {
            LLVMBuildRetVoid(cg->builder);
        }
        break;
    }

    case NODE_EXPR_STMT:
        codegen_expr(cg, node->as.expr_stmt.expr);
        break;

    case NODE_IF:
    case NODE_WHILE:
    case NODE_LOOP:
    case NODE_FOR:
    case NODE_MATCH:
    case NODE_DO_BLOCK:
    case NODE_BLOCK:
        codegen_expr(cg, node);
        break;

    case NODE_ASSERT: {
        LLVMValueRef cond = codegen_expr(cg, node->as.assert_stmt.cond);
        if (!cond) break;

        /* Ensure cond is i1 */
        Type *cond_type = node->as.assert_stmt.cond->checked_type ?
                          type_resolve(node->as.assert_stmt.cond->checked_type) : NULL;
        if (cond_type && cond_type->kind != TY_BOOL) {
            cond = LLVMBuildICmp(cg->builder, LLVMIntNE, cond,
                                 LLVMConstInt(LLVMTypeOf(cond), 0, 0), "tobool");
        }

        LLVMBasicBlockRef fail_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "assert.fail");
        LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlockInContext(cg->ctx, cg->current_fn, "assert.ok");
        LLVMBuildCondBr(cg->builder, cond, cont_bb, fail_bb);

        /* Fail block: print message and exit */
        LLVMPositionBuilderAtEnd(cg->builder, fail_bb);

        /* Get or declare fprintf and exit */
        LLVMValueRef fprintf_fn = LLVMGetNamedFunction(cg->module, "fprintf");
        if (!fprintf_fn) {
            LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
            LLVMTypeRef params[] = { ptr_ty, ptr_ty };
            LLVMTypeRef ft = LLVMFunctionType(LLVMInt32TypeInContext(cg->ctx), params, 2, 1);
            fprintf_fn = LLVMAddFunction(cg->module, "fprintf", ft);
        }
        LLVMValueRef exit_fn = LLVMGetNamedFunction(cg->module, "exit");
        if (!exit_fn) {
            LLVMTypeRef params[] = { LLVMInt32TypeInContext(cg->ctx) };
            LLVMTypeRef ft = LLVMFunctionType(LLVMVoidTypeInContext(cg->ctx), params, 1, 0);
            exit_fn = LLVMAddFunction(cg->module, "exit", ft);
        }
        LLVMValueRef stderr_fn = LLVMGetNamedFunction(cg->module, "stderr");

        /* Build error message */
        char msg[256];
        snprintf(msg, sizeof(msg), "assertion failed at %s:%d:%d\n",
                 node->loc.file, node->loc.line, node->loc.col);
        LLVMValueRef msg_str = LLVMBuildGlobalStringPtr(cg->builder, msg, ".assert.msg");

        if (stderr_fn) {
            LLVMValueRef stderr_val = LLVMBuildLoad2(cg->builder,
                LLVMPointerTypeInContext(cg->ctx, 0), stderr_fn, "stderr");
            LLVMValueRef args[] = { stderr_val, msg_str };
            LLVMBuildCall2(cg->builder, LLVMGlobalGetValueType(fprintf_fn),
                          fprintf_fn, args, 2, "");
        } else {
            /* Fall back to printf */
            LLVMValueRef printf_fn = LLVMGetNamedFunction(cg->module, "printf");
            if (!printf_fn) {
                LLVMTypeRef ptr_ty = LLVMPointerTypeInContext(cg->ctx, 0);
                LLVMTypeRef ft = LLVMFunctionType(LLVMInt64TypeInContext(cg->ctx), &ptr_ty, 1, 1);
                printf_fn = LLVMAddFunction(cg->module, "printf", ft);
            }
            LLVMValueRef args[] = { msg_str };
            LLVMBuildCall2(cg->builder, LLVMGlobalGetValueType(printf_fn),
                          printf_fn, args, 1, "");
        }

        LLVMValueRef exit_args[] = { LLVMConstInt(LLVMInt32TypeInContext(cg->ctx), 1, 0) };
        LLVMBuildCall2(cg->builder, LLVMGlobalGetValueType(exit_fn),
                      exit_fn, exit_args, 1, "");
        LLVMBuildUnreachable(cg->builder);

        LLVMPositionBuilderAtEnd(cg->builder, cont_bb);
        break;
    }

    case NODE_DEFER: {
        /* Track defer expressions — emit at scope exit (before return) */
        /* For now, defer is a no-op in simple cases; we'll handle it in returns */
        /* Store the defer body for later emission */
        if (!cg->defer_stack) {
            cg->defer_stack = NULL;
        }
        DeferEntry *de = (DeferEntry *)arena_alloc(cg->arena, sizeof(DeferEntry));
        de->body = node->as.defer.body;
        de->next = cg->defer_stack;
        cg->defer_stack = de;
        break;
    }

    case NODE_BREAK:
        if (cg->loop_stack) {
            LLVMBuildBr(cg->builder, cg->loop_stack->exit_bb);
        }
        break;

    case NODE_CONTINUE:
        if (cg->loop_stack) {
            LLVMBasicBlockRef target = cg->loop_stack->cond_bb ?
                                       cg->loop_stack->cond_bb :
                                       LLVMGetInsertBlock(cg->builder);
            LLVMBuildBr(cg->builder, target);
        }
        break;

    default:
        break;
    }
}

static void codegen_block(Codegen *cg, NodeList block) {
    for (int i = 0; i < block.count; i++) {
        /* Don't emit after a terminator */
        if (block_has_terminator(LLVMGetInsertBlock(cg->builder))) break;
        codegen_stmt(cg, block.items[i]);
    }
}

/* ===== AST type resolution for codegen ===== */

static Type *cg_resolve_ast_type(Codegen *cg, AstType *at) {
    if (!at) return type_none();
    switch (at->kind) {
    case TYPE_PATH: {
        if (at->as.path.path.seg_count == 1) {
            const char *tname = at->as.path.path.segments[0].name;
            int tlen = at->as.path.path.segments[0].len;
            const char *interned = str_intern_range(cg->interns, tname, tlen);
            if (interned == str_intern(cg->interns, "Int")) return type_int();
            if (interned == str_intern(cg->interns, "Float")) return type_float();
            if (interned == str_intern(cg->interns, "Bool")) return type_bool();
            if (interned == str_intern(cg->interns, "Char")) return type_char();
            if (interned == str_intern(cg->interns, "String")) return type_string();
            if (interned == str_intern(cg->interns, "None")) return type_none();
            /* Check type substitution map (for generics) */
            if (cg->type_substs) {
                for (int i = 0; i < cg->type_subst_count; i++) {
                    if (cg->type_substs[i].name == interned)
                        return cg->type_substs[i].concrete;
                }
            }
            /* Look up user-defined types via checker's symtab */
            Symbol *sym = symtab_lookup(&cg->symtab, interned);
            if (sym && sym->kind == SYM_TYPE) return sym->type;
        }
        return type_int(); /* fallback */
    }
    case TYPE_POINTER:
        return type_ptr(cg->arena, cg_resolve_ast_type(cg, at->as.pointer_elem));
    case TYPE_TUPLE: {
        int n = at->as.tuple.count;
        Type **elems = (Type **)arena_alloc(cg->arena, n * sizeof(Type *));
        for (int i = 0; i < n; i++) {
            elems[i] = cg_resolve_ast_type(cg, at->as.tuple.items[i]);
        }
        return type_tuple(cg->arena, elems, n);
    }
    case TYPE_FN: {
        int n = at->as.fn.params.count;
        Type **params = NULL;
        if (n > 0) {
            params = (Type **)arena_alloc(cg->arena, n * sizeof(Type *));
            for (int i = 0; i < n; i++) {
                params[i] = cg_resolve_ast_type(cg, at->as.fn.params.items[i]);
            }
        }
        Type *ret = cg_resolve_ast_type(cg, at->as.fn.ret);
        return type_fn(cg->arena, params, n, ret, false);
    }
    case TYPE_LIST:
        return type_list(cg->arena, cg_resolve_ast_type(cg, at->as.list_elem));
    case TYPE_MAP: {
        Type *key = cg_resolve_ast_type(cg, at->as.map.key);
        Type *val = cg_resolve_ast_type(cg, at->as.map.val);
        return type_map(cg->arena, key, val);
    }
    default:
        return type_int(); /* fallback */
    }
}

/* ===== Function codegen ===== */

static void codegen_extern_fn(Codegen *cg, AstNode *node) {
    const char *name = cg_intern(cg, node->as.extern_fn.name.name,
                                 node->as.extern_fn.name.len);

    int param_count = 0;
    bool is_variadic = false;
    for (int i = 0; i < node->as.extern_fn.param_count; i++) {
        if (node->as.extern_fn.params[i].is_variadic)
            is_variadic = true;
        else
            param_count++;
    }

    LLVMTypeRef *param_types = NULL;
    if (param_count > 0) {
        param_types = (LLVMTypeRef *)malloc(param_count * sizeof(LLVMTypeRef));
        int idx = 0;
        for (int i = 0; i < node->as.extern_fn.param_count; i++) {
            if (!node->as.extern_fn.params[i].is_variadic) {
                Type *pt = cg_resolve_ast_type(cg, node->as.extern_fn.params[i].type);
                param_types[idx++] = llvm_type(cg, pt);
            }
        }
    }

    Type *ret = cg_resolve_ast_type(cg, node->as.extern_fn.ret_type);
    LLVMTypeRef ret_ty = llvm_type(cg, ret);
    LLVMTypeRef fn_ty = LLVMFunctionType(ret_ty, param_types, param_count, is_variadic);
    LLVMAddFunction(cg->module, name, fn_ty);
    free(param_types);
}

/* ===== Monomorphization for generic functions ===== */

/* Build a mangled name like "max_Int" or "pair_Int_Float" */
static const char *build_mono_name(Codegen *cg, const char *base_name,
                                    Type **concrete_types, int count) {
    char buf[512];
    int pos = snprintf(buf, sizeof(buf), "%s", base_name);
    for (int i = 0; i < count && pos < (int)sizeof(buf) - 1; i++) {
        pos += snprintf(buf + pos, sizeof(buf) - pos, "_%s",
                        type_to_string(concrete_types[i]));
    }
    return str_intern(cg->interns, buf);
}

/* Look up cached monomorphization */
static LLVMValueRef mono_cache_lookup(Codegen *cg, const char *mangled_name) {
    for (int i = 0; i < cg->mono_cache_count; i++) {
        if (cg->mono_cache[i].mangled_name == mangled_name)
            return cg->mono_cache[i].fn;
    }
    return NULL;
}

/* Cache a monomorphized function */
static void mono_cache_store(Codegen *cg, const char *mangled_name, LLVMValueRef fn) {
    if (cg->mono_cache_count >= cg->mono_cache_cap) {
        cg->mono_cache_cap = cg->mono_cache_cap ? cg->mono_cache_cap * 2 : 16;
        cg->mono_cache = realloc(cg->mono_cache,
                                  cg->mono_cache_cap * sizeof(*cg->mono_cache));
    }
    cg->mono_cache[cg->mono_cache_count].mangled_name = mangled_name;
    cg->mono_cache[cg->mono_cache_count].fn = fn;
    cg->mono_cache_count++;
}

/* Find a generic function AST node by name */
static AstNode *find_generic_fn(Codegen *cg, const char *name) {
    for (int i = 0; i < cg->generic_fn_count; i++) {
        const char *fn_name = cg_intern(cg, cg->generic_fns[i]->as.fn_decl.name.name,
                                         cg->generic_fns[i]->as.fn_decl.name.len);
        if (fn_name == name) return cg->generic_fns[i];
    }
    return NULL;
}

/* Monomorphize a generic function with concrete type substitutions.
   Returns the LLVMValueRef of the specialized function. */
static LLVMValueRef codegen_monomorphize(Codegen *cg, AstNode *generic_fn,
                                          Type **concrete_types, int type_count) {
    const char *base_name = cg_intern(cg, generic_fn->as.fn_decl.name.name,
                                       generic_fn->as.fn_decl.name.len);
    const char *mangled = build_mono_name(cg, base_name, concrete_types, type_count);

    /* Check cache */
    LLVMValueRef cached = mono_cache_lookup(cg, mangled);
    if (cached) return cached;

    /* Set up type substitution map */
    int saved_subst_count = cg->type_subst_count;
    struct CgTypeSubst *saved_substs = cg->type_substs;

    cg->type_subst_count = type_count;
    cg->type_substs = (struct CgTypeSubst *)malloc(type_count * sizeof(struct CgTypeSubst));
    for (int i = 0; i < type_count; i++) {
        TypeParam *tp = &generic_fn->as.fn_decl.type_params.items[i];
        cg->type_substs[i].name = cg_intern(cg, tp->name.name, tp->name.len);
        cg->type_substs[i].concrete = concrete_types[i];
    }

    /* Build the specialized function type */
    int param_count = generic_fn->as.fn_decl.params.count;
    LLVMTypeRef *param_types = NULL;
    if (param_count > 0) {
        param_types = (LLVMTypeRef *)malloc(param_count * sizeof(LLVMTypeRef));
        for (int i = 0; i < param_count; i++) {
            Param *p = &generic_fn->as.fn_decl.params.items[i];
            Type *pt = cg_resolve_ast_type(cg, p->type);
            /* Resolve TY_PARAM through substitution map */
            pt = cg_resolve_param(cg, pt);
            param_types[i] = llvm_type(cg, pt);
        }
    }

    Type *ret = cg_resolve_ast_type(cg, generic_fn->as.fn_decl.ret_type);
    ret = cg_resolve_param(cg, ret);
    LLVMTypeRef ret_ty = llvm_type(cg, ret);
    LLVMTypeRef fn_ty = LLVMFunctionType(ret_ty, param_types, param_count, false);
    LLVMValueRef fn = LLVMAddFunction(cg->module, mangled, fn_ty);

    /* Cache before generating body (for recursive generics) */
    mono_cache_store(cg, mangled, fn);

    /* Save current function context */
    LLVMValueRef saved_fn = cg->current_fn;
    Type *saved_ret = cg->current_ret_type;
    DeferEntry *saved_defers = cg->defer_stack;
    LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(cg->builder);

    cg->current_fn = fn;
    cg->current_ret_type = ret;
    cg->defer_stack = NULL;

    /* Create entry block */
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cg->ctx, fn, "entry");
    LLVMPositionBuilderAtEnd(cg->builder, entry);

    push_value_scope(cg);

    /* Alloca + store for each parameter */
    for (int i = 0; i < param_count; i++) {
        Param *p = &generic_fn->as.fn_decl.params.items[i];
        if (!p->pattern || p->pattern->kind != PAT_BIND) continue;
        const char *pname = cg_intern(cg, p->pattern->as.bind.name,
                                      p->pattern->as.bind.len);
        LLVMValueRef alloca_val = LLVMBuildAlloca(cg->builder, param_types[i], pname);
        LLVMBuildStore(cg->builder, LLVMGetParam(fn, i), alloca_val);
        set_value(cg, pname, alloca_val);
    }

    /* Generate body */
    codegen_block(cg, generic_fn->as.fn_decl.body);

    /* Add implicit return if needed */
    LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(cg->builder);
    if (!block_has_terminator(current_bb)) {
        for (DeferEntry *d = cg->defer_stack; d; d = d->next) {
            codegen_expr(cg, d->body);
        }
        if (ret->kind == TY_NONE) {
            LLVMBuildRetVoid(cg->builder);
        } else {
            LLVMBuildRet(cg->builder, LLVMConstNull(ret_ty));
        }
    }

    pop_value_scope(cg);

    /* Verify */
    if (LLVMVerifyFunction(fn, LLVMPrintMessageAction)) {
        fprintf(stderr, "codegen: verification failed for monomorphized function '%s'\n", mangled);
    }

    /* Restore context */
    cg->current_fn = saved_fn;
    cg->current_ret_type = saved_ret;
    cg->defer_stack = saved_defers;
    if (saved_bb) LLVMPositionBuilderAtEnd(cg->builder, saved_bb);

    /* Restore substitution map */
    free(cg->type_substs);
    cg->type_substs = saved_substs;
    cg->type_subst_count = saved_subst_count;

    free(param_types);
    return fn;
}

static void codegen_fn_decl(Codegen *cg, AstNode *node) {
    if (node->as.fn_decl.is_signature) return;

    const char *name = cg_intern(cg, node->as.fn_decl.name.name,
                                 node->as.fn_decl.name.len);
    int param_count = node->as.fn_decl.params.count;

    LLVMTypeRef *param_types = NULL;
    Type **semantic_param_types = NULL;
    if (param_count > 0) {
        param_types = (LLVMTypeRef *)malloc(param_count * sizeof(LLVMTypeRef));
        semantic_param_types = (Type **)malloc(param_count * sizeof(Type *));
        for (int i = 0; i < param_count; i++) {
            Param *p = &node->as.fn_decl.params.items[i];
            Type *pt = cg_resolve_ast_type(cg, p->type);
            semantic_param_types[i] = pt;
            param_types[i] = llvm_type(cg, pt);
        }
    }

    Type *ret = cg_resolve_ast_type(cg, node->as.fn_decl.ret_type);

    LLVMTypeRef ret_ty = llvm_type(cg, ret);
    LLVMTypeRef fn_ty = LLVMFunctionType(ret_ty, param_types, param_count, false);
    LLVMValueRef fn = LLVMAddFunction(cg->module, name, fn_ty);

    cg->current_fn = fn;
    cg->current_ret_type = ret;

    /* Save and reset defer stack for this function */
    DeferEntry *saved_defers = cg->defer_stack;
    cg->defer_stack = NULL;

    /* Create entry block */
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cg->ctx, fn, "entry");
    LLVMPositionBuilderAtEnd(cg->builder, entry);

    push_value_scope(cg);

    /* Alloca + store for each parameter */
    for (int i = 0; i < param_count; i++) {
        Param *p = &node->as.fn_decl.params.items[i];
        if (!p->pattern || p->pattern->kind != PAT_BIND) continue;

        const char *pname = cg_intern(cg, p->pattern->as.bind.name,
                                      p->pattern->as.bind.len);
        LLVMValueRef alloca_val = LLVMBuildAlloca(cg->builder, param_types[i], pname);
        LLVMBuildStore(cg->builder, LLVMGetParam(fn, i), alloca_val);
        set_value(cg, pname, alloca_val);
    }

    /* Generate body */
    codegen_block(cg, node->as.fn_decl.body);

    /* Add implicit return if needed */
    LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(cg->builder);
    if (!block_has_terminator(current_bb)) {
        /* Emit deferred expressions before implicit return */
        for (DeferEntry *d = cg->defer_stack; d; d = d->next) {
            codegen_expr(cg, d->body);
        }
        if (ret->kind == TY_NONE) {
            LLVMBuildRetVoid(cg->builder);
        } else {
            LLVMBuildRet(cg->builder, LLVMConstInt(ret_ty, 0, 0));
        }
    }

    /* Restore defer stack */
    cg->defer_stack = saved_defers;

    pop_value_scope(cg);

    free(param_types);
    free(semantic_param_types);

    /* Verify function */
    if (LLVMVerifyFunction(fn, LLVMPrintMessageAction)) {
        fprintf(stderr, "codegen: verification failed for function '%s'\n", name);
    }
}

static void codegen_method_decl(Codegen *cg, AstNode *node) {
    /* Get type name and method name */
    const char *type_name = cg_intern(cg,
        node->as.method_decl.type_path.segments[0].name,
        node->as.method_decl.type_path.segments[0].len);
    const char *method_name = cg_intern(cg,
        node->as.method_decl.method_name.name,
        node->as.method_decl.method_name.len);

    /* Mangle: TypeName_methodName */
    char mangled[256];
    snprintf(mangled, sizeof(mangled), "%s_%s", type_name, method_name);
    const char *name = str_intern(cg->interns, mangled);

    /* Get the method's function type from checked_type */
    Type *fn_type = node->checked_type ? type_resolve(node->checked_type) : NULL;
    if (!fn_type || fn_type->kind != TY_FN) return;

    int total_params = fn_type->as.fn.param_count;
    LLVMTypeRef *param_types = (LLVMTypeRef *)malloc(total_params * sizeof(LLVMTypeRef));
    for (int i = 0; i < total_params; i++) {
        param_types[i] = llvm_type(cg, fn_type->as.fn.param_types[i]);
    }

    Type *ret = fn_type->as.fn.ret;
    LLVMTypeRef ret_ty = llvm_type(cg, ret);
    LLVMTypeRef llvm_fn_ty = LLVMFunctionType(ret_ty, param_types, total_params, false);
    LLVMValueRef fn = LLVMAddFunction(cg->module, name, llvm_fn_ty);

    cg->current_fn = fn;
    cg->current_ret_type = ret;
    DeferEntry *saved_defers = cg->defer_stack;
    cg->defer_stack = NULL;

    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cg->ctx, fn, "entry");
    LLVMPositionBuilderAtEnd(cg->builder, entry);
    push_value_scope(cg);

    /* Bind all parameters (self is in the param list) */
    for (int i = 0; i < total_params; i++) {
        Param *p = &node->as.method_decl.params.items[i];
        if (p->is_self) {
            const char *self_nm = str_intern(cg->interns, "self");
            LLVMValueRef self_alloca = LLVMBuildAlloca(cg->builder, param_types[i], self_nm);
            LLVMBuildStore(cg->builder, LLVMGetParam(fn, i), self_alloca);
            set_value(cg, self_nm, self_alloca);
        } else if (p->pattern && p->pattern->kind == PAT_BIND) {
            const char *pname = cg_intern(cg, p->pattern->as.bind.name, p->pattern->as.bind.len);
            LLVMValueRef alloca_val = LLVMBuildAlloca(cg->builder, param_types[i], pname);
            LLVMBuildStore(cg->builder, LLVMGetParam(fn, i), alloca_val);
            set_value(cg, pname, alloca_val);
        }
    }

    /* Generate body */
    codegen_block(cg, node->as.method_decl.body);

    LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(cg->builder);
    if (!block_has_terminator(current_bb)) {
        for (DeferEntry *d = cg->defer_stack; d; d = d->next) {
            codegen_expr(cg, d->body);
        }
        if (ret->kind == TY_NONE) {
            LLVMBuildRetVoid(cg->builder);
        } else {
            LLVMBuildRet(cg->builder, LLVMConstInt(ret_ty, 0, 0));
        }
    }

    cg->defer_stack = saved_defers;
    pop_value_scope(cg);
    free(param_types);

    if (LLVMVerifyFunction(fn, LLVMPrintMessageAction)) {
        fprintf(stderr, "codegen: verification failed for method '%s'\n", name);
    }
}

/* ===== Top-level codegen ===== */

void codegen_init(Codegen *cg, const char *module_name, Arena *arena, InternTable *interns, SymTab *checker_symtab) {
    memset(cg, 0, sizeof(Codegen));
    cg->arena = arena;
    cg->interns = interns;
    if (checker_symtab) {
        cg->symtab = *checker_symtab; /* copy the symtab for type lookups */
    }
    cg->ctx = LLVMContextCreate();
    cg->module = LLVMModuleCreateWithNameInContext(module_name, cg->ctx);
    cg->builder = LLVMCreateBuilderInContext(cg->ctx);
    cg->values = NULL;
    cg->loop_stack = NULL;

    /* Initialize target */
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    char *triple = LLVMGetDefaultTargetTriple();
    LLVMSetTarget(cg->module, triple);

    LLVMTargetRef target;
    char *error = NULL;
    if (LLVMGetTargetFromTriple(triple, &target, &error)) {
        fprintf(stderr, "codegen: could not get target: %s\n", error);
        LLVMDisposeMessage(error);
    } else {
        cg->target_machine = LLVMCreateTargetMachine(
            target, triple, "generic", "",
            LLVMCodeGenLevelDefault, LLVMRelocPIC, LLVMCodeModelDefault);
        LLVMTargetDataRef data_layout = LLVMCreateTargetDataLayout(cg->target_machine);
        LLVMSetModuleDataLayout(cg->module, data_layout);
        LLVMDisposeTargetData(data_layout);
    }

    LLVMDisposeMessage(triple);

    push_value_scope(cg); /* global scope */
}

void codegen_program(Codegen *cg, AstNode *program) {
    if (!program || program->kind != NODE_PROGRAM) return;

    /* Pass 1: declare extern functions */
    for (int i = 0; i < program->as.program.count; i++) {
        AstNode *item = program->as.program.items[i];
        if (item->kind == NODE_EXTERN_BLOCK) {
            for (int j = 0; j < item->as.extern_block.decls.count; j++) {
                AstNode *decl = item->as.extern_block.decls.items[j];
                if (decl->kind == NODE_EXTERN_FN) {
                    codegen_extern_fn(cg, decl);
                }
            }
        }
    }

    /* Pass 1.5: declare constants */
    for (int i = 0; i < program->as.program.count; i++) {
        AstNode *item = program->as.program.items[i];
        if (item->kind == NODE_CONST_DECL && item->as.const_decl.value) {
            const char *name = cg_intern(cg, item->as.const_decl.name.name,
                                         item->as.const_decl.name.len);
            Type *ty = item->checked_type ? type_resolve(item->checked_type) : NULL;
            if (!ty) ty = type_int();
            LLVMTypeRef llvm_ty_val = llvm_type(cg, ty);

            /* Create global constant */
            LLVMValueRef global = LLVMAddGlobal(cg->module, llvm_ty_val, name);
            LLVMSetGlobalConstant(global, 1);
            LLVMSetLinkage(global, LLVMPrivateLinkage);

            /* Try to evaluate initializer as constant */
            AstNode *val_node = item->as.const_decl.value;
            if (val_node->kind == NODE_LITERAL) {
                switch (val_node->as.literal.kind) {
                case LIT_INT: {
                    long long v = strtoll(val_node->as.literal.value, NULL, 10);
                    LLVMSetInitializer(global, LLVMConstInt(llvm_ty_val, (unsigned long long)v, 1));
                    break;
                }
                case LIT_FLOAT: {
                    double v = strtod(val_node->as.literal.value, NULL);
                    LLVMSetInitializer(global, LLVMConstReal(llvm_ty_val, v));
                    break;
                }
                case LIT_BOOL: {
                    int v = (val_node->as.literal.len == 4 &&
                             memcmp(val_node->as.literal.value, "true", 4) == 0) ? 1 : 0;
                    LLVMSetInitializer(global, LLVMConstInt(llvm_ty_val, v, 0));
                    break;
                }
                default:
                    LLVMSetInitializer(global, LLVMConstNull(llvm_ty_val));
                    break;
                }
            } else {
                LLVMSetInitializer(global, LLVMConstNull(llvm_ty_val));
            }
            set_value(cg, name, global);
        }
    }

    /* Collect generic functions (skip in normal codegen) */
    cg->generic_fns = NULL;
    cg->generic_fn_count = 0;
    for (int i = 0; i < program->as.program.count; i++) {
        AstNode *item = program->as.program.items[i];
        if (item->kind == NODE_FN_DECL && item->as.fn_decl.type_params.count > 0) {
            cg->generic_fns = (AstNode **)realloc(cg->generic_fns,
                (cg->generic_fn_count + 1) * sizeof(AstNode *));
            cg->generic_fns[cg->generic_fn_count++] = item;
        }
    }

    /* Pass 2: generate function bodies (skip generics) */
    for (int i = 0; i < program->as.program.count; i++) {
        AstNode *item = program->as.program.items[i];
        if (item->kind == NODE_FN_DECL && item->as.fn_decl.type_params.count == 0) {
            codegen_fn_decl(cg, item);
        }
        if (item->kind == NODE_METHOD_DECL) {
            codegen_method_decl(cg, item);
        }
    }
}

void codegen_emit_ir(Codegen *cg, const char *filename) {
    if (filename) {
        char *error = NULL;
        if (LLVMPrintModuleToFile(cg->module, filename, &error)) {
            fprintf(stderr, "codegen: could not write IR: %s\n", error);
            LLVMDisposeMessage(error);
        }
    } else {
        char *ir = LLVMPrintModuleToString(cg->module);
        printf("%s", ir);
        LLVMDisposeMessage(ir);
    }
}

int codegen_emit_obj(Codegen *cg, const char *filename) {
    if (!cg->target_machine) {
        fprintf(stderr, "codegen: no target machine\n");
        return 1;
    }

    char *error = NULL;
    if (LLVMTargetMachineEmitToFile(cg->target_machine, cg->module,
                                     (char *)filename, LLVMObjectFile, &error)) {
        fprintf(stderr, "codegen: could not emit object file: %s\n", error);
        LLVMDisposeMessage(error);
        return 1;
    }
    return 0;
}

void codegen_destroy(Codegen *cg) {
    if (cg->builder) LLVMDisposeBuilder(cg->builder);
    if (cg->module) LLVMDisposeModule(cg->module);
    if (cg->target_machine) LLVMDisposeTargetMachine(cg->target_machine);
    if (cg->ctx) LLVMContextDispose(cg->ctx);
    free(cg->generic_fns);
    free(cg->mono_cache);
}
