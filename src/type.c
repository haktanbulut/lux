#include "type.h"
#include <stdio.h>
#include <string.h>

/* Singleton primitive types */
static Type ty_int    = { .kind = TY_INT };
static Type ty_float  = { .kind = TY_FLOAT };
static Type ty_bool   = { .kind = TY_BOOL };
static Type ty_char   = { .kind = TY_CHAR };
static Type ty_string = { .kind = TY_STRING };
static Type ty_none   = { .kind = TY_NONE };
static Type ty_error  = { .kind = TY_ERROR };
static Type ty_never  = { .kind = TY_NEVER };

Type *type_int(void)    { return &ty_int; }
Type *type_float(void)  { return &ty_float; }
Type *type_bool(void)   { return &ty_bool; }
Type *type_char(void)   { return &ty_char; }
Type *type_string(void) { return &ty_string; }
Type *type_none(void)   { return &ty_none; }
Type *type_error(void)  { return &ty_error; }
Type *type_never(void)  { return &ty_never; }

Type *type_fn(Arena *a, Type **params, int param_count, Type *ret, bool is_variadic) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_FN;
    if (param_count > 0) {
        t->as.fn.param_types = (Type **)arena_alloc(a, param_count * sizeof(Type *));
        memcpy(t->as.fn.param_types, params, param_count * sizeof(Type *));
    }
    t->as.fn.param_count = param_count;
    t->as.fn.ret = ret;
    t->as.fn.is_variadic = is_variadic;
    return t;
}

Type *type_tuple(Arena *a, Type **elems, int count) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_TUPLE;
    if (count > 0) {
        t->as.tuple.elems = (Type **)arena_alloc(a, count * sizeof(Type *));
        memcpy(t->as.tuple.elems, elems, count * sizeof(Type *));
    }
    t->as.tuple.count = count;
    return t;
}

Type *type_typevar(Arena *a, int id) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_TYPEVAR;
    t->as.typevar.id = id;
    t->as.typevar.resolved = NULL;
    return t;
}

Type *type_param(Arena *a, const char *name) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_PARAM;
    t->as.param.name = name;
    return t;
}

Type *type_ptr(Arena *a, Type *elem) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_PTR;
    t->as.ptr.elem = elem;
    return t;
}

Type *type_list(Arena *a, Type *elem) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_LIST;
    t->as.list.elem = elem;
    return t;
}

Type *type_map(Arena *a, Type *key, Type *val) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_MAP;
    t->as.map.key = key;
    t->as.map.val = val;
    return t;
}

Type *type_record(Arena *a, const char *name, const char **field_names, Type **field_types, int count) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_RECORD;
    t->as.record.name = name;
    t->as.record.field_count = count;
    if (count > 0) {
        t->as.record.field_names = (const char **)arena_alloc(a, count * sizeof(const char *));
        t->as.record.field_types = (Type **)arena_alloc(a, count * sizeof(Type *));
        memcpy(t->as.record.field_names, field_names, count * sizeof(const char *));
        memcpy(t->as.record.field_types, field_types, count * sizeof(Type *));
    }
    return t;
}

Type *type_enum(Arena *a, const char *name, VariantInfo *variants, int variant_count) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_ENUM;
    t->as.enumt.name = name;
    t->as.enumt.variant_count = variant_count;
    if (variant_count > 0) {
        t->as.enumt.variants = (VariantInfo *)arena_alloc(a, variant_count * sizeof(VariantInfo));
        memcpy(t->as.enumt.variants, variants, variant_count * sizeof(VariantInfo));
    }
    return t;
}

Type *type_task(Arena *a, Type *elem) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_TASK;
    t->as.task.elem = elem;
    return t;
}

Type *type_variant(Arena *a, Type *parent_enum, int variant_index) {
    Type *t = (Type *)arena_alloc(a, sizeof(Type));
    memset(t, 0, sizeof(Type));
    t->kind = TY_VARIANT;
    t->as.variant.parent_enum = parent_enum;
    t->as.variant.variant_index = variant_index;
    return t;
}

Type *type_resolve(Type *t) {
    if (!t) return NULL;
    while (t->kind == TY_TYPEVAR && t->as.typevar.resolved) {
        t = t->as.typevar.resolved;
    }
    return t;
}

bool type_equals(Type *a, Type *b) {
    a = type_resolve(a);
    b = type_resolve(b);
    if (!a || !b) return false;
    if (a == b) return true;
    if (a->kind == TY_ERROR || b->kind == TY_ERROR) return true;
    if (a->kind == TY_NEVER || b->kind == TY_NEVER) return true;
    if (a->kind != b->kind) return false;

    switch (a->kind) {
    case TY_INT: case TY_FLOAT: case TY_BOOL:
    case TY_CHAR: case TY_STRING: case TY_NONE:
        return true;
    case TY_FN:
        if (a->as.fn.param_count != b->as.fn.param_count) return false;
        if (a->as.fn.is_variadic != b->as.fn.is_variadic) return false;
        for (int i = 0; i < a->as.fn.param_count; i++) {
            if (!type_equals(a->as.fn.param_types[i], b->as.fn.param_types[i]))
                return false;
        }
        return type_equals(a->as.fn.ret, b->as.fn.ret);
    case TY_TUPLE:
        if (a->as.tuple.count != b->as.tuple.count) return false;
        for (int i = 0; i < a->as.tuple.count; i++) {
            if (!type_equals(a->as.tuple.elems[i], b->as.tuple.elems[i]))
                return false;
        }
        return true;
    case TY_PTR:
        return type_equals(a->as.ptr.elem, b->as.ptr.elem);
    case TY_LIST:
        return type_equals(a->as.list.elem, b->as.list.elem);
    case TY_TASK:
        return type_equals(a->as.task.elem, b->as.task.elem);
    case TY_MAP:
        return type_equals(a->as.map.key, b->as.map.key) &&
               type_equals(a->as.map.val, b->as.map.val);
    case TY_TYPEVAR:
        return a->as.typevar.id == b->as.typevar.id;
    case TY_PARAM:
        return a->as.param.name == b->as.param.name; /* interned pointer equality */
    case TY_RECORD:
        return a->as.record.name == b->as.record.name; /* interned name equality */
    case TY_ENUM:
        return a->as.enumt.name == b->as.enumt.name;
    case TY_VARIANT:
        return type_equals(a->as.variant.parent_enum, b->as.variant.parent_enum) &&
               a->as.variant.variant_index == b->as.variant.variant_index;
    default:
        return false;
    }
}

bool type_is_numeric(Type *t) {
    t = type_resolve(t);
    return t && (t->kind == TY_INT || t->kind == TY_FLOAT);
}

/* Buffer for type_to_string — simple static buffer approach */
static char type_buf[512];

const char *type_to_string(Type *t) {
    t = type_resolve(t);
    if (!t) return "(null)";

    switch (t->kind) {
    case TY_INT:    return "Int";
    case TY_FLOAT:  return "Float";
    case TY_BOOL:   return "Bool";
    case TY_CHAR:   return "Char";
    case TY_STRING: return "String";
    case TY_NONE:   return "None";
    case TY_ERROR:  return "<error>";
    case TY_NEVER:  return "Never";
    case TY_PTR:
        snprintf(type_buf, sizeof(type_buf), "*%s", type_to_string(t->as.ptr.elem));
        return type_buf;
    case TY_FN: {
        int pos = 0;
        pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, "fn(");
        for (int i = 0; i < t->as.fn.param_count; i++) {
            if (i > 0) pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, ", ");
            pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, "%s",
                            type_to_string(t->as.fn.param_types[i]));
        }
        if (t->as.fn.is_variadic)
            pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, ", ...");
        pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, ") -> %s",
                        type_to_string(t->as.fn.ret));
        return type_buf;
    }
    case TY_TUPLE: {
        int pos = 0;
        pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, "(");
        for (int i = 0; i < t->as.tuple.count; i++) {
            if (i > 0) pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, ", ");
            pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, "%s",
                            type_to_string(t->as.tuple.elems[i]));
        }
        pos += snprintf(type_buf + pos, sizeof(type_buf) - pos, ")");
        return type_buf;
    }
    case TY_LIST:
        snprintf(type_buf, sizeof(type_buf), "[%s]", type_to_string(t->as.list.elem));
        return type_buf;
    case TY_TASK:
        snprintf(type_buf, sizeof(type_buf), "Task[%s]", type_to_string(t->as.task.elem));
        return type_buf;
    case TY_MAP:
        snprintf(type_buf, sizeof(type_buf), "{%s: %s}",
                 type_to_string(t->as.map.key), type_to_string(t->as.map.val));
        return type_buf;
    case TY_TYPEVAR:
        snprintf(type_buf, sizeof(type_buf), "?%d", t->as.typevar.id);
        return type_buf;
    case TY_PARAM:
        return t->as.param.name ? t->as.param.name : "?";
    case TY_RECORD:
        return t->as.record.name ? t->as.record.name : "<record>";
    case TY_ENUM:
        return t->as.enumt.name ? t->as.enumt.name : "<enum>";
    case TY_VARIANT: {
        Type *parent = t->as.variant.parent_enum;
        if (parent && parent->kind == TY_ENUM &&
            t->as.variant.variant_index < parent->as.enumt.variant_count) {
            return parent->as.enumt.variants[t->as.variant.variant_index].name;
        }
        return "<variant>";
    }
    default:
        return "<?>";
    }
}
