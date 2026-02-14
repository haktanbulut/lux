#ifndef LUX_TYPE_H
#define LUX_TYPE_H

#include "arena.h"
#include <stdbool.h>

typedef struct Type Type;

typedef enum {
    TY_INT,
    TY_FLOAT,
    TY_BOOL,
    TY_CHAR,
    TY_STRING,
    TY_NONE,
    TY_FN,
    TY_TUPLE,
    TY_RECORD,
    TY_ENUM,
    TY_VARIANT,
    TY_LIST,
    TY_MAP,
    TY_TYPEVAR,
    TY_PARAM,
    TY_ERROR,
    TY_NEVER,
    TY_PTR,
    TY_TASK,
} TypeKind;

struct Type {
    TypeKind kind;
    union {
        /* TY_FN */
        struct {
            Type **param_types;
            int param_count;
            Type *ret;
            bool is_variadic;
        } fn;

        /* TY_TUPLE */
        struct {
            Type **elems;
            int count;
        } tuple;

        /* TY_TYPEVAR — union-find */
        struct {
            int id;
            Type *resolved;
        } typevar;

        /* TY_PARAM — named type parameter */
        struct {
            const char *name;
        } param;

        /* TY_PTR — pointer to element type */
        struct {
            Type *elem;
        } ptr;

        /* TY_LIST — list of element type */
        struct {
            Type *elem;
        } list;

        /* TY_TASK — coroutine task returning elem type */
        struct {
            Type *elem;
        } task;

        /* TY_MAP — map from key type to value type */
        struct {
            Type *key;
            Type *val;
        } map;

        /* TY_RECORD */
        struct {
            const char *name;           /* interned type name */
            const char **field_names;   /* interned field names */
            Type **field_types;
            int field_count;
        } record;

        /* TY_ENUM */
        struct {
            const char *name;
            struct VariantInfo *variants;
            int variant_count;
        } enumt;

        /* TY_VARIANT — reference to parent enum + variant index */
        struct {
            Type *parent_enum;
            int variant_index;
        } variant;
    } as;
};

/* Variant info for enum types */
typedef struct VariantInfo {
    const char *name;           /* interned */
    Type **payload_types;
    const char **payload_names; /* NULL for positional, non-NULL for record variants */
    int payload_count;
    bool is_record_variant;
} VariantInfo;

/* Singleton primitive types */
Type *type_int(void);
Type *type_float(void);
Type *type_bool(void);
Type *type_char(void);
Type *type_string(void);
Type *type_none(void);
Type *type_error(void);
Type *type_never(void);

/* Constructors (arena-allocated) */
Type *type_fn(Arena *a, Type **params, int param_count, Type *ret, bool is_variadic);
Type *type_tuple(Arena *a, Type **elems, int count);
Type *type_typevar(Arena *a, int id);
Type *type_param(Arena *a, const char *name);
Type *type_ptr(Arena *a, Type *elem);
Type *type_list(Arena *a, Type *elem);
Type *type_map(Arena *a, Type *key, Type *val);
Type *type_record(Arena *a, const char *name, const char **field_names, Type **field_types, int count);
Type *type_enum(Arena *a, const char *name, VariantInfo *variants, int variant_count);
Type *type_variant(Arena *a, Type *parent_enum, int variant_index);
Type *type_task(Arena *a, Type *elem);

/* Follow union-find chain */
Type *type_resolve(Type *t);

/* Structural equality (resolves type vars) */
bool type_equals(Type *a, Type *b);

/* For error messages */
const char *type_to_string(Type *t);

/* Is numeric (int or float) */
bool type_is_numeric(Type *t);

#endif
