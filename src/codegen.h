#ifndef LUX_CODEGEN_H
#define LUX_CODEGEN_H

#include "ast.h"
#include "type.h"
#include "scope.h"
#include "str.h"
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/TargetMachine.h>

/* Map from Symbol* to LLVMValueRef */
typedef struct ValueEntry {
    const char *name;        /* interned name — pointer equality */
    LLVMValueRef value;
    struct ValueEntry *next;
} ValueEntry;

#define VALUEMAP_BUCKETS 64

typedef struct ValueScope {
    ValueEntry *buckets[VALUEMAP_BUCKETS];
    struct ValueScope *parent;
} ValueScope;

/* Loop context for break/continue */
typedef struct LoopCtx {
    LLVMBasicBlockRef cond_bb;   /* for continue (NULL for loop) */
    LLVMBasicBlockRef exit_bb;   /* for break */
    struct LoopCtx *parent;
} LoopCtx;

/* Defer stack entry */
typedef struct DeferEntry {
    AstNode *body;
    struct DeferEntry *next;
} DeferEntry;

typedef struct {
    LLVMContextRef ctx;
    LLVMModuleRef module;
    LLVMBuilderRef builder;
    LLVMTargetMachineRef target_machine;

    ValueScope *values;
    LoopCtx *loop_stack;
    Arena *arena;
    InternTable *interns;

    /* Current function being generated */
    LLVMValueRef current_fn;
    Type *current_ret_type;

    /* Symbol table from checker (for type lookups) */
    SymTab symtab;

    /* Defer stack for current scope */
    DeferEntry *defer_stack;

    /* Lambda counter for unique names */
    int lambda_counter;

    /* Spawn thunk counter for unique names */
    int spawn_counter;

    /* Generic function AST nodes for monomorphization */
    AstNode **generic_fns;
    int generic_fn_count;

    /* Type substitution map for monomorphization (active during generic codegen) */
    struct CgTypeSubst { const char *name; Type *concrete; } *type_substs;
    int type_subst_count;

    /* Track already-monomorphized functions to avoid duplicates */
    struct MonoCacheEntry { const char *mangled_name; LLVMValueRef fn; } *mono_cache;
    int mono_cache_count;
    int mono_cache_cap;
} Codegen;

void codegen_init(Codegen *cg, const char *module_name, Arena *arena, InternTable *interns, SymTab *checker_symtab);
void codegen_program(Codegen *cg, AstNode *program);
void codegen_emit_ir(Codegen *cg, const char *filename);
int  codegen_emit_obj(Codegen *cg, const char *filename);
void codegen_destroy(Codegen *cg);

#endif
