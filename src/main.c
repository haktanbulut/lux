#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "arena.h"
#include "vec.h"
#include "str.h"
#include "error.h"
#include "token.h"
#include "lexer.h"
#include "ast.h"
#include "parser.h"
#include "checker.h"
#include "codegen.h"

typedef enum {
    MODE_COMPILE,
    MODE_TOKENS,
    MODE_AST,
    MODE_CHECK,
    MODE_EMIT_LLVM,
} Mode;

static char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "error: could not open '%s'\n", path);
        return NULL;
    }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)malloc(size + 1);
    if ((long)fread(buf, 1, size, f) != size) {
        fprintf(stderr, "error: could not read '%s'\n", path);
        free(buf);
        fclose(f);
        return NULL;
    }
    buf[size] = '\0';
    fclose(f);
    return buf;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: lux <file.lux> [--tokens|--ast|--check|--emit-llvm] [-o output]\n");
        return 1;
    }

    const char *filename = argv[1];
    const char *output_name = NULL;
    Mode mode = MODE_COMPILE;

    /* Parse CLI flags */
    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "--tokens") == 0) {
            mode = MODE_TOKENS;
        } else if (strcmp(argv[i], "--ast") == 0) {
            mode = MODE_AST;
        } else if (strcmp(argv[i], "--check") == 0) {
            mode = MODE_CHECK;
        } else if (strcmp(argv[i], "--emit-llvm") == 0) {
            mode = MODE_EMIT_LLVM;
        } else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_name = argv[++i];
        }
    }

    char *source = read_file(filename);
    if (!source) return 1;

    /* Initialize subsystems */
    InternTable interns;
    intern_init(&interns);
    error_init(source, filename);

    /* Lex */
    Lexer lex;
    lexer_init(&lex, source, filename, &interns);
    Token *tokens = lexer_lex_all(&lex);
    int token_count = (int)vec_len(tokens);

    /* --tokens: print token stream and exit */
    if (mode == MODE_TOKENS) {
        for (int i = 0; i < token_count; i++) {
            Token *t = &tokens[i];
            printf("%3d:%-3d %-15s '%.*s'\n",
                   t->loc.line, t->loc.col,
                   token_kind_str(t->kind),
                   t->len, t->start);
        }
        lexer_free(&lex);
        vec_free(tokens);
        intern_free(&interns);
        free(source);
        return 0;
    }

    /* Parse */
    Arena arena;
    arena_init(&arena);

    Parser parser;
    parser_init(&parser, tokens, token_count, &arena, &interns);
    AstNode *program = parse_program(&parser);

    int errs = error_count() + parser.errors;
    if (errs > 0) {
        fprintf(stderr, "\n%d error(s) found.\n", errs);
        arena_destroy(&arena);
        lexer_free(&lex);
        vec_free(tokens);
        intern_free(&interns);
        free(source);
        return 1;
    }

    /* --ast: print AST and exit */
    if (mode == MODE_AST) {
        ast_print(program, 0);
        arena_destroy(&arena);
        lexer_free(&lex);
        vec_free(tokens);
        intern_free(&interns);
        free(source);
        return 0;
    }

    /* Typecheck */
    Checker checker;
    checker_init(&checker, &arena, &interns);
    checker_check(&checker, program);

    errs = checker.errors;
    if (errs > 0) {
        fprintf(stderr, "\n%d type error(s) found.\n", errs);
        arena_destroy(&arena);
        lexer_free(&lex);
        vec_free(tokens);
        intern_free(&interns);
        free(source);
        return 1;
    }

    /* --check: typecheck only, exit */
    if (mode == MODE_CHECK) {
        printf("OK: no type errors.\n");
        arena_destroy(&arena);
        lexer_free(&lex);
        vec_free(tokens);
        intern_free(&interns);
        free(source);
        return 0;
    }

    /* Codegen */
    Codegen cg;
    codegen_init(&cg, filename, &arena, &interns, &checker.symtab);
    codegen_program(&cg, program);

    int result = 0;

    if (mode == MODE_EMIT_LLVM) {
        codegen_emit_ir(&cg, NULL);
    } else {
        /* Compile to executable */
        const char *obj_file = "/tmp/lux_output.o";
        if (codegen_emit_obj(&cg, obj_file) != 0) {
            result = 1;
        } else {
            /* Link with cc (include coroutine runtime) */
            const char *out = output_name ? output_name : "output";
            const char *runtime_obj = "/tmp/lux_runtime.o";
            char link_cmd[512];
            snprintf(link_cmd, sizeof(link_cmd), "cc %s %s -o %s", obj_file, runtime_obj, out);
            int link_result = system(link_cmd);
            if (link_result != 0) {
                fprintf(stderr, "error: linking failed\n");
                result = 1;
            }
        }
    }

    /* Cleanup */
    codegen_destroy(&cg);
    arena_destroy(&arena);
    lexer_free(&lex);
    vec_free(tokens);
    intern_free(&interns);
    free(source);

    return result;
}
