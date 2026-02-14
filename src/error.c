#include "error.h"
#include <stdio.h>
#include <string.h>

static const char *g_source;
static const char *g_filename;
static int g_errors;

void error_init(const char *source, const char *filename) {
    g_source = source;
    g_filename = filename;
    g_errors = 0;
}

int error_count(void) {
    return g_errors;
}

/* Find the start of the line containing offset */
static const char *line_start(const char *src, int line) {
    const char *p = src;
    int cur = 1;
    while (*p && cur < line) {
        if (*p == '\n') cur++;
        p++;
    }
    return p;
}

void error_at(SrcLoc loc, const char *fmt, ...) {
    g_errors++;

    fprintf(stderr, "%s:%d:%d: error: ", g_filename, loc.line, loc.col);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    if (g_source && loc.line > 0) {
        const char *ls = line_start(g_source, loc.line);
        const char *le = ls;
        while (*le && *le != '\n') le++;

        fprintf(stderr, " %4d | %.*s\n", loc.line, (int)(le - ls), ls);
        fprintf(stderr, "      | ");
        for (int i = 1; i < loc.col; i++) fprintf(stderr, " ");
        fprintf(stderr, "^\n");
    }
}
