#ifndef LUX_ERROR_H
#define LUX_ERROR_H

#include <stdarg.h>

typedef struct {
    const char *file;
    int line;
    int col;
} SrcLoc;

void error_init(const char *source, const char *filename);
void error_at(SrcLoc loc, const char *fmt, ...);
int error_count(void);

#endif
