/*
 * Lux Coroutine Runtime
 *
 * Cooperative coroutines using POSIX ucontext.
 * - lux_coro_spawn(thunk, data) creates a coroutine that calls thunk(data)
 * - lux_coro_await(task) runs the coroutine to completion
 * - lux_coro_destroy(task) frees the coroutine resources
 *
 * The thunk is responsible for reading arguments from `data` and
 * writing the result back into `data` (at offset 0).
 */

#define _XOPEN_SOURCE 600  /* for ucontext on glibc */
#include <ucontext.h>
#include <stdlib.h>
#include <stdint.h>

#define LUX_CORO_STACK_SIZE (64 * 1024)  /* 64 KB per coroutine */

typedef struct LuxTask {
    ucontext_t ctx;
    void *stack;
    int completed;
    void (*thunk)(void *);
    void *data;
} LuxTask;

/* Main context to return to when coroutine finishes or yields */
static ucontext_t lux_main_ctx;

/*
 * Coroutine entry point.
 * ucontext's makecontext only passes ints, so we split the
 * LuxTask pointer into two 32-bit halves.
 */
static void lux_coro_entry(unsigned int hi, unsigned int lo) {
    uintptr_t addr = ((uintptr_t)hi << 32) | (uintptr_t)lo;
    LuxTask *task = (LuxTask *)addr;
    task->thunk(task->data);
    task->completed = 1;
    /* Return to whoever called swapcontext into us */
    setcontext(&lux_main_ctx);
}

LuxTask *lux_coro_spawn(void (*thunk)(void *), void *data) {
    LuxTask *task = (LuxTask *)calloc(1, sizeof(LuxTask));
    task->stack = malloc(LUX_CORO_STACK_SIZE);
    task->thunk = thunk;
    task->data = data;
    task->completed = 0;

    getcontext(&task->ctx);
    task->ctx.uc_stack.ss_sp = task->stack;
    task->ctx.uc_stack.ss_size = LUX_CORO_STACK_SIZE;
    task->ctx.uc_link = &lux_main_ctx;

    unsigned int hi = (unsigned int)((uintptr_t)task >> 32);
    unsigned int lo = (unsigned int)((uintptr_t)task & 0xFFFFFFFFu);
    makecontext(&task->ctx, (void (*)(void))lux_coro_entry, 2, hi, lo);

    return task;
}

void lux_coro_await(LuxTask *task) {
    if (!task->completed) {
        swapcontext(&lux_main_ctx, &task->ctx);
    }
}

void lux_coro_destroy(LuxTask *task) {
    if (task) {
        free(task->stack);
        free(task);
    }
}
