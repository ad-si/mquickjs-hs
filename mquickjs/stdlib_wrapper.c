/* Wrapper to include the generated stdlib definition with stub functions */
#include <stdio.h>
#include <string.h>
#include "mquickjs.h"

/* Stub implementations for required functions */
static JSValue js_date_now(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    return JS_NewFloat64(ctx, 0.0); /* TODO: implement with real time */
}

static JSValue js_print(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    int i;
    JSCStringBuf buf;
    for (i = 0; i < argc; i++) {
        if (i > 0) printf(" ");
        const char *str = JS_ToCString(ctx, argv[i], &buf);
        if (str) printf("%s", str);
    }
    printf("\n");
    return JS_UNDEFINED;
}

static JSValue js_performance_now(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    return JS_NewFloat64(ctx, 0.0); /* TODO: implement with real time */
}

static JSValue js_gc(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    JS_GC(ctx);
    return JS_UNDEFINED;
}

static JSValue js_load(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    return JS_ThrowTypeError(ctx, "load() not implemented");
}

static JSValue js_setTimeout(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    return JS_ThrowTypeError(ctx, "setTimeout() not implemented");
}

static JSValue js_clearTimeout(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv) {
    return JS_ThrowTypeError(ctx, "clearTimeout() not implemented");
}

#include "mqjs_stdlib.h"
