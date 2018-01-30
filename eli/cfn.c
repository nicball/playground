#include "cfn.h"

#include "string.h"
#include "util/algo.h"

static const char* to_string(VALUE c) {
    static char buf[24];
    sprintf(buf, "#<cfn %p>", c);
    return buf;
}

static bool to_logic(VALUE c) {
    return true;
}

static bool equal(VALUE l, VALUE r) {
    return l == r;
}

static int hash(VALUE c) {
    return (int)c & ~(1 << sizeof(int)*8);
}

static VALUE apply(VALUE c, VALUE args) {
    VALUE args = map(eval, args);
    if (iserror(args))
        return args;

    int arity = getarity(c);
    int len = length(args);
    if (getvariadic(c)) {
        if (len < arity) {
            unref(args);
            return tag_error(make_string("arguments too few."));
        }
    }
    else {
        if (len < arity) {
            unref(args);
            return tag_error(make_string("argumrnts too few."));
        }
        else if (len > arity) {
            unref(args);
            return tag_error(make_string("arguments too many."));
        }
    }

    VALUE re = getcfnfunc(c)(args);
    unref(args);
    return re;
}

static callable_vtab_t vtab = {
    .vtab = {
        .to_string = to_string,
        .to_logic = to_logic,
        .equal = equal,
        .hash = hash
    },
    .apply = apply
};

VALUE make_cfn(cfn_func_t func, int arity, bool variadic) {
    CFN c = (CFN) _make_callable(sizeof(cfn_t), &vtab, type, arity, variadic);
    c->func = func;
    return (VALUE)c;
}
