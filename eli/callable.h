#ifndef CALLABLE_H_
#define CALLABLE_H_

#include "value.h"

typedef struct callable_s callable_t, * CALLABLE;
typedef struct callable_vtab_s callable_vtab_t, * CALLABLE_VTAB;
typedef VALUE (*apply_func_t)(VALUE, VALUE);

struct callable_s {
    value_t value;
    int arity;
    bool variadic;
};

struct callable_vtab_s {
    vtab_t vtab;
    apply_func_t apply;
};

inline bool iscallable(VALUE c) {
    return type(c) == V_CFN
        || type(c) == V_SPEC
        || type(c) == V_FN
        || type(c) == V_MAC;
}

#define getarity(c)     (((CALLABLE)(c))->arity)
#define getvariadic(c)  (((CALLABLE)(c))->variadic)

inline VALUE apply(VALUE c, VALUE args) {
    return ((CALLABLE_VTAB)c->vtab)->apply(c, args);
}

inline CALLABLE _make_callable(size_t sz, CALLABLE_VTAB vtab, short type, int arity, bool variadic) {
    CALLABLE c = (CALLABLE) _make_value(sz, (VTAB)vtab, type);
    c->arity = arity;
    c->variadic = variadic;
    return c;
}

#endif
