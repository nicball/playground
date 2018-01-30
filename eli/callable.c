#include "callable.h"

bool iscallable(VALUE c) {
    return type(c) == V_CFN
        || type(c) == V_SPEC
        || type(c) == V_FN
        || type(c) == V_MAC;
}

VALUE apply(VALUE c, VALUE args) {
    return ((CALLABLE_VTAB)c->vtab)->apply(c, args);
}

CALLABLE _make_callable(size_t sz, CALLABLE_VTAB vtab, short type, int arity, bool variadic) {
    CALLABLE c = (CALLABLE) _make_value(sz, (VTAB)vtab, type);
    c->arity = arity;
    c->variadic = variadic;
    return c;
}
