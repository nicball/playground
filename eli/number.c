#include "number.h"

static const char* to_string(VALUE v) {
    static char buf[350];
    sprintf(buf, "%g", getnumber(v));
    return buf;
}

static bool to_logic(VALUE v) {
    return true;
}

static bool equal(VALUE l, VALUE r) {
    if (!isnumber(r))
        return false;
    else
        return getnumber(l) == getnumber(r);
}

static int hash(VALUE n) {
    return *(int*)&getnumber(n) & ~(1 << sizeof(int)*8);
}

static vtab_t vtab = {
    .to_string = to_string,
    .to_logic = to_logic,
    .equal = equal,
    .hash = hash
};

VALUE make_number(double d) {
    NUMBER n = (NUMBER) _make_value(sizeof(number_t), &vtab, V_NUMBER);
    n->number = d;
    return (VALUE)n;
}
