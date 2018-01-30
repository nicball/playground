#include "cons.h"

#include "util/string.h"
#include <stdlib.h>

static void dtor(VALUE c) {
    unref(car(c));
    unref(cdr(c));
}

static const char* to_string(VALUE c) {
    static char* str = NULL;

    free(str);
    STRBUILDER buf = make_strbuilder();

    strbuilder_append(buf, "(");
    VALUE p;
    for (p = c; !isatom(p); p = cdr(p)) {
        strbuilder_append(buf, tostring(car(p)));
        strbuilder_append(buf, " ");
    }
    if (!isnil(p)) {
        strbuilder_append(buf, ". ");
        strbuilder_append(buf, tostring(p));
    }
    else {
        strbuilder_drop(buf, 1);
    }
    strbuilder_append(buf, ")");

    str = str_dup(getstrbuilderstr(buf));
    destroy_strbuilder(buf);
    return str;
}

static bool to_logic(VALUE c) {
    return !isnil(c);
}

static bool equal(VALUE c, VALUE r) {
    if (isnil(c))
        return isnil(r);
    else if (isnil(r))
        return false;
    else
        return equal(car(c), car(r)) && equal(cdr(c), cdr(r));
}

static int hash(VALUE c) {
    return (int)c & ~(1 << sizeof(int)*8);
}

static vtab_t vtab = {
    .dtor = dtor,
    .to_string = to_string,
    .to_ligic = to_logic,
    .equal = equal,
    .hash = hash
};

VALUE make_cons(VALUE a, VALUE b) {
    CONS c = (CONS) _make_value(sizeof(cons_t), &vtab, V_CONS));
    c->first = a;
    c->second = b;
    return (VALUE)c;
}
