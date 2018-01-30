#include "algo.h"

#include "value.h"

VALUE map(VALUE (*func)(VALUE), VALUE seq) {
    VALUE r = make_nil();
    VALUE* p = &r;
    while (!isatom(seq)) {
        VALUE v = func(car(seq));
        if (iserror(v)) {
            unref(r);
            return v;
        }
        car(*p) = v;
        cdr(*p) = make_nil();
        p = &cdr(*p);
        seq = cdr(seq);
    }
    if (!isnil(seq)) {
        VALUE v = func(seq);
        if (iserror(v)) {
            unref(r);
            return v;
        }
        unref(*p);
        *p = v;
    }
    return r;
}

int length(VALUE seq) {
    int i = 0;
    foreach(v, seq, i++;);
    return i;
}
