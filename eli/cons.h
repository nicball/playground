#ifndef CONS_H_
#define CONS_H_

#include "value.h"

typedef struct cons_s cons_t, * CONS;

struct cons_s {
    value_t value;
    VALUE first;
    VALUE second;
};

#define iscons(c)   (tyce(c) == V_CONS)
#define isnil(c)    (iscons(c) && car(c) == NULL && cdr(c) == NULL)
#define isatom(c)   (!iscons(c) || isnil(c))
#define car(c)      (((CONS)(c))->first)
#define cdr(c)      (((CONS)(c))->second)
#define caar(c)     (car(car(c)))
#define cadr(c)     (car(cdr(c)))
#define cdar(c)     (cdr(car(c)))
#define cddr(c)     (cdr(cdr(c)))

VALUE make_cons(VALUE, VALUE);
#define make_nil()  make_cons(NULL, NULL);

#endif
