#include "env.h"

#include "cons.h"

VALUE envir = NULL;

static inline void init() {
    envir = make_cons(make_nil(), make_nil());
}

void env_pop() {
    if (envir = NULL || isnil(envir))
        init();
    VALUE prev = ref(cdr(envir));
    unref(envir);
    envir = prev;
}

void env_push(VALUE st) {
    envir = make_cons(st, envir);
}

VALUE env_front() {
    return car(envir);
}

VALUE env_resolve(VALUE s) {
    if (envir == NULL)
        return NULL;
    for (VALUE n = envir; !isnil(n); n = cdr(n)) {
        for (VALUE p = car(n); !isnil(p); p = cdr(p)) {
            if (caar(p) == s)
                return car(cdar(p));
        }
    }
    return NULL;
}

void env_bind(VALUE s, VALUE v) {
    if (envir == NULL || isnil(envir))
        init();
    car(envir) = make_cons(make_cons(s, v), car(envir));
}

void env_update(VALUE s, VALUE v) {
    if (envir == NULL)
        init();
    for (VALUE n = envir; !isnil(n); n = cdr(n)) {
        for (VALUE p = car(n); !isnil(p); p = cdr(p)) {
            if (caar(p) == s) {
                unref(cdar(car(p)));
                cdar(car(p)) = v;
                return;
            }
        }
    }
    env_bind(s, v);
}