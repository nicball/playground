#ifndef VALUE_H_
#define VALUE_H_

#include <stdlib.h>
#include <stdbool.h>

typedef struct value_s value_t, * VALUE;
typedef struct vtab_s vtab_t, * VTAB;

struct value_s {
    unsigned int refcount;
    short type;
    int hash;
    VTAB vtab;
    VALUE meta;
};

struct vtab_s {
    void (*dtor)(VALUE);
    const char* (*to_string)(VALUE);
    bool (*to_logic)(VALUE);
    bool (*equal)(VALUE, VALUE);
    int (*hash)(VALUE);
};

enum {
    BTYPE_MASK = 0x20 - 1,
    MTYPE_MASK = !BTYPE_MASK
};

enum {
    V_CONS,
    V_SPEC,
    V_CFN,
    V_FN,
    V_MAC,
    V_BOOLEAN,
    V_NUMBER,
    V_STRING,
    V_SYMBOL
};

enum {
    V_ERROR,
    V_TAILCALL
};

#define type(v)        ((v)->type & MTYPE_MASK)
#define iserror(v)     (((v)->type & BTYPE_MASK) == V_ERROR)
#define istailcall(v)  (((v)->type & BTYPE_MASK) == V_TAILCALL)

void _destroy_value(VALUE);

inline VALUE ref(VALUE v) {
    v->refcount++;
    return v;
}

inline void unref(VALUE v) {
    if (--v->refcount == 0)
        _destroy_value(v);
}

inline const char* tostring(VALUE v) {
    return v->vtab->to_string(v);
}

inline bool tologic(VALUE v) {
    return v->vtab->to_logic(v);
}

inline bool equal(VALUE l, VALUE r) {
    return l->vtab->equal(l, r);
}

inline int hash(VALUE v) {
    if (v->hash == -1)
        v->hash = v->vtab->hash(v);
    return v->hash;
}

inline VALUE _make_value(size_t sz, VTAB vtab, short type) {
    VALUE r = (VALUE) malloc(sz);
    r->refcount = 1;
    r->type = type;
    r->hash = -1;
    r->vtab = vtab;
    r->meta = NULL;
    return r;
}

inline VALUE tag_error(VALUE v) {
    v->type |= V_ERROR;
    return v;
}

inline VALUE tag_tailcall(VALUE v) {
    v->type |= V_TAILCALL;
    return v;
}

#endif
