#include "value.h"

#include "util/queue.h"

void _destroy_value(VALUE v) {
    static bool in_iter = false;
    static QUEUE q = NULL;

    if (in_iter) {
        queue_push(q, v);
        return;
    }
    else {
        q = make_queue();
        in_iter = true;
        queue_push(q, v);
        while (!queue_empty(q)) {
            VALUE p = queue_pop(q);
            if (p->meta != NULL)
                unref(p->meta);
            if (p->vtab->dtor != NULL)
                p->vtab->dtor(v);
            free(p);
        }
        in_iter = false;
        destroy_queue(q);
    }
}

VALUE ref(VALUE v) {
    v->refcount++;
    return v;
}

void unref(VALUE v) {
    if (--v->refcount == 0)
        _destroy_value(v);
}

const char* tostring(VALUE v) {
    return v->vtab->to_string(v);
}

bool tologic(VALUE v) {
    return v->vtab->to_logic(v);
}

bool equal(VALUE l, VALUE r) {
    return l->vtab->equal(l, r);
}

int hash(VALUE v) {
    if (v->hash == -1)
        v->hash = v->vtab->hash(v);
    return v->hash;
}

VALUE _make_value(size_t sz, VTAB vtab, short type) {
    VALUE r = (VALUE) malloc(sz);
    r->refcount = 1;
    r->type = type;
    r->hash = -1;
    r->vtab = vtab;
    r->meta = NULL;
    return r;
}

VALUE tag_error(VALUE v) {
    v->type |= V_ERROR;
    return v;
}

VALUE tag_tailcall(VALUE v) {
    v->type |= V_TAILCALL;
    return v;
}
