#include "util/queue.h"

#include <stdlib.h>

struct queue_node_s {
    VALUE v;
    QUEUE_NODE prev;
    QUEUE_NODE next;
};

static inline QUEUE_NODE make_queue_node(VALUE v, QUEUE_NODE prev, QUEUE_NODE next) {
    QUEUE_NODE n = (QUEUE_NODE) malloc(sizeof(queue_node_t));
    n->v = v;
    n->prev = prev;
    n->next = next;
    return n;
}

static inline void destroy_queue_node(QUEUE_NODE n) {
    free(n);
}

QUEUE make_queue() {
    QUEUE q = (QUEUE) malloc(sizeof(queue_t));
    q->head = q->tail = NULL;
    return q;
}

void destroy_queue(QUEUE q) {
    for (QUEUE_NODE* n = &q->head; *n != NULL; ) {
        QUEUE_NODE cur = *n;
        *n = (*n)->next;
        destroy_queue_node(cur);
    }
    free(q);
}

void queue_push(QUEUE q, VALUE v) {
    if (queue_empty(q)) {
        q->head = q->tail = make_queue_node(v, NULL, NULL);
    }
    else {
        QUEUE_NODE n = make_queue_node(v, tail, NULL);
        q->tail->next = n;
        q->tail = n;
    }
}

VALUE queue_pop(QUEUE q) {
    if (!queue_empty(q)) {
        VALUE r = q->head->v;
        QUEUE_NODE n = q->head->next;
        destroy_queue_node(q->head);
        if (n != NULL) {
            n->prev = NULL;
            q->head = n;
        }
        else {
            q->head = q->tail = NULL;
        }
        return r;
    }
    else {
        return NULL;
    }
}

bool queue_empty(QUEUE q) {
    return q->head == NULL && q->tail == NULL;
}
