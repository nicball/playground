#ifndef UTIL_QUEUE_H_
#define UTIL_QUEUE_H_

#include "value.h"

typedef struct queue_s queue_t, * QUEUE;
typedef struct queue_node_s queue_node_t, * QUEUE_NODE;

struct queue_s {
    QUEUE_NODE head;
    QUEUE_NODE tail;
};

QUEUE make_queue();
void destroy_queue(QUEUE);
void queue_push(QUEUE, VALUE);
VALUE queue_pop(QUEUE);

inline bool queue_empty(QUEUE q) {
    return q->head == NULL && q->tail == NULL;
}

#endif
