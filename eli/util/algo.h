#ifndef UTIL_ALGO_H_
#define UTIL_ALGO_H_

#include "cons.h"

#define foreach(v, list, block) \
    do { \
        VALUE __foreach_iter = (list) \
        for (; !isatom(__foreach_iter); __foreach_iter = cdr(__foreach_iter)) { \
            VALUE v = car(__foreach_iter); \
            block \
        } \
        if (!isnil(__foreach_iter)) { \
            VALUE v = __foreach_iter; \
            block \
        } \
    } while (false)

VALUE map(VALUE (*)(VALUE), VALUE);

inline int length(VALUE seq) {
    int i = 0;
    foreach(v, seq, i++;);
    return i;
}

#endif
