#include "util/string.h"

#include <string.h>

char* str_dup(const char* src) {
    return str_ndup(src, strlen(src));
}

char* str_ndup(const char* src, size_t n) {
    char* r = (char*) malloc(n+1);
    strncpy(r, src, n);
    r[n] = '\0';
    return r;
}

enum {
    INIT_CAP = 1024
};

STRBUILDER make_strbuilder() {
    STRBUILDER s = (STRBUILDER) malloc(sizeof(strbuilder_t));
    s->len = 0;
    s->cap = INIT_CAP;
    s->str = (char*) malloc(INIT_CAP);
    s->str[0] = '\0';
    return s;
}

void destroy_strbuilder(STRBUILDER s) {
    free(s->str);
    free(s);
}

void strbuilder_append(STRBUILDER s, const char* s) {
    int sz = strlen(s);
    if (sz + 1 > (s->cap - s->len))
        s->str = (char*) realloc(s->str, s->cap *= 2);
    strcat(s->str + s->len, s);
    s->len += sz;
}

void strbuilder_drop(STRBUILDER s, int n) {
    s->str[s->len - n] = '\0';
}
