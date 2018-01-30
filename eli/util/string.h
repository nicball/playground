#ifndef UTIL_STRING_H_
#define UTIL_STRING_H_

#include <stdlib.h>

char* str_dup(const char*);
char* str_ndup(const char*, size_t);

typedef struct strbuilder_s strbuilder_t, * STRBUILDER;

struct strbuilder_s {
    int len;
    int cap;
    char* str;
};

#define getstrbuilderlen(s) ((s)->len)
#define getstrbuilderstr(s) ((s)->str)

STRBUILDER make_strbuilder();
void destroy_strbuilder(STRBUILDER);
void strbuilder_append(STRBUILDER, const char*);
void strbuilder_drop(STRBUILDER, int);

#endif
