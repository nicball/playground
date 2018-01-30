#ifndef SCANNER_H_
#define SCANNER_H_

#include <stdio.h>

typedef struct scanner_s scanner_t, * SCANNER;

struct scanner_s {
    FILE* input;
    int token;
    union {
        double number;
        char* string;
        char* symbol;
    };
};

enum {
    T_NUMBER = 257,
    T_STRING,
    T_SYMBOL,
    T_EOF
}

SCANNER make_scanner(FILE*);
void destroy_scanner(SCANNER);
int scanner_get(SCANNER);

#endif
