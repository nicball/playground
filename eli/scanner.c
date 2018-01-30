#include "scanner.h"

#include <stdlib.h>

SCANNER make_scanner(FILE* input) {
    SCANNER s = (SCANNER) malloc(sizeof(scanner_t));
    s->input = input;
    scanner_get(s);
    return s;
}

void destroy_scanner(SCANNER s) {
    free(s);
}

