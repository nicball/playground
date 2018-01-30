#ifndef NUMBER_H_
#define NUMBER_H_

#include "value.h"

typedef struct number_s number_t, * NUMBER;

struct number_s {
    value_t value;
    double number;
};

#define isnumber(c)    (type(c) == V_NUMBER)
#define getnumber(c)   (((NUMBER)(c))->number)

VALUE make_number(double);

#endif
