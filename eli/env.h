#ifndef ENV_H_
#define ENV_H_

#include "value.h"

void env_pop();
void env_push(VALUE);
VALUE env_front();
VALUE env_resolve(VALUE);
void env_bind(VALUE, VALUE);
void env_update(VALUE, VALUE);

#endif