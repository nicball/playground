#ifndef CFN_H_
#define CFN_H_

#include "callable.h"

typedef struct cfn_s cfn_t, * CFN;
typedef VALUE (*cfn_func_t)(VALUE);

struct cfn_s {
    callable_t callable;
    cfn_func_t func;
};

#define iscfn(c)            (type(c) == V_CFN)
#define getcfnfunc(c)       (((CFN)(c))->func)

VALUE make_cfn(cfn_func_t, int, bool);

#endif
