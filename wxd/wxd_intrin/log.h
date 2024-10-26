#include <stdio.h>

#ifndef NDEBUG
#define LOG_DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define LOG_DEBUG(...)
#endif

#ifdef PROFILING
#define LOG_PROFILING(...) fprintf(stderr, __VA_ARGS__)
#else
#define LOG_PROFILING(...)
#endif
