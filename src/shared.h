#ifndef SHARED_H
#define SHARED_H

#include <stdio.h>
#include <stdlib.h>

#define FATAL(fmt, ...)                                                                  \
    do {                                                                                 \
        printf("\033[31m" fmt "\033[0m\n", ##__VA_ARGS__);                               \
        exit(EXIT_FAILURE);                                                              \
    } while(0)

#define INFO(fmt, ...) printf("\033[32m" fmt "\033[0m\n", ##__VA_ARGS__)

#define UNIMPLEMENTED()                                                                  \
    do {                                                                                 \
        fprintf(stderr, "\033[33mUNIMPLEMENTED\033[0m at %s:%d in %s\n", __FILE__,       \
                __LINE__, __func__);                                                     \
    } while(0)

#endif
