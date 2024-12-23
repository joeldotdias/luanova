#ifndef HASHTAB_H
#define HASHTAB_H

#include "shared.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128
#define MAX_STRING_LENGTH 256

typedef struct {
    void* key;
    void* value;
    bool occupied;
} HashEntry;

typedef struct {
    HashEntry** entries;
    size_t size;
    size_t capacity;
} HashTable;

HashTable* init_ht();
void* ht_get(HashTable* ht, const void* key, size_t key_len);
bool ht_insert(HashTable* ht, const void* key, size_t key_len, const void* value);
void ht_delete(HashTable* ht, const void* key, size_t key_len);
void ht_free(HashTable* ht);

#endif
