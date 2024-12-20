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
    char key[MAX_STRING_LENGTH];
    void* value;
    bool occupied;
} HashEntry;

typedef struct {
    HashEntry* entries;
    size_t size;
    size_t capacity;
} HashTable;

HashTable* init_ht();
void* ht_get(HashTable* ht, const char* key);
bool ht_insert(HashTable* ht, const char* key, const void* value);
void ht_delete(HashTable* ht, const char* key);
void ht_free(HashTable* ht);

#endif
