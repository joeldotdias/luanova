#include "hashtab.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Reference: www.ietf.org/archive/id/draft-eastlake-fnv-21.html */
#define FNV_OFFSET_BASIS 0x811C9DC5
#define FNV_PRIME 0x01000193

/* This implements the core of the FNV-1a (32 bit) hashing algorithm */
static uint32_t hash(const void* val, size_t v_len) {
    if(!val || v_len > SIZE_MAX) {
        FATAL("Null ptr | Weird size");
    }

    uint32_t hash = FNV_OFFSET_BASIS;
    const unsigned char* octets = (unsigned char*)val;

    for(size_t i = 0; i < v_len; i++) {
        hash ^= octets[i];
        hash *= FNV_PRIME;
    }

    return hash;
}

#undef FNV_OFFSET_BASIS
#undef FNV_PRIME

HashTable* init_ht() {
    HashTable* ht = malloc(sizeof(HashTable));
    ht->size = 0;
    ht->capacity = TABLE_SIZE;
    ht->entries = calloc(ht->capacity, sizeof(HashEntry*));

    return ht;
}

#define HT_NOT_FOUND ((size_t)-1)

static size_t find_slot(HashTable* ht, const void* key, size_t key_len) {
    uint32_t computed_hash = hash(key, key_len);
    size_t idx = computed_hash % ht->capacity;
    size_t original = idx;
    /* INFO("Computed hash 0x%08X | idx %zu", computed_hash, idx); */

    do {
        // to insert a new value
        if(!ht->entries[idx]) {
            return idx;
        }

        // to update an existing value
        if(ht->entries[idx]->occupied) {
            return idx;
        }
        idx = (idx + 1) % ht->capacity;
    } while(idx != original);

    return HT_NOT_FOUND;
};

bool ht_insert(HashTable* ht, const void* key, size_t key_len, const void* value) {
    if(ht->size >= ht->capacity * 0.85) {
        return false;
    }

    size_t slot = find_slot(ht, key, key_len);
    if(slot == HT_NOT_FOUND) {
        return false;
    }

    if(!ht->entries[slot]) {
        ht->entries[slot] = malloc(sizeof(HashEntry));
        ht->entries[slot]->occupied = false;
        ht->entries[slot]->key = malloc(key_len);
        memcpy(ht->entries[slot]->key, key, key_len);
        ht->size++;
    }

    HashEntry* entry = ht->entries[slot];
    entry->value = (void*)value;
    entry->occupied = true;

    return true;
}

void* ht_get(HashTable* ht, const void* key, size_t key_len) {
    size_t slot = find_slot(ht, key, key_len);
    if(slot == HT_NOT_FOUND) {
        FATAL("Didn't find key");
    }

    HashEntry* target = ht->entries[slot];
    if(!target) {
        return NULL;
    }

    return target->occupied ? target->value : NULL;
}

void ht_delete(HashTable* ht, const void* key, size_t key_len) {
    size_t idx = find_slot(ht, key, key_len);
    if(idx != HT_NOT_FOUND && ht->entries[idx]->occupied) {
        ht->entries[idx]->occupied = false;
        ht->size--;
    }
}

#undef HT_NOT_FOUND

void ht_free(HashTable* table) {
    if(!table) {
        return;
    }

    for(size_t i = 0; i < table->capacity; i++) {
        if(table->entries[i]->occupied) {
            free(table->entries[i]->value);
        }
    }

    free(table->entries);
    free(table);
}
