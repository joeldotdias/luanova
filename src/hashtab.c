#include "hashtab.h"

static unsigned long hash(const char* str) {
    unsigned long hash = 5381;
    int c;
    while((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash;
}

HashTable* init_ht() {
    HashTable* ht = (HashTable*)malloc(sizeof(HashTable));
    ht->size = 0;
    ht->capacity = TABLE_SIZE;
    ht->entries = (HashEntry*)calloc(ht->capacity, sizeof(HashEntry));

    return ht;
}

#define HT_NOT_FOUND ((size_t)-1)

static size_t find_slot(HashTable* ht, const char* key) {
    size_t idx = hash(key) % ht->capacity;
    size_t original = idx;

    do {
        if(!ht->entries[idx].occupied) {
            return idx;
        }
        if(strcmp(ht->entries[idx].key, key) == 0) {
            return idx;
        }
        idx = (idx + 1) % ht->capacity;
    } while(idx != original);

    return HT_NOT_FOUND;
}

bool ht_insert(HashTable* ht, const char* key, const void* value) {
    if(ht->size >= ht->capacity * 0.85) {
        return false;
    }

    size_t slot = find_slot(ht, key);
    if(slot == HT_NOT_FOUND) {
        return false;
    }

    HashEntry* entry = &ht->entries[slot];

    if(!entry->occupied) {
        ht->size++;
    }

    strncpy(entry->key, key, MAX_STRING_LENGTH - 1);
    entry->key[MAX_STRING_LENGTH - 1] = '\0';
    entry->value = (void*)value;
    entry->occupied = true;
    return true;
}

void ht_delete(HashTable* ht, const char* key) {
    size_t idx = find_slot(ht, key);
    if(idx != HT_NOT_FOUND && ht->entries[idx].occupied) {
        ht->entries[idx].occupied = false;
        ht->size--;
    }
}

void* ht_get(HashTable* ht, const char* key) {
    size_t slot = find_slot(ht, key);
    if(slot == HT_NOT_FOUND) {
        return NULL;
    }

    HashEntry* target = &ht->entries[slot];

    if(target->occupied && strcmp(target->key, key) == 0) {
        return target->value;
    }

    return NULL;
}

#undef HT_NOT_FOUND

void ht_free(HashTable* ht) {
    if(!ht) {
        return;
    }

    free(ht->entries);
    free(ht);
}
