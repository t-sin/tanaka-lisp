#include "tanaka-lisp.h"
#include "garbage_collector.h"

#define HASHTABLE_SUCCESS 0
#define HASHTABLE_NOT_FOUND -1
#define HASHTABLE_FULL -2

size_t hash(uintptr_t key, size_t size) {
    return key % size;
}

int t_hash_table_find(tHashTable *table, void *key, tObject **out_value) {
    uintptr_t k = (uintptr_t)key;
    size_t idx = hash(k, table->u.header.size);

    tHashTableEntry *entry = table->body[idx];
    while (entry != NULL) {
        if (entry->u.entry.key == k) {
            *out_value = entry->u.entry.value;
            return HASHTABLE_SUCCESS;
        }

        entry = entry->u.entry.next;
    }

    return HASHTABLE_NOT_FOUND;
}

int t_hash_table_put(tHashTable *table, void *key, tObject *value) {
    uintptr_t k = (uintptr_t)key;
    size_t idx = hash(k, table->u.header.size);

    if (table->body[idx] == NULL) {
        tHashTableEntry *new_entry = t_gc_allocate_hash_table_entry();
        new_entry->u.entry.key = k;
        new_entry->u.entry.value = value;
        new_entry->u.entry.next = NULL;

        table->body[idx] = new_entry;

        return HASHTABLE_SUCCESS;
    }

    tHashTableEntry *entry = table->body[idx];
    while (entry != NULL) {
        if (entry->u.entry.next == NULL) {
            tHashTableEntry *new_entry = t_gc_allocate_hash_table_entry();
            new_entry->u.entry.key = k;
            new_entry->u.entry.value = value;
            new_entry->u.entry.next = NULL;

            entry->u.entry.next = new_entry;

            return HASHTABLE_SUCCESS;
        }

        entry = entry->u.entry.next;
    }

    return HASHTABLE_FULL;
}

void t_hash_table_traverse(tHashTable *table, void (*traverse_func)(tObject *, tObject *)) {
    for (int i = 0; i < table->u.header.size; i++) {
        tHashTableEntry *entry = table->body[i];

        while (entry != NULL) {
            tObject *key = (tObject *)(void *)entry->u.entry.key;
            traverse_func(key, entry->u.entry.value);

            entry = entry->u.entry.next;
        }
    }
}
