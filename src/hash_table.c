#include "tanaka-lisp.h"
#include "garbage_collector.h"
#include "equality.h"

#define HASHTABLE_SUCCESS 0
#define HASHTABLE_NOT_FOUND -1
#define HASHTABLE_FULL -2

size_t hash(tObject *obj, size_t size) {
    size_t val;

    switch (TLISP_TYPE(obj)) {
    case TLISP_NIL:
        val = TLISP_NIL;
        break;

    case TLISP_BOOL:
        val = TLISP_BOOL + ((tPrimitive *)obj)->u.primitive;
        break;

    case TLISP_CHAR:
        val = TLISP_CHAR + ((tPrimitive *)obj)->u.primitive;
        break;

    case TLISP_INTEGER:
        val = TLISP_INTEGER + ((tPrimitive *)obj)->u.primitive;
        break;

    // case TLISP_FLOAT:
    //     val = TLISP_FLOAT + ((tPrimitive *)obj)->u.primitive;
    //     break;

    case TLISP_STREAM:
        val = TLISP_STREAM + (uintptr_t)obj;
        break;

    case TLISP_CONS: {
            tConsCell *cons = (tConsCell *)obj;
            val = TLISP_CONS + hash(cons->u.cell.car, size) + hash(cons->u.cell.car, size);
            break;
        }

    case TLISP_ARRAY:
        val = TLISP_ARRAY + (uintptr_t)obj;
        break;

    case TLISP_HASH_TABLE:
        val = TLISP_ARRAY + (uintptr_t)obj;
        break;

    default:
        val = 0;
    }

    return val % size;
}

int t_hash_table_find(tHashTable *table, void *key, tObject **out_value) {
    size_t idx = hash(key, table->u.header.size);

    tHashTableEntry *entry = table->body[idx];
    while (entry != NULL) {
        if (t_equality_equal((tObject *)entry->u.entry.key, (tObject *)key)) {
            *out_value = entry->u.entry.value;
            return HASHTABLE_SUCCESS;
        }

        entry = entry->u.entry.next;
    }

    return HASHTABLE_NOT_FOUND;
}

int t_hash_table_put(tHashTable *table, void *key, tObject *value) {
    size_t idx = hash((tObject *)key, table->u.header.size);

    if (table->body[idx] == NULL) {
        tHashTableEntry *new_entry = t_gc_allocate_hash_table_entry();
        new_entry->u.entry.key = key;
        new_entry->u.entry.value = value;
        new_entry->u.entry.next = NULL;

        table->body[idx] = new_entry;

        return HASHTABLE_SUCCESS;
    }

    tHashTableEntry *entry = table->body[idx];
    while (entry != NULL) {
        if (t_equality_equal((tObject *)entry->u.entry.key, (tObject *)key)) {
            entry->u.entry.value = value;

            return HASHTABLE_SUCCESS;

        } else if (entry->u.entry.next == NULL) {
            tHashTableEntry *new_entry = t_gc_allocate_hash_table_entry();
            new_entry->u.entry.key = key;
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
