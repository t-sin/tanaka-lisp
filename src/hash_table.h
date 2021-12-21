#ifndef tanaka_lisp_hash_table
#define tanaka_lisp_hash_table


#include "tanaka-lisp.h"

#define HASHTABLE_SUCCESS 0
#define HASHTABLE_NOT_FOUND -1
#define HASHTABLE_FULL -2

int t_hash_table_find(tHashTable *table, void *key, tObject **out_value);
int t_hash_table_put(tHashTable *table, void *key, tObject *value);

void t_hash_table_traverse(tHashTable *table, void (*traverse_func)(tObject *, tObject *));

#endif
