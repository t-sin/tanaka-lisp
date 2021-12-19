#ifndef tanaka_lisp_gc
#define tanaka_lisp_gc

#include "tanaka-lisp.h"

void t_gc_setup();
void t_gc_terminate();

size_t calculate_struct_size(int type);
size_t calculate_size(void *obj);

tPrimitive *t_gc_allocate_nil();
tPrimitive *t_gc_allocate_bool(int v);
tPrimitive *t_gc_allocate_char(tChar v);
tPrimitive *t_gc_allocate_integer(tInt v);
tPrimitive *t_gc_allocate_float(tFloat v);

tConsCell *t_gc_allocate_cons(tObject *car, tObject *cdr);
tStream *t_gc_allocate_stream();
tArray *t_gc_allocate_array(int type, size_t num);

tHashTableEntry *t_gc_allocate_hash_table_entry();
tHashTable *t_gc_allocate_hash_table(size_t num);

#ifdef TANAKA_LISP_TEST
void test_gc_all();
#endif

#endif
