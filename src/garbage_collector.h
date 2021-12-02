#ifndef tanaka_lisp_gc
#define tanaka_lisp_gc

#include "tanaka-lisp.h"

void t_gc_setup();
void t_gc_terminate();

tLispObject *t_gc_allocate_nil();
tLispObject *t_gc_allocate_bool(int v);
tLispObject *t_gc_allocate_char(tChar v);
tLispObject *t_gc_allocate_integer(tInt v);
tLispObject *t_gc_allocate_float(tFloat v);

tObject *t_gc_allocate_stream_obj();
tObject *t_gc_allocate_stream();


#ifdef TANAKA_LISP_TEST
void test_gc_all();
#endif

#endif
