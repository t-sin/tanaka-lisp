#ifndef tanaka_lisp_string_repr
#define tanaka_lisp_string_repr

#include "tanaka-lisp.h"
#include "stream.h"

#define READ_NO_INPUT 0
#define READ_MORE -1
#define READ_FAILED -2

int tLisp_read(tStream *in, tLispObject *out_obj);
void tLisp_print(tStream *out, tLispObject *obj);

#ifdef TANAKA_LISP_TEST
void test_string_repr_all();
#endif

#endif
