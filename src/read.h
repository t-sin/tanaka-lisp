#ifndef tanaka_lisp_read
#define tanaka_lisp_read

#define READ_NO_INPUT 0
#define READ_MORE -1
#define READ_FAILED -2

int tLisp_read(tStream *in, tLispObject *out_obj);


#ifdef TANAKA_LISP_TEST
void test_read_all();
#endif

#endif
