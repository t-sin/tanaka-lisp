#ifndef tanaka_lisp_utf8
#define tanaka_lisp_utf8

#include <stdint.h>

#include "tanaka_type.h"

int tanaka_utf8_decode(const uint8_t *bytes, int start, int len, TanakaChar *out_char);

#ifdef TANAKA_LISP_TEST
void test_utf8_all();
#endif

#endif
