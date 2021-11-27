#ifndef tanaka_lisp_utf8
#define tanaka_lisp_utf8

#include <stdint.h>

#include "tanaka_type.h"

// Distinguishes UTF-8 octet length from the first byte.
int t_utf8_length(tByte first_byte);

// Decode UTF-8 octets.
// If this reads valid bytes it returns a number of bytes read, otherwise -1.
//
// Specification fo UTF-8 octets: https://datatracker.ietf.org/doc/html/rfc3629#section-4
int tanaka_utf8_decode(const uint8_t *bytes, int start, int len, tChar *out_char);

#ifdef TANAKA_LISP_TEST
void test_utf8_all();
#endif

#endif
