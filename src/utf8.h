#ifndef tanaka_lisp_utf8
#define tanaka_lisp_utf8

#include <stdint.h>

#include "tanaka_type.h"

#define UTF8_INVALID_OCTETS -1

// Distinguishes UTF-8 octet length from the first byte.
int t_utf8_length(tByte first_byte);

// Decodes UTF-8 octets.
// If this reads valid bytes it returns a number of bytes decoded, otherwise `UTF8_INVALID_OCTETS`.
// It assumes that `bytes4` has 4-byte length.
//
// Specification fo UTF-8 octets: https://datatracker.ietf.org/doc/html/rfc3629#section-4
int t_utf8_decode(const tByte *bytes4, tChar *out_char);

// Encodes Unicode codepoint and write bytes into `out_4bytes`.
// It returns a number of bytes encoded.
// It assumes that `out_bytes4` has 4-byte length.
int t_utf8_encode(tChar ch, tByte *out_bytes4);

#ifdef TANAKA_LISP_TEST
void test_utf8_all();
#endif

#endif
