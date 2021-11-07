#include <assert.h>
#include <stdint.h>

#include "tanaka_type.h"

// Decode UTF-8 octet sequence.
// If this reads valid bytes it returns a number of bytes read, otherwise -1.
// cf. https://datatracker.ietf.org/doc/html/rfc3629#section-4
int tanaka_utf8_decode(const uint8_t *bytes, int start, int len, TanakaChar *out_char) {
    int n = 0;
    uint8_t byte = bytes[start + n];

    if (!(byte & 0x80)) {
        *out_char = byte;
        return n + 1;

    } else {
        return -1;
    }
}


#ifdef TANAKA_LISP_TEST

static void test_utf8_decode_ascii_one() {
    uint8_t input_bytes[] = {'a'};
    int input_start = 0;
    int input_len = sizeof(input_bytes) / sizeof(input_bytes[0]);
    uint32_t expected_char = 'a';
    int expected_ret = 1;

    uint32_t actual_char;
    int actual_ret = tanaka_utf8_decode(input_bytes, input_start, input_len, &actual_char);

    assert(actual_ret == expected_ret);
    assert(actual_char == expected_char);
}

void test_utf8_all() {
    test_utf8_decode_ascii();
}

#endif