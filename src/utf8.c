#include <stdint.h>

#include "tanaka_type.h"

int tanaka_utf8_decode(const uint8_t *bytes, int start, int len, tChar *out_char) {
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

#include <assert.h>
#include <stdio.h>

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
    test_utf8_decode_ascii_one();

    printf("test: utf8 -> ok\n");
}

#endif