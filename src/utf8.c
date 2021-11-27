#include <stdint.h>

#include "tanaka_type.h"

int t_utf8_length(tByte first_byte) {
    if (!(first_byte & 0x80)) {
        return 1;
    } else if (first_byte & 0xc0) {
        return 2;
    } else if (first_byte & 0xe0) {
        return 3;
    } else if (first_byte & 0xf0) {
        return 4;
    } else {
        return -1;
    }
}

int t_utf8_decode(const tByte *bytes, int start, int len, tChar *out_char) {
    int n = 0;
    tByte byte = bytes[start + n];

    switch (t_utf8_length(byte)) {
    case 1:
        *out_char = byte;
        return n + 1;
    default:
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
    int actual_ret = t_utf8_decode(input_bytes, input_start, input_len, &actual_char);

    assert(actual_ret == expected_ret);
    assert(actual_char == expected_char);
}

void test_utf8_all() {
    test_utf8_decode_ascii_one();

    printf("test: utf8 -> ok\n");
}

#endif