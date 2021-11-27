#include <stdint.h>

#include "tanaka_type.h"
#include "utf8.h"

int t_utf8_length(tByte first_byte) {
    if ((first_byte & 0x80) == 0x00) {
        return 1;
    } else if ((first_byte & 0xe0) == 0xc0) {
        return 2;
    } else if ((first_byte & 0xf0) == 0xe0) {
        return 3;
    } else if ((first_byte & 0xf8) == 0xf0) {
        return 4;
    } else {
        return UTF8_INVALID_OCTETS;
    }
}

int t_utf8_decode(const tByte *bytes4, tChar *out_char) {
    tChar tmp;

    switch (t_utf8_length(bytes4[0])) {
    case 1:
        tmp = bytes4[0];
        *out_char = tmp;

        return 1;

    case 2:
        tmp = (bytes4[0] & 0x1f) << 6;
        tmp = (bytes4[1] & 0x3f) | tmp;
        *out_char = tmp;

        return 2;

    case 3:
        tmp = (bytes4[0] & 0x0f) << 12;
        tmp = (bytes4[1] & 0x3f) << 6 | tmp;
        tmp = (bytes4[2] & 0x3f) | tmp;
        *out_char = tmp;

        return 3;

    case 4:
        tmp = (bytes4[0] & 0x07) << 18;
        tmp = (bytes4[1] & 0x3f) << 12 | tmp;
        tmp = (bytes4[2] & 0x3f) << 6 | tmp;
        tmp = (bytes4[3] & 0x3f) | tmp;
        *out_char = tmp;

        return 4;

    default:
        return UTF8_INVALID_OCTETS;
    }
}


#ifdef TANAKA_LISP_TEST

#include <assert.h>
#include <stdio.h>

#define INPUT_SIZE 4

static void verify_utf8_decode(tByte *input, int eret, tChar ech) {
    uint32_t actual_ch;
    int actual_ret = t_utf8_decode(input, &actual_ch);

    assert(actual_ret == eret);
    assert(actual_ch == ech);
}

static void test_utf8_decode_ascii_one() {
    tByte input[INPUT_SIZE] = {'a'};
    int expected_ret = 1;
    tChar expected_ch = 'a';

    verify_utf8_decode(input, expected_ret, expected_ch);
}

static void test_utf8_decode_greek_one() {
    tByte input[INPUT_SIZE] = {0xce, 0xbb};
    int expected_ret = 2;
    tChar expected_ch = 0x03bb; // 'λ'

    verify_utf8_decode(input, expected_ret, expected_ch);
}

static void test_utf8_decode_hiragana_one() {
    tByte input[INPUT_SIZE] = {0xe3, 0x81, 0x82};
    int expected_ret = 3;
    tChar expected_ch = 0x3042; // 'あ'

    verify_utf8_decode(input, expected_ret, expected_ch);
}

static void test_utf8_decode_partying_face_one() {
    tByte input[INPUT_SIZE] = {0xf0, 0x9f, 0xa5, 0xb3};
    int expected_ret = 4;
    tChar expected_ch = 0x1f973; // '🥳'

    verify_utf8_decode(input, expected_ret, expected_ch);
}

void test_utf8_all() {
    test_utf8_decode_ascii_one();
    test_utf8_decode_greek_one();
    test_utf8_decode_hiragana_one();
    test_utf8_decode_partying_face_one();

    printf("test: utf8 -> ok\n");
}

#endif