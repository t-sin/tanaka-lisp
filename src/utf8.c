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

int t_utf8_encode(tChar ch, tByte *out_bytes4) {
    if (ch <= 0x007f) {
        out_bytes4[0] = (ch & 0x007f);
        return 1;

    } else if (ch >= 0x0080 && ch <=0x07ff) {
        out_bytes4[0] = 0xc0 | ((ch >> 6) & 0x1f);
        out_bytes4[1] = 0x80 | (ch & 0x3f);
        return 2;

    } else if (ch >= 0x0800 && ch <=0xffff) {
        out_bytes4[0] = 0xe0 | ((ch >> 12) & 0x0f);
        out_bytes4[1] = 0x80 | ((ch >> 6) & 0x3f);
        out_bytes4[2] = 0x80 | (ch & 0x003f);
        return 3;

    } else if (ch >= 0x10000 && ch <=0x10ffff) {
        out_bytes4[0] = 0xf0 | ((ch >> 18) & 0x07);
        out_bytes4[1] = 0x80 | ((ch >> 12) & 0x3f);
        out_bytes4[2] = 0x80 | ((ch >> 6) & 0x3f);
        out_bytes4[3] = 0x80 | (ch & 0x3f);
        return 4;
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

static void verify_utf8_encode(tChar input, int eret, tByte *ebytes) {
    tByte actual_bytes[INPUT_SIZE];
    int actual_ret = t_utf8_encode(input, actual_bytes);

    assert(actual_ret == eret);
    for (int i = 0; i < eret; i++) {
        assert(actual_bytes[i] == ebytes[i]);
    }
}

static void test_utf8_encode_ascii() {
    tChar input = 'a';
    int expected_ret = 1;
    tByte expected_bytes[INPUT_SIZE] = {'a'};

    verify_utf8_encode(input, expected_ret, expected_bytes);
}

static void test_utf8_encode_greek() {
    tChar input = 0x03bb; // 'λ'
    int expected_ret = 2;
    tByte expected_bytes[INPUT_SIZE] = {0xce, 0xbb};

    verify_utf8_encode(input, expected_ret, expected_bytes);
}

static void test_utf8_encode_hiragana() {
    tChar input = 0x3042; // あ
    int expected_ret = 3;
    tByte expected_bytes[INPUT_SIZE] = {0xe3, 0x81, 0x82};

    verify_utf8_encode(input, expected_ret, expected_bytes);
}

static void test_utf8_encode_partying_face() {
    tChar input = 0x1f973; // 🥳
    int expected_ret = 4;
    tByte expected_bytes[INPUT_SIZE] = {0xf0, 0x9f, 0xa5, 0xb3};

    verify_utf8_encode(input, expected_ret, expected_bytes);
}

void test_utf8_all() {
    test_utf8_decode_ascii_one();
    test_utf8_decode_greek_one();
    test_utf8_decode_hiragana_one();
    test_utf8_decode_partying_face_one();

    test_utf8_encode_ascii();
    test_utf8_encode_greek();
    test_utf8_encode_hiragana();
    test_utf8_encode_partying_face();

    printf("test: utf8 -> ok\n");
}

#endif
