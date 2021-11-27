#include <stdlib.h>

#include "tanaka_type.h"
#include "binary_stream.h"
#include "stream.h"
#include "utf8.h"

typedef struct tStream_t {
    tBinaryStream *bstream;
} tStream;

tStream *make_stream(tBinaryStream *bstream) {
    tStream *stream = (tStream *)malloc(sizeof(tStream));
    stream->bstream = bstream;

    return stream;
}

int t_stream_peek_nth_byte(tStream *stream, size_t nth, tByte *out_byte) {
    return t_peek_nth_byte(stream->bstream, nth, out_byte);
}

int t_stream_read_byte(tStream *stream, tByte *out_byte) {
    return t_read_byte(stream->bstream, out_byte);
}
int t_stream_write_byte(tStream *stream, tByte byte) {
    return t_write_byte(stream->bstream, byte);
}

int t_stream_clear(tStream *stream) {
    t_clear_stream(stream->bstream);
}

int t_stream_peek_char(tStream *stream, tChar *out_ch) {
    tByte byte;
    int ret = t_peek_nth_byte(stream->bstream, 0, &byte);
    if (ret != 1) {
        return ret;
    }

    int length = t_utf8_length(byte);
    if (length == -1) {
        return STREAM_INVALID_UTF8_OCTETS;
    }

    tByte bytes[4];
    for (int i = 0; i < length; i++) {
        ret = t_peek_nth_byte(stream->bstream, i, &byte);
        if (ret != 1) {
            return STREAM_INVALID_UTF8_OCTETS;
        }

        bytes[i] = byte;
    }

    ret = t_utf8_decode(bytes, out_ch);
    if (ret == UTF8_INVALID_OCTETS) {
        return STREAM_INVALID_UTF8_OCTETS;
    }

    return 1;
}

int t_stream_read_char(tStream *stream, tChar *out_ch) {
    int ret = t_stream_peek_char(stream, out_ch);
    if (ret < 0) {
        return ret;
    }

    tByte b;
    for (int i = 0; i < ret; i++) {
        t_read_byte(stream->bstream, &b);
    }

    return ret;
}

int t_stream_write_char(tStream *stream, tChar ch);
int t_stream_unread_char(tStream *stream, tChar ch);


#ifdef TANAKA_LISP_TEST
#include <assert.h>
#include <stdio.h>

static void verify_peek_char(tByte *input, size_t size, int eret, tChar ech) {
    tBinaryStream *bstream = make_binary_stream();
    for (int i = 0; i < size; i++) {
        t_write_byte(bstream, input[i]);
    }
    tStream stream = {bstream};

    tChar actual_ch;
    int actual_ret = t_stream_peek_char(&stream, &actual_ch);

    assert(actual_ret == eret);
    assert(actual_ch == ech);
}

static void test_peek_char_one_byte() {
    tByte input[] = {'a'};
    int expected_ret = 1;
    tChar expected_ch = 'a';

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_char_two_bytes() {
    tByte input[] = {0xce, 0xbb}; // 'λ'
    int expected_ret = 1;
    tChar expected_ch = 0x03bb;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_char_three_bytes() {
    tByte input[] = {0xe3, 0x81, 0x82}; // 'あ'
    int expected_ret = 1;
    tChar expected_ch = 0x03042;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_char_four_bytes() {
    tByte input[] = {0xf0, 0x9f, 0xa5, 0xb3}; // '🥳'
    int expected_ret = 1;
    tChar expected_ch = 0x1f973;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_same_char_twice() {
    tByte input[] = {0xce, 0xbb, 0xf0, 0x9f, 0xa5, 0xb3}; // '🥳'
    int expected_ret = 1;
    tChar expected_ch = 0x03bb;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void verify_read_char(tByte *input, size_t size, int eret, tChar ech) {
    tBinaryStream *bstream = make_binary_stream();
    for (int i = 0; i < size; i++) {
        t_write_byte(bstream, input[i]);
    }
    tStream stream = {bstream};

    tChar actual_ch;
    int actual_ret = t_stream_read_char(&stream, &actual_ch);

    assert(actual_ret == eret);
    assert(actual_ch == ech);
}

static void test_read_char_one_byte() {
    tByte input[] = {'a'};
    int expected_ret = 1;
    tChar expected_ch = 'a';

    int len = sizeof(input) / sizeof(input[0]);
    verify_read_char(input, len, expected_ret, expected_ch);
}

void test_stream_all() {
    test_peek_char_one_byte();
    test_peek_char_two_bytes();
    test_peek_char_three_bytes();
    test_peek_char_four_bytes();
    test_peek_same_char_twice();

    test_read_char_one_byte();

    printf("test: stream -> ok\n");
}
#endif
