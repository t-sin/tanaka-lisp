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

int t_stream_peek_byte(tStream *stream, tByte *out_byte) {
    return t_peek_byte(stream->bstream, out_byte);
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
    int ret = t_peek_byte(stream->bstream, &byte);
    if (ret != 1) {
        return ret;
    }

    int length = t_utf8_length(byte);
    if (length == -1) {
        return STREAM_INVALID_UTF8_OCTETS;
    }

    tByte bytes[4];
    switch (length) {
    case 1:
        bytes[0] = byte;
        break;

    default:
        printf("NOT SUPPORTED!\n");
        return STREAM_INVALID_UTF8_OCTETS;
    }

    ret = t_utf8_decode(bytes, out_ch);
    if (ret == UTF8_INVALID_OCTETS) {
        return STREAM_INVALID_UTF8_OCTETS;
    }

    return length;
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

static verify_peek_char(tByte *input, size_t size, int eret, tChar ech) {
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

static verify_read_char(tByte *input, size_t size, int eret, tChar ech) {
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

    test_read_char_one_byte();

    printf("test: stream -> ok\n");
}
#endif
