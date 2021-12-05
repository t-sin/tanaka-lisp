#include <stdlib.h>

#include "tanaka-lisp.h"
#include "garbage_collector.h"
#include "utf8.h"
#include "stream.h"

void t_stream_clear(tStream *stream) {
    stream->u.tap.tail = stream->u.tap.head;
}

size_t t_stream_count_bytes(tStream *stream) {
    if (stream->u.tap.head == stream->u.tap.tail) {
        return 0;
    } else if (stream->u.tap.head > stream->u.tap.tail) {
        return stream->u.tap.head - stream->u.tap.tail;
    } else {
        return stream->u.tap.head + (STREAM_BUFFER_SIZE - stream->u.tap.tail);
    }
}

size_t t_stream_count_free_bytes(tStream *stream) {
    return STREAM_BUFFER_SIZE - t_stream_count_bytes(stream);
}

int t_stream_peek_nth_byte(tStream *stream, size_t n, tByte *out_byte) {
    if (stream == NULL || t_stream_count_bytes(stream) <= n) {
        return STREAM_EMPTY;
    }

    *out_byte = stream->array[stream->u.tap.tail + n];
    return 1;
}

static void proceed(size_t *pos) {
    *pos = (*pos + 1) % STREAM_BUFFER_SIZE;
}

int t_stream_read_byte(tStream *stream, tByte *out_byte) {
    if (stream == NULL || t_stream_count_bytes(stream) <= 0) {
        return STREAM_EMPTY;
    }

    *out_byte = stream->array[stream->u.tap.tail];
    proceed(&stream->u.tap.tail);

    return 1;
}

int t_stream_write_byte(tStream *stream, tByte byte) {
    // When the stream is full, head is placed at the previous byte of tail.
    // In this case, the head points a free byte but it never be used while the stream is full
    // because the head cannot go ahead by the tail.
    // This also means the maximum counts of bytes filled is always `STREAM_BUFFER_SIZE - 1`.
    if (stream == NULL || t_stream_count_bytes(stream) >= STREAM_BUFFER_SIZE - 1) {
        return STREAM_FULL;
    }

    stream->array[stream->u.tap.head] = byte;
    proceed(&stream->u.tap.head);

    return 1;
}

int t_stream_peek_char(tStream *stream, tChar *out_ch) {
    tByte byte;
    int ret = t_stream_peek_nth_byte(stream, 0, &byte);
    if (ret != 1) {
        return ret;
    }

    int length = t_utf8_length(byte);
    if (length == -1) {
        return STREAM_INVALID_UTF8_OCTETS;
    }

    tByte bytes[4];
    for (int i = 0; i < length; i++) {
        ret = t_stream_peek_nth_byte(stream, i, &byte);
        if (ret != 1) {
            return STREAM_INVALID_UTF8_OCTETS;
        }

        bytes[i] = byte;
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
        t_stream_read_byte(stream, &b);
    }

    return ret;
}

int t_stream_write_char(tStream *stream, tChar ch) {
    tByte bytes[4];
    int length = t_utf8_encode(ch, bytes);
    int num_free = t_stream_count_free_bytes(stream);

    if (length > num_free - 1) {
        return STREAM_FULL;
    }

    for (int i = 0; i < length; i++) {
        t_stream_write_byte(stream, bytes[i]);
    }

    return length;
}

int t_stream_unread_char(tStream *stream, tChar ch);


#ifdef TANAKA_LISP_TEST

#include <assert.h>
#include <stdio.h>
#include <string.h>

static tStream *make_stream(size_t head, size_t tail, tByte *content) {
    tStream *stream = t_gc_allocate_stream();
    stream->u.tap.head = head;
    stream->u.tap.tail = tail;
    memcpy(stream->array, content, STREAM_BUFFER_SIZE);

    return stream;
}

static void test_peek_1st_byte_from_empty_stream() {
    tByte input_content[STREAM_BUFFER_SIZE] = {};
    size_t input_head = 0;
    size_t input_tail = 0;
    size_t nth = 0;
    int expected_ret = STREAM_EMPTY;

    tStream *stream = make_stream(input_head, input_tail, input_content);

    tByte actual_byte;
    int actual_ret = t_stream_peek_nth_byte(stream, nth, &actual_byte);

    assert(actual_ret == expected_ret);
}

static void test_peek_2nd_byte_from_length1_stream() {
    tByte input_content[STREAM_BUFFER_SIZE] = {'a', 'b', 'c'};
    size_t input_head = 1;
    size_t input_tail = 0;
    size_t nth = 1;
    int expected_ret = STREAM_EMPTY;

    tStream *stream = make_stream(input_head, input_tail, input_content);

    tByte actual_byte;
    int actual_ret = t_stream_peek_nth_byte(stream, nth, &actual_byte);

    assert(actual_ret == expected_ret);
}

static void verify_peek_nth_byte(tStream *input, size_t nth, int expected_ret, tByte expected_byte) {
    tByte actual_byte;
    int actual_ret = t_stream_peek_nth_byte(input, nth, &actual_byte);

    assert(actual_ret == expected_ret);
    assert(actual_byte == expected_byte);
}

static void test_peek_1st_byte() {
    tByte input_content[STREAM_BUFFER_SIZE] = {'a', 'b', 'c'};
    size_t input_head = 1;
    size_t input_tail = 0;
    size_t nth = 0;
    int expected_ret = 1;
    tByte expected_byte = 'a';
    size_t expected_tail = 0;

    tStream *stream = make_stream(input_head, input_tail, input_content);
    verify_peek_nth_byte(stream, nth, expected_ret, expected_byte);
    assert(stream->u.tap.tail == expected_tail);
}

static void test_peek_2nd_byte() {
    tByte input_content[STREAM_BUFFER_SIZE] = {'a', 'b', 'c'};
    size_t input_head = 2;
    size_t input_tail = 0;
    size_t nth = 1;
    int expected_ret = 1;
    tByte expected_byte = 'b';
    size_t expected_tail = 0;

    tStream *stream = make_stream(input_head, input_tail, input_content);
    verify_peek_nth_byte(stream, nth, expected_ret, expected_byte);
    assert(stream->u.tap.tail == expected_tail);
}

static void test_read_byte_from_empty_stream() {
    tByte input_content[STREAM_BUFFER_SIZE] = {};
    size_t input_head = 0;
    size_t input_tail = 0;
    int expected_ret = STREAM_EMPTY;

    tStream *stream = make_stream(input_head, input_tail, input_content);

    tByte actual_byte;
    int actual_ret = t_stream_read_byte(stream, &actual_byte);

    assert(actual_ret == expected_ret);
}

static void verify_read_byte(tStream *input, int expected_ret, tByte expected_byte) {
    tByte actual_byte;
    int actual_ret = t_stream_read_byte(input, &actual_byte);

    assert(actual_ret == expected_ret);
    assert(actual_byte == expected_byte);
}

static void test_read_byte_one() {
    tByte input_content[STREAM_BUFFER_SIZE] = {'a', 0, 0};
    size_t input_head = 1;
    size_t input_tail = 0;
    int expected_ret = 1;
    tByte expected_byte = 'a';
    size_t expected_tail = 1;

    tStream *stream = make_stream(input_head, input_tail, input_content);
    verify_read_byte(stream, expected_ret, expected_byte);
    assert(stream->u.tap.tail == expected_tail);
}

static void test_write_byte_to_empty_stream() {
    tByte input_content[STREAM_BUFFER_SIZE] = {};
    size_t input_head = 0;
    size_t input_tail = 0;
    tByte input_byte = 'a';
    int expected_ret = 1;
    size_t expected_head = 1;
    tByte expected_byte = 'a';

    tStream *stream = make_stream(input_head, input_tail, input_content);
    int actual_ret = t_stream_write_byte(stream, input_byte);

    assert(actual_ret == expected_ret);
    assert(stream->u.tap.head == expected_head);
    assert(stream->array[stream->u.tap.head - 1] == expected_byte);
}

static void test_write_byte_to_full_stream() {
    // full stream := its head points to the previous element of its tail
    tByte input_content[STREAM_BUFFER_SIZE] = {};
    size_t input_head = 12;
    size_t input_tail = 13;
    tByte input_byte = 'a';
    int expected_ret = STREAM_FULL;

    tStream *stream = make_stream(input_head, input_tail, input_content);
    int actual_ret = t_stream_write_byte(stream, input_byte);
    assert(actual_ret == expected_ret);
}

static tStream * make_empty_stream() {
    tByte initial_content[STREAM_BUFFER_SIZE] = {};
    tStream *stream = make_stream(0, 0, initial_content);

    return stream;
}

static void verify_peek_char(tByte *input, size_t size, int eret, tChar ech) {
    tStream *stream = make_empty_stream();

    for (int i = 0; i < size; i++) {
        t_stream_write_byte(stream, input[i]);
    }

    tChar actual_ch;
    int actual_ret = t_stream_peek_char(stream, &actual_ch);

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
    int expected_ret = 2;
    tChar expected_ch = 0x03bb;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_char_three_bytes() {
    tByte input[] = {0xe3, 0x81, 0x82}; // 'あ'
    int expected_ret = 3;
    tChar expected_ch = 0x03042;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_char_four_bytes() {
    tByte input[] = {0xf0, 0x9f, 0xa5, 0xb3}; // '🥳'
    int expected_ret = 4;
    tChar expected_ch = 0x1f973;

    int len = sizeof(input) / sizeof(input[0]);
    verify_peek_char(input, len, expected_ret, expected_ch);
}

static void test_peek_char_twice() {
    tByte input[] = {0xce, 0xbb, 0xf0, 0x9f, 0xa5, 0xb3}; // '🥳'
    int expected_ret = 2;
    tChar expected_ch = 0x03bb;

    tStream *stream = make_empty_stream();
    int len = sizeof(input) / sizeof(input[0]);
    for (int i = 0; i < len; i++) {
        t_stream_write_byte(stream, input[i]);
    }

    tChar actual_ch;
    int actual_ret = t_stream_peek_char(stream, &actual_ch);
    assert(actual_ret == expected_ret);
    assert(actual_ch == expected_ch);

    actual_ret = t_stream_peek_char(stream, &actual_ch);
    assert(actual_ret == expected_ret);
    assert(actual_ch == expected_ch);
}

static void verify_read_char(tByte *input, size_t size, int eret, tChar ech) {
    tStream *stream = make_empty_stream();

    for (int i = 0; i < size; i++) {
        t_stream_write_byte(stream, input[i]);
    }

    tChar actual_ch;
    int actual_ret = t_stream_read_char(stream, &actual_ch);

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

static void test_read_char_twice() {
    tByte input[] = {0xce, 0xbb, 0xf0, 0x9f, 0xa5, 0xb3}; // '🥳'
    int expected_ret1 = 2;
    int expected_ret2 = 4;
    tChar expected_ch1 = 0x03bb;
    tChar expected_ch2 = 0x1f973;

    tStream *stream = make_empty_stream();

    int len = sizeof(input) / sizeof(input[0]);
    for (int i = 0; i < len; i++) {
        t_stream_write_byte(stream, input[i]);
    }

    tChar actual_ch;
    int actual_ret = t_stream_read_char(stream, &actual_ch);
    assert(actual_ret == expected_ret1);
    assert(actual_ch == expected_ch1);

    actual_ret = t_stream_read_char(stream, &actual_ch);
    assert(actual_ret == expected_ret2);
    assert(actual_ch == expected_ch2);
}

static void verify_write_char_patterns(tStream *stream, int num, tChar *input, int *erets, tByte **ebytes) {
    for (int n = 0; n < num; n++) {
        int actual_ret = t_stream_write_char(stream, input[n]);

        assert(actual_ret == erets[n]);
        for (int i = 0; i < actual_ret; i++) {
            tByte actual_byte;
            t_stream_read_byte(stream, &actual_byte);
            assert(actual_byte == ebytes[n][i]);
        }
    }
}

static void test_write_char_one() {
    tChar input[] = {0x3042}; // 'あ'
    int expected_rets[] = {3};
    tByte ebytes1[] = {0xe3, 0x81, 0x82};
    tByte *expected_bytes[1] = {ebytes1};

    tStream *stream = make_empty_stream();
    verify_write_char_patterns(stream, 1, input, expected_rets, expected_bytes);
}

static void test_write_char_two() {
    tChar input[] = {0x3042, 0x1f973}; // 'あ', '🥳'
    int expected_rets[] = {3, 4};
    tByte ebytes1[] = {0xe3, 0x81, 0x82};
    tByte ebytes2[] = {0xf0, 0x9f, 0xa5, 0xb3};
    tByte *expected_bytes[2] = {ebytes1, ebytes2};

    tStream *stream = make_empty_stream();
    verify_write_char_patterns(stream, 1, input, expected_rets, expected_bytes);
}

void test_stream_all() {
    t_gc_setup();

    test_peek_1st_byte_from_empty_stream();
    test_peek_2nd_byte_from_length1_stream();
    test_peek_1st_byte();
    test_peek_2nd_byte();

    test_read_byte_from_empty_stream();
    test_read_byte_one();

    test_write_byte_to_empty_stream();
    test_write_byte_to_full_stream();

    test_peek_char_one_byte();
    test_peek_char_two_bytes();
    test_peek_char_three_bytes();
    test_peek_char_four_bytes();
    test_peek_char_twice();

    test_read_char_one_byte();
    test_read_char_twice();

    test_write_char_one();
    test_write_char_two();

    t_gc_terminate();

    printf("test: stream -> ok\n");
}

#endif
