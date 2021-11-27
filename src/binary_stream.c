#include <stdlib.h>

#include "tanaka_type.h"
#include "binary_stream.h"

#define STREAM_BUFFER_SIZE 1024

typedef struct tBinaryStream_t {
    tByte *array;
    size_t head;
    size_t tail;
} tBinaryStream;

tBinaryStream *make_binary_stream() {
    tBinaryStream *stream = (tBinaryStream *)malloc(sizeof(tBinaryStream));

    stream->array = (tByte *)malloc(sizeof(tByte) * STREAM_BUFFER_SIZE);
    stream->head = 0;
    stream->tail = 0;

    return stream;
}

size_t t_stream_count_bytes(tBinaryStream *stream) {
    if (stream->head == stream->tail) {
        return 0;
    } else if (stream->head > stream->tail) {
        return stream->head - stream->tail;
    } else {
        return stream->head + (STREAM_BUFFER_SIZE - stream->tail);
    }
}

size_t t_stream_count_free_bytes(tBinaryStream *stream) {
    return STREAM_BUFFER_SIZE - t_stream_count_bytes(stream);
}

int t_peek_nth_byte(tBinaryStream *stream, size_t n, tByte *out_byte) {
    if (stream == NULL || t_stream_count_bytes(stream) <= n) {
        return STREAM_EMPTY;
    }

    *out_byte = stream->array[stream->tail + n];
    return 1;
}

static void proceed(size_t *pos) {
    *pos = (*pos + 1) % STREAM_BUFFER_SIZE;
}

int t_read_byte(tBinaryStream *stream, tByte *out_byte) {
    if (stream == NULL || t_stream_count_bytes(stream) <= 0) {
        return STREAM_EMPTY;
    }

    *out_byte = stream->array[stream->tail];
    proceed(&stream->tail);

    return 1;
}

int t_write_byte(tBinaryStream *stream, tByte byte) {
    // When the stream is full, head is placed at the previous byte of tail.
    // In this case, the head points a free byte but it never be used while the stream is full
    // because the head cannot go ahead by the tail.
    // This also means the maximum counts of bytes filled is always `STREAM_BUFFER_SIZE - 1`.
    if (stream == NULL || t_stream_count_bytes(stream) >= STREAM_BUFFER_SIZE - 1) {
        return STREAM_FULL;
    }

    stream->array[stream->head] = byte;
    proceed(&stream->head);

    return 1;
}

void t_clear_stream(tBinaryStream *stream) {
    stream->tail = stream->head;
}


#ifdef TANAKA_LISP_TEST

#include <assert.h>
#include <stdio.h>

static void test_peek_1st_byte_from_empty_stream() {
    tBinaryStream input = {NULL, 0, 0};
    size_t nth = 0;
    int expected_ret = STREAM_EMPTY;

    tByte actual_byte;
    int actual_ret = t_peek_nth_byte(&input, nth, &actual_byte);

    assert(actual_ret == expected_ret);
}

static void test_peek_2nd_byte_from_length1_stream() {
    tByte input_buf[STREAM_BUFFER_SIZE] = {'a', 'b', 'c'};
    tBinaryStream input = {input_buf, 1, 0};
    size_t nth = 1;
    int expected_ret = STREAM_EMPTY;

    tByte actual_byte;
    int actual_ret = t_peek_nth_byte(&input, nth, &actual_byte);

    assert(actual_ret == expected_ret);
}

static void verify_peek_nth_byte(tBinaryStream *input, size_t nth, int expected_ret, tByte expected_byte) {
    tByte actual_byte;
    int actual_ret = t_peek_nth_byte(input, nth, &actual_byte);

    assert(actual_ret == expected_ret);
    assert(actual_byte == expected_byte);
}

static void test_peek_1st_byte() {
    tByte input_buf[STREAM_BUFFER_SIZE] = {'a', 'b', 'c'};
    tBinaryStream input = {input_buf, 1, 0};
    size_t nth = 0;
    int expected_ret = 1;
    tByte expected_byte = 'a';
    size_t expected_tail = 0;

    verify_peek_nth_byte(&input, nth, expected_ret, expected_byte);
    assert(input.tail == expected_tail);
}

static void test_peek_2nd_byte() {
    tByte input_buf[STREAM_BUFFER_SIZE] = {'a', 'b', 'c'};
    tBinaryStream input = {input_buf, 2, 0};
    size_t nth = 1;
    int expected_ret = 1;
    tByte expected_byte = 'b';
    size_t expected_tail = 0;

    verify_peek_nth_byte(&input, nth, expected_ret, expected_byte);
    assert(input.tail == expected_tail);
}

static void test_read_byte_from_empty_stream() {
    tBinaryStream input = {NULL, 0, 0};
    int expected_ret = STREAM_EMPTY;

    tByte actual_byte;
    int actual_ret = t_read_byte(&input, &actual_byte);

    assert(actual_ret == expected_ret);
}

static void verify_read_byte(tBinaryStream *input, int expected_ret, tByte expected_byte) {
    tByte actual_byte;
    int actual_ret = t_read_byte(input, &actual_byte);

    assert(actual_ret == expected_ret);
    assert(actual_byte == expected_byte);
}

static void test_read_byte_one() {
    tByte input_buf[STREAM_BUFFER_SIZE] = {'a', 0, 0};
    tBinaryStream input = {input_buf, 1, 0};
    int expected_ret = 1;
    tByte expected_byte = 'a';
    size_t expected_tail = 1;

    verify_read_byte(&input, expected_ret, expected_byte);
    assert(input.tail == expected_tail);
}

static void test_write_byte_to_empty_stream() {
    tByte input_byte = 'a';
    int expected_ret = 1;
    size_t expected_head = 1;
    tByte expected_byte = 'a';

    tByte stream_buf[STREAM_BUFFER_SIZE] = {};
    tBinaryStream stream = {stream_buf, 0, 0};

    int actual_ret = t_write_byte(&stream, input_byte);

    assert(actual_ret == expected_ret);
    assert(stream.head == expected_head);
    assert(stream.array[stream.head - 1] == expected_byte);
}

static void test_write_byte_to_full_stream() {
    tByte stream_buf[STREAM_BUFFER_SIZE] = {};
    // full stream := its head points to the previous element of its tail
    tBinaryStream stream = {stream_buf, 12, 13};
    tByte input_byte = 'a';
    int expected_ret = STREAM_FULL;

    int actual_ret = t_write_byte(&stream, input_byte);
    assert(actual_ret == expected_ret);
}

void test_binary_stream_all() {
    test_peek_1st_byte_from_empty_stream();
    test_peek_2nd_byte_from_length1_stream();
    test_peek_1st_byte();
    test_peek_2nd_byte();

    test_read_byte_from_empty_stream();
    test_read_byte_one();

    test_write_byte_to_empty_stream();
    test_write_byte_to_full_stream();

    printf("test: binary stream -> ok\n");
}

#endif
