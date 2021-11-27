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

    return 0;
}

int t_stream_peek_byte(tBinaryStream *stream, tByte *out_byte) {
int t_stream_read_byte(tBinaryStream *stream, tByte *out_byte) {}
int t_stream_write_byte(tBinaryStream *stream, tByte byte) {}
