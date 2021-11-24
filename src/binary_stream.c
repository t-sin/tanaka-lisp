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
    stream->head = 0;

    return stream;
}

int t_peek_byte(tBinaryStream *stream, tByte *out_byte) {
    return 0;
}
int t_read_byte(tBinaryStream *stream, tByte *out_byte) {}

int t_write_byte(tBinaryStream *stream, tByte byte) {}
