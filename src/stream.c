#include <stdlib.h>

#include "tanaka_type.h"
#include "binary_stream.h"
#include "stream.h"

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

int t_stream_peek_char(tStream *stream, tChar *out_ch);
int t_stream_read_char(tStream *stream, tChar *out_ch);
int t_stream_write_char(tStream *stream, tChar ch);
int t_stream_unread_char(tStream *stream, tChar ch);


#ifdef TANAKA_LISP_TEST
#include <assert.h>
#include <stdio.h>

void test_stream_all() {
    printf("test: stream -> ok\n");
}
#endif
