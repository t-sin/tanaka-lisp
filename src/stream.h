#ifndef tanaka_lisp_stream
#define tanaka_lisp_stream

#include "tanaka_type.h"
#include "binary_stream.h"

typedef struct tStream_t tStream;

#define STREAM_INVALID_UTF8_OCTETS -3

tStream *make_stream(tBinaryStream *bstream);

// These are thin wrapper of binary_stream.h functions.
int t_stream_peek_byte(tStream *stream, tByte *out_byte);
int t_stream_read_byte(tStream *stream, tByte *out_byte);
int t_stream_write_byte(tStream *stream, tByte byte);
int t_stream_clear(tStream *stream);

int t_stream_peek_char(tStream *stream, tChar *out_ch);
int t_stream_read_char(tStream *stream, tChar *out_ch);
int t_stream_write_char(tStream *stream, tChar ch);
int t_stream_unread_char(tStream *stream, tChar ch);

#ifdef TANAKA_LISP_TEST
void test_stream_all();
#endif

#endif
