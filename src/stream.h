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

// Sees an Unicode character but stream tail is not proceeded.
// It returns:
// - STREAM_EMPTY: when stream is empty
// - STREAM_INVALID_UTF8_OCTETS: when UTF-8 decoding fails
// - number of UTF-8 bytes: otherwise
int t_stream_peek_char(tStream *stream, tChar *out_ch);

// Reads an Unicode character then proceeds stream tail.
// It returns:
// - STREAM_EMPTY: when stream is empty
// - STREAM_INVALID_UTF8_OCTETS: when UTF-8 decoding fails
// - number of UTF-8 bytes: otherwise
int t_stream_read_char(tStream *stream, tChar *out_ch);

// Writes an Unicode character into `stream`.
// It returns:
// - STREAM_FULL: when stream is full
// - STREAM_INVALID_UTF8_OCTETS: when UTF-8 decoding fails
// - number of UTF-8 bytes: otherwise
int t_stream_write_char(tStream *stream, tChar ch);

int t_stream_unread_char(tStream *stream, tChar ch);

#ifdef TANAKA_LISP_TEST
void test_stream_all();
#endif

#endif
