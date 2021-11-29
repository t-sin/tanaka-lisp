#ifndef tanaka_lisp_stream
#define tanaka_lisp_stream

#include "tanaka-lisp.h"

#define STREAM_EMPTY -1
#define STREAM_FULL -2
#define STREAM_INVALID_UTF8_OCTETS -3

tStream *make_stream();

// Clear stream contents.
void t_stream_clear(tStream *stream);

// Counts a number of bytes stored in `stream`.
size_t t_stream_count_bytes(tStream *stream);

// Counts a number of free bytes in `stream`.
size_t t_stream_count_free_bytes(tStream *stream);


// Sees a `n`-th byte from head of the stream but its head is not proceeded.
// If the stream is empty (when `n`-th byte from head and tail are same), it returns STREAM_EMPTY.
// Otherwise it returns a number of peek, so always returns one.
int t_stream_peek_nth_byte(tStream *stream, size_t n, tByte *out_byte);

// Reads a byte from head of the stream head then proceeds its head.
// If the stream is empty (when its head and tail are same), it returns STREAM_EMPTY.
// Otherwise it returns a number of read, so always returns one.
int t_stream_read_byte(tStream *stream, tByte *out_byte);

// Writes a byte to at tail of the stream then proceeds its tail.
// If the stream is full (when its head is at previous position of head), it returns STREAM_FULL.
// Otherwise it returns a number of write, so always returns one.
int t_stream_write_byte(tStream *stream, tByte byte);


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
