#ifndef tanaka_lisp_binary_stream
#define tanaka_lisp_binary_stream

#include "tanaka_type.h"

// Raw stream object for octets.
// Intended to make an abstraction layer to used by tanaka-lisp.
// In this layer, it does not provides text I/O.
typedef struct tBinaryStream_t tBinaryStream;

#define STREAM_EMPTY -1
#define STREAM_FULL -2

tBinaryStream *make_binary_stream();

// Sees a byte from head of the stream but its head is not proceeded.
// If the stream is empty (when its head and tail are same), it returns STREAM_EMPTY.
// Otherwise it returns a number of peek, so always returns one.
int t_peek_byte(tBinaryStream *stream, tByte *out_byte);

// Reads a byte from head of the stream head then proceeds its head.
// If the stream is empty (when its head and tail are same), it returns STREAM_EMPTY.
// Otherwise it returns a number of read, so always returns one.
int t_read_byte(tBinaryStream *stream, tByte *out_byte);

// Writes a byte to at tail of the stream then proceeds its tail.
// If the stream is full (when its head is at previous position of head), it returns STREAM_FULL.
// Otherwise it returns a number of write, so always returns one.
int t_write_byte(tBinaryStream *stream, tByte byte);

// Clear stream contents.
void t_clear_stream(tBinaryStream *stream);


#ifdef TANAKA_LISP_TEST
void test_binary_stream_all();
#endif

#endif
