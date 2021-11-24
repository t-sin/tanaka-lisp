#ifndef tanaka_lisp_binary_stream
#define tanaka_lisp_binary_stream

#include "tanaka_type.h"

typedef struct tBinaryStream_t tBinaryStream;

#define STREAM_EMPTY -1
#define STREAM_FULL -2

tBinaryStream *make_binary_stream();

int t_peek_byte(tBinaryStream *stream, tByte *out_byte);
int t_read_byte(tBinaryStream *stream, tByte *out_byte);

int t_write_byte(tBinaryStream *stream, tByte byte);

#endif
