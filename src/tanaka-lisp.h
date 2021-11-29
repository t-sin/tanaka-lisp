#ifndef tanaka_lisp
#define tanaka_lisp

#include <stdint.h>
#include <stddef.h>

// The primitive types.
// These (tByte, tInt, tFloat and tChar) are not managed on GC.
typedef uint8_t  tByte;
typedef int64_t  tInt;
typedef double   tFloat;
typedef uint32_t tChar;

typedef enum {
    // primitive types
    T_NIL = 0x00,
    T_BOOL = 0x01,
    T_CHAR = 0x02,
    T_INTEGER = 0x03,
    T_FLOAT = 0x04,
    T_STREAM = 0x05
} tLispType;

// The stream object. It treats binary data but can be read/write as characters.
typedef struct tStream_t {
    tByte *array;
    size_t head;
    size_t tail;
} tStream;

typedef struct tLispObject_t {
    tByte type;
    union {
        uint64_t primitive;
        tStream *stream;
    } o;
} tLispObject;

#endif
