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
    T_NIL = 0x0,
    T_BOOL = 0x1,
    T_CHAR = 0x2,
    T_INTEGER = 0x3,
    T_FLOAT = 0x4,
    T_STREAM = 0x5
} tLispType;

// The stream object. It treats binary data but can be read/write as characters.
typedef struct tStream_t {
    tByte *array;
    size_t head;
    size_t tail;
} tStream;

typedef struct tLispObject_t {
    tLispType type;
    union {
        uint64_t primitive;
        tStream *stream;
    } o;
} tLispObject;

#endif
