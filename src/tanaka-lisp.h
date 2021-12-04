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
    // primitive lisp types
    TLISP_NULL = 0x00,
    TLISP_NIL = 0x01,
    TLISP_BOOL = 0x02,
    TLISP_CHAR = 0x03,
    TLISP_INTEGER = 0x04,
    TLISP_FLOAT = 0x05,
    // lisp objects
    TLISP_STREAM = 0x06,
    // other types
    T_STREAM = 0x0a,
} tLispType;

#define STREAM_BUFFER_SIZE 1024

typedef struct tObjectHeader_t {
    tByte type;
    void *forwarding;
} tObjectHeader;

#define tHeader tObjectHeader header

// The stream object. It treats binary data but can be read/write as characters.
typedef struct tStream_t {
    tHeader;

    size_t head;
    size_t tail;
    tByte array[];
} tStream;

typedef struct tLispObject_t {
    tHeader;

    union {
        uint64_t primitive;
        tStream *stream;
    } o;
} tLispObject;

#define TLISP_TYPE(obj) (((tObjectHeader *)obj)->header.type)

typedef struct tLispRuntime_t {
    tLispObject *toplevel_obj;

    tStream *stdin;
    tStream *stdout;
} tLispRuntime;

static tLispRuntime runtime;

#endif
