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

typedef struct tObject_t tObject;

// The stream object. It treats binary data but can be read/write as characters.
typedef struct tStream_t {
    size_t head;
    size_t tail;
    tByte array[];
} tStream;

typedef struct tLispObject_t {
    union {
        uint64_t primitive;
        tObject *stream;
    } o;
} tLispObject;

typedef struct tObject_t {
    tByte type;
    size_t size;

    union {
        struct tObject_t *next;
        tLispObject lisp_obj;
        tStream stream;
    } o;
} tObject;

#define TLISP_TYPE(obj) (obj->type & 0x7f)
#define TLISP_MARKED(obj) (!!(obj->type & 0x80))
#define TLISP_MARK(obj) (obj->type = obj->type | 0x80)
#define TLISP_UNMARK(obj) (obj->type = obj->type & 0x7f)

#define TLISP_OBJ(obj) (obj->o.lisp_obj)
#define T_STREAM(obj) (obj->o.stream)

#endif
