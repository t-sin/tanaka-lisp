#ifndef tanaka_lisp_type
#define tanaka_lisp_type

#include <stdint.h>

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
} tLispType;

typedef struct tLispObject_t {
    tLispType type;
    union {
        uint64_t primitive;
    } o;
} tLispObject;

#endif
