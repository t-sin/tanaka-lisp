#ifndef tanaka_lisp_type
#define tanaka_lisp_type

#include <stdint.h>

typedef uint8_t tByte;
typedef int64_t tInt;
typedef uint32_t tChar;

typedef enum {
    BOOLE, INTEGER,
} tLispType;

typedef struct tLispObject_t {
    tLispType type;
    union {
        uint8_t bool;
        tInt intn;
    } o;
} tLispObject;

#endif
