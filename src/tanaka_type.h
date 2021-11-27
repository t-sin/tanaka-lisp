#ifndef tanaka_lisp_type
#define tanaka_lisp_type

#include <stdint.h>

typedef uint8_t tByte;
typedef uint32_t tChar;

typedef enum {
    BOOLE,
} tLispType;

typedef struct tLispObject_t {
    tLispType type;
    union {
        uint8_t bool;
    } o;
} tLispObject;

#endif
