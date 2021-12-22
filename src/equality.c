#include "tanaka-lisp.h"

int t_equality_eq(tObject *a, tObject *b) {
    return a == b;
}

int t_equality_equal(tObject *a, tObject *b) {
    if (TLISP_TYPE(a) != TLISP_TYPE(b)) {
        return 0;
    }

    switch (TLISP_TYPE(a)) {
    case TLISP_NIL:
        return 1;

    case TLISP_BOOL:
    case TLISP_CHAR:
    case TLISP_INTEGER:
    case TLISP_FLOAT:
        return ((tPrimitive *)a)->u.primitive == ((tPrimitive *)b)->u.primitive;

    case TLISP_CONS:
    case TLISP_STREAM:
    case TLISP_ARRAY:
    case TLISP_HASH_TABLE:
        return t_equality_eq(a, b);

    default:
        0;
    }
}
