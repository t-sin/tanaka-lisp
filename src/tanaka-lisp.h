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
    TLISP_CONS = 0x07,
    TLISP_ARRAY = 0x08,
    TLISP_HASH_TABLE = 0x09,
    TLISP_HASH_TABLE_ENTRY = 0x0a,
} tLispType;

#define STREAM_BUFFER_SIZE 1024

typedef struct tObject_t {
    tByte type;
    void *forwarding;
} tObject;

typedef struct tConsCell_t {
    tByte type;
    union {
        void *forwarding;
        struct {
            tObject *car;
            tObject *cdr;
        } cell;
    } u;
} tConsCell;

typedef struct tStreamTap_t {
    size_t head;
    size_t tail;
} tStreamTap;

// The stream object. It treats binary data but can be read/write as characters.
typedef struct tStream_t {
    tByte type;
    union {
        void *forwarding;
        tStreamTap tap;
    } u;
    tByte array[];
} tStream;

typedef struct tArray_t {
    tByte type;
    union {
        void *forwarding;
        struct {
            tByte elem_type;
            size_t num_elems;
        } header;
    } u;
    tByte body[];
} tArray;

typedef struct tHashTableEntry_t {
    tByte type;
    union {
        void *forwarding;
        struct {
            uintptr_t key;
            tObject *value;
        } entry;
    } u;
} tHashTableEntry;

typedef struct tHashTable_t {
    tByte type;
    union {
        void *forwarding;
        struct {
            size_t size;
            size_t num_elems;
        } header;
    } u;
    tHashTableEntry body[];
} tHashTable;

typedef struct tPrimitive_t {
    tByte type;
    union {
        void *forwarding;
        uint64_t primitive;
    } u;
} tPrimitive;

#define TLISP_TYPE(obj) (((tObject *)obj)->type)

typedef struct tLispRuntime_t {
    tObject *toplevel_obj;

    tStream *stdin;
    tStream *stdout;
} tLispRuntime;

extern tLispRuntime runtime;

#endif
