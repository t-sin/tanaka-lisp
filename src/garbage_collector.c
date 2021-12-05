#include <assert.h>
#include <sys/mman.h>
#include <stdio.h>
#include <string.h>

#include "tanaka-lisp.h"
#include "garbage_collector.h"


#define HEAP_SIZE (1024 * 10)
#define AREA_SIZE (HEAP_SIZE / 2)

static void *heap_from = NULL;
static void *heap_to = NULL;
static void *heap_free = NULL;


void t_gc_setup() {
    int protect = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;

    heap_from = mmap(NULL, AREA_SIZE, protect, flags, -1, 0);
    heap_to = mmap(NULL, AREA_SIZE, protect, flags, -1, 0);
    heap_free = heap_from;

    assert(heap_from != NULL);
    assert(heap_to != NULL);
}

void t_gc_terminate() {
    munmap(heap_from, AREA_SIZE);
    munmap(heap_to, AREA_SIZE);

    heap_from = NULL;
    heap_to = NULL;
    heap_free = NULL;
}

int is_pointer_to(void *ptr, void *heap) {
    return (heap <= ptr && ptr < heap + AREA_SIZE);
}

size_t calculate_size(void *obj) {
    switch (TLISP_TYPE(obj)) {
    case TLISP_NULL:
    case TLISP_NIL:
    case TLISP_BOOL:
    case TLISP_CHAR:
    case TLISP_INTEGER:
    case TLISP_FLOAT:
    case TLISP_STREAM:
        return sizeof(tLispObject);

    case T_STREAM:
        return sizeof(tStream) + sizeof(tByte) * STREAM_BUFFER_SIZE;
    }
}

void *gc_copy(void *obj) {
    //printf("[gc] start copying obj = %p\n", obj);

    if (!is_pointer_to(((tObjectHeader *)obj)->forwarding, heap_to)) {
        size_t size = calculate_size(obj);

        memcpy(heap_free, obj, size);
        ((tObjectHeader *)obj)->forwarding = heap_free;

        heap_free += size;
    }

    return ((tObjectHeader *)obj)->forwarding;
}

void gc_collect() {
    //printf("[gc] start collect garbages...\n");

    void *scan = heap_free = heap_to;

    tObjectHeader **roots[] = {
        (tObjectHeader **)&runtime.toplevel_obj,
        (tObjectHeader **)&runtime.stdin,
        (tObjectHeader **)&runtime.stdout,
    };
    for (int i = 0; i < 3; i++) {
        tObjectHeader **root = roots[i];
        if (*root != NULL) {
            *root = gc_copy(*root);
        }
    }

    while (scan != heap_free) {
        switch (TLISP_TYPE(scan)) {
        case TLISP_NULL:
        case TLISP_NIL:
        case TLISP_BOOL:
        case TLISP_CHAR:
        case TLISP_INTEGER:
        case TLISP_FLOAT:
            break;

        case TLISP_STREAM: {
                tLispObject *stream = (tLispObject *)scan;
                stream->o.stream = gc_copy(stream->o.stream);
                break;
            }

        case T_STREAM: {
                tStream *stream_obj = (tStream *)scan;
                break;
            }
        }

        scan += calculate_size(scan);
    }

    void *tmp = heap_from;
    heap_from = heap_to;
    heap_to = tmp;
}

void *gc_allocate(size_t size) {
    if (heap_free + size > heap_from + AREA_SIZE) {
        //printf("[gc] HEAP IS FULL!!!\n");
        gc_collect();

        if (heap_free + size > heap_from + AREA_SIZE) {
            //printf("[gc] HEAP IS FULL AFTER GC!!!\n");
            return NULL;
        }
    }

    //printf("[gc] %ld bytes allocated.\n", size);
    void *obj = heap_free;
    heap_free += size;
    return obj;
}

tLispObject *t_gc_allocate_nil();

tLispObject *t_gc_allocate_bool(int v) {
    size_t size = sizeof(tLispObject);
    tLispObject *obj = (tLispObject *)gc_allocate(size);

    obj->header.type = TLISP_BOOL;
    obj->o.primitive = v;
    return obj;
}

tLispObject *t_gc_allocate_char(tChar v);

tLispObject *t_gc_allocate_integer(tInt v) {
    size_t size = sizeof(tLispObject);
    tLispObject *obj = (tLispObject *)gc_allocate(size);

    obj->header.type = TLISP_INTEGER;
    obj->o.primitive = v;
    return obj;
}

tLispObject *t_gc_allocate_float(tFloat v);

tStream *t_gc_allocate_stream_obj() {
    size_t size = sizeof(tStream) + sizeof(tByte) * STREAM_BUFFER_SIZE;
    tStream *stream_obj = (tStream *)gc_allocate(size);
    memset(stream_obj, 0, size);

    stream_obj->header.type = T_STREAM;
    stream_obj->head = 0;
    stream_obj->tail = 0;

    return stream_obj;
}

tLispObject *t_gc_allocate_stream() {
    size_t size = sizeof(tLispObject);
    tLispObject *stream = (tLispObject *)gc_allocate(size);

    stream->header.type = TLISP_STREAM;
    stream->o.stream = t_gc_allocate_stream_obj();

    return stream;
}


#ifdef TANAKA_LISP_TEST
#include <assert.h>
#include <stdio.h>

void test_gc_all() {
    printf("test: gc -> ok\n");
}
#endif
