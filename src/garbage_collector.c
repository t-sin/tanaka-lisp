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

size_t calculate_size(int type) {
    switch (type) {
    case TLISP_NULL:
    case TLISP_NIL:
    case TLISP_BOOL:
    case TLISP_CHAR:
    case TLISP_INTEGER:
    case TLISP_FLOAT:
        return sizeof(tPrimitive);

    case TLISP_STREAM:
        return sizeof(tStream) + sizeof(tByte) * STREAM_BUFFER_SIZE;

    case TLISP_CONS:
        return sizeof(tConsCell);

    default:
        return 0;
    }
}

void *gc_copy(void *obj) {
    //printf("[gc] start copying obj = %p\n", obj);

    if (!is_pointer_to(((tObject *)obj)->forwarding, heap_to)) {
        size_t size = calculate_size(TLISP_TYPE(obj));

        memcpy(heap_free, obj, size);
        ((tObject *)obj)->forwarding = heap_free;

        heap_free += size;
    }

    return ((tObject *)obj)->forwarding;
}

void gc_collect() {
    //printf("[gc] start collect garbages...\n");

    void *scan = heap_free = heap_to;

    tObject **roots[] = {
        (tObject **)&runtime.toplevel_obj,
        (tObject **)&runtime.stdin,
        (tObject **)&runtime.stdout,
    };
    for (int i = 0; i < 3; i++) {
        tObject **root = roots[i];
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
                tStream *stream_obj = (tStream *)scan;
                break;
            }

        case TLISP_CONS: {
                tConsCell *cons = (tConsCell *)scan;
                cons->u.cell.car = gc_copy(cons->u.cell.car);
                cons->u.cell.cdr = gc_copy(cons->u.cell.cdr);
                break;
            }
        }

        scan += calculate_size(TLISP_TYPE(scan));
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

tPrimitive *t_gc_allocate_nil();

tPrimitive *t_gc_allocate_bool(int v) {
    size_t size = calculate_size(TLISP_BOOL);
    tPrimitive *obj = (tPrimitive *)gc_allocate(size);

    obj->type = TLISP_BOOL;
    obj->u.primitive = v;
    return obj;
}

tPrimitive *t_gc_allocate_char(tChar v);

tPrimitive *t_gc_allocate_integer(tInt v) {
    size_t size = calculate_size(TLISP_INTEGER);
    tPrimitive *obj = (tPrimitive *)gc_allocate(size);

    obj->type = TLISP_INTEGER;
    obj->u.primitive = v;
    return obj;
}

tPrimitive *t_gc_allocate_float(tFloat v);

tConsCell *t_gc_allocate_cons(tObject *car, tObject *cdr) {
    size_t size = calculate_size(TLISP_CONS);
    tConsCell *cons = (tConsCell *)gc_allocate(size);

    cons->type = TLISP_CONS;
    cons->u.cell.car = car;
    cons->u.cell.cdr = cdr;

    return cons;
}

tStream *t_gc_allocate_stream() {
    size_t size = calculate_size(TLISP_STREAM);
    tStream *stream = (tStream *)gc_allocate(size);
    memset(stream, 0, size);

    stream->type = TLISP_STREAM;
    stream->u.tap.head = 0;
    stream->u.tap.tail = 0;

    return stream;
}

#ifdef TANAKA_LISP_TEST
#include <assert.h>
#include <stdio.h>

void test_gc_all() {
    printf("test: gc -> ok\n");
}
#endif
