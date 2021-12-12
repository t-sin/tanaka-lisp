#include <assert.h>
#include <sys/mman.h>
#include <stdio.h>
#include <string.h>

#include "tanaka-lisp.h"
#include "garbage_collector.h"

static size_t heap_size = 0;
static size_t heap_area_size = 0;

static void *heap_from = NULL;
static void *heap_to = NULL;
static void *heap_free = NULL;

static void gc_setup(size_t size) {
    heap_size = size;
    heap_area_size = size / 2;

    int protect = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;

    heap_from = mmap(NULL, heap_area_size, protect, flags, -1, 0);
    heap_to = mmap(NULL, heap_area_size, protect, flags, -1, 0);
    heap_free = heap_from;

    assert(heap_from != NULL);
    assert(heap_to != NULL);
}

#define HEAP_SIZE (1024 * 10)

void t_gc_setup() {
    gc_setup(HEAP_SIZE);
}

void t_gc_terminate() {
    munmap(heap_from, heap_area_size);
    munmap(heap_to, heap_area_size);

    heap_size = 0;
    heap_area_size = 0;
    heap_from = NULL;
    heap_to = NULL;
    heap_free = NULL;
}

size_t calculate_struct_size(int type) {
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

    case TLISP_ARRAY:
        return sizeof(tArray);

    default:
        return 0;
    }
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
        return calculate_struct_size(TLISP_TYPE(obj));

    case TLISP_ARRAY: {
            tArray *array = (tArray *)obj;
            size_t struct_size = calculate_struct_size(TLISP_TYPE(obj));
            size_t elem_size = calculate_struct_size(array->u.header.elem_type);
            size_t num_elems = array->u.header.num_elems;
            return struct_size + elem_size * num_elems;
        }

    default:
        return 0;
    }
}

int is_pointer_to(void *ptr, void *heap) {
    return (heap <= ptr && ptr < heap + heap_area_size);
}

void *gc_copy(void *obj) {
#ifdef TANAKA_DEBUG
    printf("[gc] copy obj = %p\n", obj);
#endif

    if (!is_pointer_to(((tObject *)obj)->forwarding, heap_to)) {
        size_t size = calculate_size(obj);

        memcpy(heap_free, obj, size);
        ((tObject *)obj)->forwarding = heap_free;

        heap_free += size;
    }

    return ((tObject *)obj)->forwarding;
}

void gc_collect() {
#ifdef TANAKA_DEBUG
    printf("[gc] collecting garbages...\n");
#endif

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
        assert(scan - heap_to < heap_area_size);

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

        size_t size = calculate_size(scan);
        assert(size > 0);
        scan += size;
    }

    void *tmp = heap_from;
    heap_from = heap_to;
    heap_to = tmp;

#ifdef TANAKA_DEBUG
    printf("[gc] garbage collected.\n");
#endif
}

void *gc_allocate(size_t size) {
#ifdef TANAKA_DEBUG
    printf("[gc] allocating...\n");
#endif
    if (heap_free + size > heap_from + heap_area_size) {
#ifdef TANAKA_DEBUG
        printf("[gc] HEAP IS FULL!!!\n");
#endif
        gc_collect();

        if (heap_free + size > heap_from + heap_area_size) {
#ifdef TANAKA_DEBUG
            printf("[gc] HEAP IS FULL AFTER GC!!!\n");
#endif
            return NULL;
        }
    }

    void *obj = heap_free;
    heap_free += size;

#ifdef TANAKA_DEBUG
    printf("[gc] %ld bytes allocated.\n", size);
#endif

    return obj;
}

tPrimitive *t_gc_allocate_nil() {
    size_t size = calculate_struct_size(TLISP_NIL);
    tPrimitive *obj = (tPrimitive *)gc_allocate(size);

    obj->type = TLISP_NIL;
    return obj;
}

tPrimitive *t_gc_allocate_bool(int v) {
    size_t size = calculate_struct_size(TLISP_BOOL);
    tPrimitive *obj = (tPrimitive *)gc_allocate(size);

    obj->type = TLISP_BOOL;
    obj->u.primitive = v;
    return obj;
}

tPrimitive *t_gc_allocate_char(tChar v);

tPrimitive *t_gc_allocate_integer(tInt v) {
    size_t size = calculate_struct_size(TLISP_INTEGER);
    tPrimitive *obj = (tPrimitive *)gc_allocate(size);

    obj->type = TLISP_INTEGER;
    obj->u.primitive = v;
    return obj;
}

tPrimitive *t_gc_allocate_float(tFloat v);

tConsCell *t_gc_allocate_cons(tObject *car, tObject *cdr) {
    size_t size = calculate_struct_size(TLISP_CONS);
    tConsCell *cons = (tConsCell *)gc_allocate(size);

    cons->type = TLISP_CONS;
    cons->u.cell.car = car;
    cons->u.cell.cdr = cdr;

    return cons;
}

tStream *t_gc_allocate_stream() {
    size_t size = calculate_struct_size(TLISP_STREAM);
    tStream *stream = (tStream *)gc_allocate(size);
    memset(stream, 0, size);

    stream->type = TLISP_STREAM;
    stream->u.tap.head = 0;
    stream->u.tap.tail = 0;

    return stream;
}

tArray *t_gc_allocate_array(int type, size_t num) {
    size_t size = calculate_struct_size(TLISP_ARRAY);
    size += calculate_struct_size(type) * num;
    tArray *array = (tArray *)gc_allocate(size);
    memset(array, 0, size);

    array->type = TLISP_ARRAY;
    array->u.header.elem_type = type;
    array->u.header.num_elems = num;

    return array;
}

#ifdef TANAKA_LISP_TEST
#include <assert.h>
#include <stdio.h>

static void verify_gc_allocate(void *ptr, int nbytes, tByte *ibytes, size_t *epos, tByte *ebytes) {
    for (int i = 0; i < nbytes; i++) {
        tByte *byte = (tByte *)ptr;
        byte[i] = ibytes[i];
    }

    for (int i = 0; i < nbytes; i++) {
        tByte *byte = (tByte *)(ptr + sizeof(tByte) *i);
        assert(byte != NULL);
        assert((void *)byte - heap_from == epos[i]);
        assert(*byte == ebytes[i]);
    }
}

static void test_gc_allocate_1byte_from_4byte_heap() {
    size_t heap_size = sizeof(tByte) * 4;
    size_t input_size = sizeof(tByte);
    tByte input_bytes[] = {42};
    size_t expected_pos[] = {0};
    tByte expected_bytes[] = {42};

    gc_setup(heap_size);
    void *ptr = gc_allocate(input_size);

    verify_gc_allocate(ptr, 1, input_bytes, expected_pos, expected_bytes);

    t_gc_terminate();
}

static void test_gc_allocate_2byte_from_4byte_heap() {
    size_t heap_size = sizeof(tByte) * 4;
    size_t input_size = sizeof(tByte) * 2;
    tByte input_bytes[] = {42, 43};
    size_t expected_pos[] = {0, 1};
    tByte expected_bytes[] = {42, 43};

    gc_setup(heap_size);
    void *ptr = gc_allocate(input_size);

    verify_gc_allocate(ptr, 2, input_bytes, expected_pos, expected_bytes);

    t_gc_terminate();
}

static void test_gc_allocate_3byte_from_4byte_heap_fails() {
    size_t heap_size = sizeof(tByte) * 4;
    size_t input_size = sizeof(tByte) * 3;

    gc_setup(heap_size);
    void *ptr = gc_allocate(input_size);

    assert(ptr == NULL);

    t_gc_terminate();
}

void test_gc_all() {
    test_gc_allocate_1byte_from_4byte_heap();
    test_gc_allocate_2byte_from_4byte_heap();
    test_gc_allocate_3byte_from_4byte_heap_fails();

    printf("test: gc -> ok\n");
}
#endif
