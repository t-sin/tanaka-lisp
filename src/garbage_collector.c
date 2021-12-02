#include <sys/mman.h>
#include <stdio.h>
#include <string.h>

#include "tanaka-lisp.h"
#include "garbage_collector.h"


#define HEAP_SIZE 1024 * 20

static tObject *heap_head = NULL;
static tObject *free_list = NULL;

void t_gc_setup() {
    int protect = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;
    heap_head = mmap(NULL, HEAP_SIZE, protect, flags, -1, 0);

    free_list = heap_head;
    free_list->type = TLISP_NULL;
    free_list->size = HEAP_SIZE - sizeof(size_t);
    free_list->o.next = NULL;
}

void t_gc_terminate() {
    munmap(heap_head, HEAP_SIZE);

    heap_head = NULL;
    free_list = NULL;
}

void gc_mark(tObject *obj) {
    if (!TLISP_MARKED(obj)) {
        TLISP_MARK(obj);

        switch (TLISP_TYPE(obj)) {
        case TLISP_STREAM:
            gc_mark(TLISP_OBJ(obj).o.stream);
            break;
        }
    }
}

void gc_mark_phase(tObject **roots, size_t len) {
    for (int i = 0; i < len; i++) {
        gc_mark(roots[i]);
    }
}

void gc_sweep_phase() {}

void gc_collect() {
    gc_mark_phase(NULL, 0);
    gc_sweep_phase();
}

void *gc_allocate_1(size_t size, tObject *free_list) {
    if (free_list == NULL) {
        return NULL;
    }

    if (free_list->size < size) {
        return gc_allocate_1(size, free_list->o.next);
    }

    // なんかfree_listをつくる処理
    tObject *allocated = free_list;
}

void *gc_allocate(size_t size, tObject *free_list) {
    void *chunk = gc_allocate_1(size, free_list);

    if (chunk != NULL) {
        return chunk;
    }

    gc_collect();
    return gc_allocate_1(size, free_list);
}

tLispObject *t_gc_allocate_nil();
tLispObject *t_gc_allocate_bool(int v);
tLispObject *t_gc_allocate_char(tChar v);
tLispObject *t_gc_allocate_integer(tInt v);
tLispObject *t_gc_allocate_float(tFloat v);

tObject *t_gc_allocate_stream_obj() {
    size_t size = sizeof(tObject) + sizeof(tByte) * STREAM_BUFFER_SIZE;
    tObject *stream_obj = gc_allocate(size, free_list);
    memset(stream_obj, 0, size);

    stream_obj->type = T_STREAM;
    stream_obj->size = size;
    stream_obj->o.stream.head = 0;
    stream_obj->o.stream.tail = 0;

    return stream_obj;
}

tObject *t_gc_allocate_stream() {
    size_t size = sizeof(tObject);
    tObject *stream = gc_allocate(size, free_list);

    stream->type = TLISP_STREAM;
    stream->size = size;
    stream->o.lisp_obj.o.stream = t_gc_allocate_stream_obj();

    return stream;
}


#ifdef TANAKA_LISP_TEST
#include <assert.h>
#include <stdio.h>

void test_gc_all() {
    printf("test: gc -> ok\n");
}
#endif
