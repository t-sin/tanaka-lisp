#include <stdio.h>

#include "tanaka-lisp.h"

#include "utf8.h"
#include "stream.h"
#include "string_repr.h"
#include "garbage_collector.h"

tLispRuntime runtime = {NULL, NULL, NULL};

int main(int argc, char **argv) {
    printf("running tanaka-lisp tests...\n");

    test_utf8_all();
    test_stream_all();
    test_string_repr_all();
    test_gc_all();

    printf("all tests ok.\n");
    return 0;
}
