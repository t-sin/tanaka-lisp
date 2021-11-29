#include <stdio.h>

#include "utf8.h"
#include "stream.h"
#include "string_repr.h"

int main(int argc, char **argv) {
    printf("running tanaka-lisp tests...\n");

    test_utf8_all();
    test_stream_all();
    test_string_repr_all();

    printf("all tests ok.\n");
    return 0;
}
