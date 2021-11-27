#include <stdio.h>

#include "utf8.h"
#include "binary_stream.h"

int main(int argc, char **argv) {
    printf("running tanaka-lisp tests...\n");

    test_utf8_all();
    test_binary_stream_all();

    printf("all tests ok.\n");
    return 0;
}
