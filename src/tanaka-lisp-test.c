#include <stdio.h>

#include "utf8.h"
#include "binary_stream.h"
#include "stream.h"
#include "read.h"

int main(int argc, char **argv) {
    printf("running tanaka-lisp tests...\n");

    test_utf8_all();
    test_binary_stream_all();
    test_stream_all();
    test_read_all();

    printf("all tests ok.\n");
    return 0;
}
