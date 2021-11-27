#include <stdio.h>

#include "utf8.h"

int main(int argc, char **argv) {
    printf("running tanaka-lisp tests...\n");

    test_utf8_all();

    printf("all tests ok.\n");
    return 0;
}
