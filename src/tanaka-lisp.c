#include <stdint.h>
#include <stdio.h>

char *tlisp_version = "0.0.0";

#define LINE_SIZE 512

typedef enum {
    BOOLE,
} tLispType;

typedef struct tLispObject_t {
    tLispType type;
    union {
        uint8_t bool;
    } o;
} tLispObject;

int tLisp_read(FILE *in, tLispObject *out_obj) {
    return 0;
}

int main(int argc, char **argv) {
    printf("tanaka-lisp v%s\n", tlisp_version);

    char linebuf[LINE_SIZE];

    while (1) {
        printf("tanaka-lisp> ");

        if (fgets(linebuf, LINE_SIZE, stdin) > 0) {
            printf("%s", linebuf);

        } else {
            printf("\nbye.\n");
            break;
        }
    }

    return 0;
}
