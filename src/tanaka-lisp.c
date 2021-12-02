#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "tanaka-lisp.h"
#include "garbage_collector.h"

#include "stream.h"
#include "string_repr.h"

char *tlisp_version = "0.0.0";

int tLisp_eval(tLispObject *obj, tLispObject *out_obj) {
    *out_obj = *obj;

    return 0;
}

#define LINE_SIZE 512

int main(int argc, char **argv) {
    printf("tanaka-lisp v%s\n", tlisp_version);

    char linebuf[LINE_SIZE];
    memset(linebuf, 0, LINE_SIZE);

    t_gc_setup();

    tStream *stream_stdin = &t_gc_allocate_stream_obj()->o.stream;
    tStream *stream_stdout = &t_gc_allocate_stream_obj()->o.stream;
    int more_input_needed = 0;

    while (1) {
        if (more_input_needed) {
            printf("...        > ");
            more_input_needed = 0;
        } else {
            printf("tanaka-lisp> ");
        }

        if (fgets(linebuf, LINE_SIZE, stdin) > 0) {
            t_stream_clear(stream_stdin);
            for (int i = 0; linebuf[i] != '\n'; i++) {
                t_stream_write_byte(stream_stdin, linebuf[i]);
            }

            tLispObject obj;
            int ret = tLisp_read(stream_stdin, &obj);

            if (ret == READ_MORE) {
                more_input_needed = 1;
                continue;
            } else if (ret <= 0) {
                continue;
            }

            tLispObject eobj;
            tLisp_eval(&obj, &eobj);

            tLisp_print(stream_stdout, &eobj);

            printf("=> ");
            tByte b;
            while (t_stream_read_byte(stream_stdout, &b) > 0) {
                fputc(b, stdout);
            }
            puts("");

        } else {
            printf("\nbye.\n");
            break;
        }
    }

    t_gc_terminate();

    return 0;
}
