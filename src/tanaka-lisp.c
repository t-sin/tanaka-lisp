#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "binary_stream.h"
#include "stream.h"
#include "tanaka_type.h"

char *tlisp_version = "0.0.0";

#define READ_NO_INPUT 0
#define READ_MORE -1
#define READ_FAILED -2

int tLisp_read(tStream *in, tLispObject *out_obj) {
    size_t num = 0;
    tChar ch;
    int ret;

    while (ret = t_stream_peek_char(in, &ch), ret > 0) {
        switch (ch) {
        case '#':
            t_stream_read_char(in, &ch);
            num++;

            if (t_stream_peek_char(in, &ch) <= 0) {
                return READ_MORE;
            }
            t_stream_read_char(in, &ch);
            num++;

            switch (ch) {
            case 'f':
                out_obj->type = BOOLE;
                out_obj->o.bool = 0;
                break;
            case 't':
                out_obj->type = BOOLE;
                out_obj->o.bool = 1;
                break;
            default:
                return READ_FAILED;
            }
            break;

        default:
            return num;
        }
    }

    return num;
}

int tLisp_eval(tLispObject *obj, tLispObject *out_obj) {
    *out_obj = *obj;

    return 0;
}

void tLisp_print(tStream *out, tLispObject *obj) {
    switch (obj->type) {
    case BOOLE:
        t_stream_write_byte(out, '#');
        t_stream_write_byte(out, obj->o.bool == 0 ? 'f' : 't');
        break;

    default:
        break;
    }
}

#define LINE_SIZE 512

int main(int argc, char **argv) {
    printf("tanaka-lisp v%s\n", tlisp_version);

    char linebuf[LINE_SIZE];
    memset(linebuf, 0, LINE_SIZE);

    tStream *stream_stdin = make_stream(make_binary_stream());
    tStream *stream_stdout = make_stream(make_binary_stream());
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

    return 0;
}
