#include <ctype.h>
#include <stddef.h>

#include "tanaka_type.h"

#include "stream.h"
#include "string_repr.h"

static int read_sharp(tStream *in, tLispObject *out_obj) {
    int num = 0;
    tChar ch;

    if (t_stream_peek_char(in, &ch) <= 0) {
        return READ_FAILED;
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

    return num;
}

static int read_integer(tStream *in, tLispObject *out_obj) {
    int num = 0;
    tChar ch;

    tInt n = 0;

    while (1) {
        int ret = t_stream_peek_char(in, &ch);
        if (ret < 0 || !isdigit(ch)) {
            out_obj->type = INTEGER;
            out_obj->o.intn = n;

            return num;
        }

        t_stream_read_char(in, &ch);
        num++;

        n = n * 10 + (ch - '0');
    }
}

int tLisp_read(tStream *in, tLispObject *out_obj) {
    size_t num = 0;
    tChar ch;
    int ret;

    while (ret = t_stream_peek_char(in, &ch), ret > 0) {
        if (ch == '#') {
            t_stream_read_char(in, &ch);
            num++;

            ret = read_sharp(in, out_obj);
            if (ret <= 0) {
                return ret;
            }

        } else if (isdigit(ch)) {
            ret = read_integer(in, out_obj);
            if (ret <= 0) {
                return ret;
            }
            num += ret;

        } else {
            return num;
        }
    }

    return num;
}

#define PRINT_BUFFER_SIZE 1024

static void print_integer(tStream *out, tLispObject *obj) {
    int n = obj->o.intn;
    int l = 0;
    tByte buf[PRINT_BUFFER_SIZE];

    for (; n > 0; n = n / 10) {
        int d = n % 10;
        buf[l++] = '0' + d;
    }

    for (int i = l - 1; i >= 0; i--) {
        t_stream_write_byte(out, buf[i]);
    }
}

void tLisp_print(tStream *out, tLispObject *obj) {
    switch (obj->type) {
    case BOOLE:
        t_stream_write_byte(out, '#');
        t_stream_write_byte(out, obj->o.bool == 0 ? 'f' : 't');
        break;

    case INTEGER:
        print_integer(out, obj);
        break;

    default:
        break;
    }
}

#ifdef TANAKA_LISP_TEST

#include <assert.h>
#include <stdio.h>

void test_string_repr_all() {
    printf("test: read -> ok\n");
}

#endif
