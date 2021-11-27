#include <stddef.h>

#include "tanaka_type.h"

#include "stream.h"
#include "read.h"

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
            break;

        default:
            return num;
        }
    }

    return num;
}


#ifdef TANAKA_LISP_TEST

#include <assert.h>
#include <stdio.h>

void test_read_all() {
    printf("test: read -> ok\n");
}

#endif
