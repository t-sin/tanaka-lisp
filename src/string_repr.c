#include <assert.h>
#include <ctype.h>
#include <stddef.h>

#include "tanaka-lisp.h"
#include "garbage_collector.h"

#include "stream.h"
#include "string_repr.h"

static int skip_spaces(tStream *in) {
    tChar ch;
    int num = 0;

    while (1) {
        int ret = t_stream_peek_char(in, &ch);
        if (ret <= 0) {
            return ret;

        } else if (ch != ' ') {
            return num;
        }

        t_stream_read_char(in, &ch);
        num++;
    }
}

static int consume_char(tStream *in, tChar c) {
    tChar ch;
    int ret;

    ret = t_stream_peek_char(in, &ch);
    if (ret <= 0) {
        return ret;

    } else if (ch != c) {
        return READ_FAILED;
    }

    t_stream_read_char(in, &ch);

    return ret;
}

static int read_sharp(tStream *in, tObject **out_obj) {
    int num = 0;
    tChar ch;

    if (t_stream_peek_char(in, &ch) <= 0) {
        return READ_FAILED;
    }
    t_stream_read_char(in, &ch);
    num++;

    switch (ch) {
    case 'f':
        *out_obj = (tObject *)t_gc_allocate_bool(0);
        break;

    case 't':
        *out_obj = (tObject *)t_gc_allocate_bool(1);
        break;

    default:
        return READ_FAILED;
    }

    return num;
}

static int read_integer(tStream *in, tObject **out_obj) {
    int num = 0;
    tChar ch;
    tInt n = 0;

    while (1) {
        int ret = t_stream_peek_char(in, &ch);
        if (ret < 0) {
            return READ_FAILED;

        } else if (ret == 0 || ch == ' ' || ch == ')') {
            *out_obj = (tObject *)t_gc_allocate_integer(n);
            return num;

        } else if (!isdigit(ch)) {
            return READ_FAILED;
        }

        t_stream_read_char(in, &ch);
        num++;

        n = n * 10 + (ch - '0');
    }
}

static int read_paren(tStream *in, tObject **out_obj) {
    size_t num = 0;
    tChar ch;
    int ret;

    ret = t_stream_peek_char(in, &ch);
    if (ret <= 0) {
        return ret;
    } else if (ch == ')') {
        t_stream_read_char(in, &ch);
        num++;

        tPrimitive *nil = t_gc_allocate_nil();
        *out_obj = (tObject *)nil;

        return num;
    }

    tObject *car;
    ret = tLisp_read(in, &car);
    if (ret <= 0) {
        return ret;
    }
    num += ret;

    ret = skip_spaces(in);
    if (ret <= 0) {
        return ret;
    }

    ret = t_stream_peek_char(in, &ch);
    if (ret <= 0) {
        return ret;
    } else if (ch == '.') {
        t_stream_read_char(in, &ch);
        num++;

        tObject *cdr;
        ret = tLisp_read(in, &cdr);
        if (ret <= 0) {
            return ret;
        }
        num += ret;

        tConsCell *cons = t_gc_allocate_cons(car, cdr);
        *out_obj = (tObject *)cons;

        ret = consume_char(in, ')');
        if (ret <= 0) {
            return ret;
        }
        num++;

        return num;

    } else {
        tConsCell *cons = t_gc_allocate_cons(car, (tObject *)t_gc_allocate_nil());
        tObject **tail = &(cons->u.cell.cdr);

        while (1) {
            ret = t_stream_peek_char(in, &ch);
            if (ret <= 0) {
                return ret;

            } else if (ch == ')') {
                t_stream_read_char(in, &ch);
                num++;

                *out_obj = (tObject *)cons;
                return num;
            }

            tObject *elem;
            ret = tLisp_read(in, &elem);
            if (ret <= 0) {
                return ret;
            }
            num += ret;

            tConsCell *cons = t_gc_allocate_cons(elem, (tObject *)t_gc_allocate_nil());
            *tail = (tObject *)cons;
            tail = &(cons->u.cell.cdr);
        }
    }
}

int tLisp_read(tStream *in, tObject **out_obj) {
    size_t num = 0;
    tChar ch;
    int ret;

    ret = skip_spaces(in);
    if (ret < 0) {
        return READ_FAILED;
    }

    while (ret = t_stream_peek_char(in, &ch), ret > 0) {
        if (ch == '#') {
            t_stream_read_char(in, &ch);
            num++;

            ret = read_sharp(in, out_obj);
            if (ret <= 0) {
                return ret;
            }

            break;

        } else if (ch == '(') {
            t_stream_read_char(in, &ch);
            num++;

            ret = read_paren(in, out_obj);
            if (ret <= 0) {
                return ret;
            }

            num += ret;

            break;

        } else if (isdigit(ch)) {
            ret = read_integer(in, out_obj);
            if (ret <= 0) {
                return ret;
            }
            num += ret;

            break;

        } else if (ch == '-') {
            t_stream_read_char(in, &ch);
            num++;

            ret = t_stream_peek_char(in, &ch);
            if (ret <= 0 || !isdigit(ch)) {
                return READ_FAILED;
            }

            ret = read_integer(in, out_obj);
            if (ret <= 0) {
                return READ_FAILED;
            }

            num += ret;
            tPrimitive **tmp = (tPrimitive **)out_obj;
            (*tmp)->u.primitive = -(*tmp)->u.primitive;

            break;

        } else {
            return num;
        }
    }

    return num;
}

#define PRINT_BUFFER_SIZE 1024

static void print_integer(tStream *out, tPrimitive *obj) {
    tInt n = obj->u.primitive;

    if (n == 0) {
        t_stream_write_byte(out, '0');
        return;
    }

    if (n < 0) {
        t_stream_write_byte(out, '-');
        n = -n;
    }

    int l = 0;
    tByte buf[PRINT_BUFFER_SIZE];

    for (; n > 0; n = n / 10) {
        int d = n % 10;
        buf[l++] = '0' + d;
    }

    for (int i = l - 1; i >= 0; i--) {
        t_stream_write_char(out, buf[i]);
    }
}

static void print_cons(tStream *out, tObject *obj) {
    tConsCell *cons = (tConsCell *)obj;

    t_stream_write_char(out, '(');
    tLisp_print(out, cons->u.cell.car);
    t_stream_write_char(out, ' ');
    t_stream_write_char(out, '.');
    t_stream_write_char(out, ' ');
    tLisp_print(out, cons->u.cell.cdr);
    t_stream_write_char(out, ')');
}

void tLisp_print(tStream *out, tObject *obj) {
    assert(obj != NULL);

    switch (TLISP_TYPE(obj)) {
    case TLISP_NIL:
        t_stream_write_char(out, '(');
        t_stream_write_char(out, ')');
        break;

    case TLISP_BOOL: {
            tPrimitive *o = (tPrimitive *)obj;
            t_stream_write_char(out, '#');
            t_stream_write_char(out, o->u.primitive == 0 ? 'f' : 't');
            break;
        }

    case TLISP_INTEGER:
        print_integer(out, (tPrimitive *)obj);
        break;

    case TLISP_CONS:
        print_cons(out, obj);
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
