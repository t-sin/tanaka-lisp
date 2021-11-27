PROGRAM = tanaka-lisp
PROGRAM_OBJECT = tanaka-lisp.o
OBJECTS = utf8.o binary_stream.o stream.o string_repr.o

TEST_PROGRAM = tanaka-lisp-test
TEST_OBJECT = tanaka-lisp-test.o

$(PROGRAM): $(PROGRAM_OBJECT) $(OBJECTS)
	gcc $(CFLAGS) $^ -o $@

$(TEST_PROGRAM): CFLAGS += -D TANAKA_LISP_TEST -g -O0 -fsanitize=address
$(TEST_PROGRAM): $(TEST_OBJECT) $(OBJECTS)
	gcc $(CFLAGS) $^ -o $@

.c.o:
	gcc $(CFLAGS) -c $<

.PHONY: all
all: $(PROGRAM)

.PHONY: test
test: $(TEST_PROGRAM)
	./$(TEST_PROGRAM)

.PHONY: clean
clean:
	@rm *.o
	@rm $(PROGRAM)
	@rm $(TEST_PROGRAM)
