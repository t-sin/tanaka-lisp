PROGRAM = tanaka-lisp
PROGRAM_OBJECT = tanaka-lisp.o
OBJECTS = utf8.o stream.o string_repr.o garbage_collector.o

TEST_PROGRAM = tanaka-lisp-test
TEST_OBJECT = tanaka-lisp-test.o

$(PROGRAM): $(PROGRAM_OBJECT) $(OBJECTS)
	gcc $(CFLAGS) $^ -o $@

$(TEST_PROGRAM): CFLAGS += -D TANAKA_LISP_TEST -g -O0 -fsanitize=address
$(TEST_PROGRAM): $(TEST_OBJECT) $(OBJECTS)
	gcc $(CFLAGS) $^ -o $@

.c.o:
	gcc $(CFLAGS) -c $<

.PHONY: build
build: clean $(PROGRAM)

.PHONY: dbuild
dbuild: CFLAGS += -D TANAKA_DEBUG -g -O0 -fsanitize=address
dbuild: build

.PHONY: test
test: clean $(TEST_PROGRAM)
	./$(TEST_PROGRAM)

.PHONY: clean
clean:
	@rm -f *.o
	@rm -f $(PROGRAM)
	@rm -f $(TEST_PROGRAM)
