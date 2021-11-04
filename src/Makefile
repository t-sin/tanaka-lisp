PROGRAM = tanaka-lisp
TEST_PROGRAM = tanaka-lisp.test
OBJECTS = tanaka-lisp.o

$(PROGRAM): $(OBJECTS)
	gcc $(CFLAGS) $^ -o $@

$(TEST_PROGRAM): $(OBJECTS)
	gcc $(CFLAGS) $^ -o $@

.c.o:
	gcc $(CFLAGS) -c $<

.PHONY: all
all: $(PROGRAM)

.PHONY: clean
clean:
	@rm *.o
	@rm $(PROGRAM)
	@rm $(TEST_PROGRAM)
