CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -g -Isrc
LLVM_CFLAGS = $(shell llvm-config --cflags)
LLVM_LDFLAGS = $(shell llvm-config --ldflags --libs core analysis native)
SRC = src/main.c src/lexer.c src/str.c src/ast.c src/parser.c src/error.c \
      src/type.c src/scope.c src/checker.c src/codegen.c
TARGET = lux
RUNTIME = /tmp/lux_runtime.o

$(TARGET): $(SRC) src/*.h $(RUNTIME)
	$(CC) $(CFLAGS) $(LLVM_CFLAGS) -o $(TARGET) $(SRC) $(LLVM_LDFLAGS)

$(RUNTIME): src/lux_runtime.c
	$(CC) -Wall -Wextra -std=c11 -g -c -o $(RUNTIME) src/lux_runtime.c

clean:
	rm -f $(TARGET) output /tmp/lux_output.o $(RUNTIME)

test: $(TARGET)
	./$(TARGET) test/hello.lux --ast

.PHONY: clean test
