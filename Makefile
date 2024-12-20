COMPILER = clang
CFLAGS = -Wall -Wextra -g -lm
SOURCES = $(shell find src -name "*.c" -not -name "proc_ast.c")
TARGET = luanova

$(TARGET): $(SOURCES)
	@rm -f ./luanova
	@$(COMPILER) $(SOURCES) -o $(TARGET) $(CFLAGS)

anz:
	@clang $(CFLAGS) --analyze -Xclang -analyzer-output=text $(SOURCES)

clean:
	@rm -f $(TARGET)
