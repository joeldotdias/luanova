COMPILER = clang
CFLAGS = -Wall -Wextra -g
SOURCES = $(shell find src -name "*.c" -not -name "parserbak.c")
TARGET = luanova

$(TARGET): $(SOURCES)
	@$(COMPILER) $(SOURCES) -o $(TARGET)

anz:
	@clang $(CFLAGS) --analyze -Xclang -analyzer-output=text $(SOURCES)

clean:
	@rm -f $(TARGET)
