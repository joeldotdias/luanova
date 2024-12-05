#include "parser.h"

int main() {
    /* char* path = "example.lua"; */
    char* path = "_lua/hello.lua";
    Lexer* lexer = init_lexer(path);
    Parser* parser = init_parser(lexer);
    parse(parser);
    annihilate_parser(&parser);
}
