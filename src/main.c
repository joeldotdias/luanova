#include "ast.h"
#include "parser.h"

int main() {
    /* char* path = "_lua/hello.lua"; */
    /* char* path = "_lua/table.lua"; */
    char* path = "_lua/suf.lua";
    Lexer* lexer = init_lexer(path);
    Parser* parser = init_parser(lexer);
    ASTNode* root = parse(parser);
    ast_dump(root);
    annihilate_parser(&parser);

    return 0;
}
