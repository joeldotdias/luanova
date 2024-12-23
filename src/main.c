#include "ast.h"
#include "eval.h"
#include "parser.h"

int main() {
    /* char* path = "_lua/hello.lua"; */
    /* char* path = "_lua/table.lua"; */
    /* char* path = "_lua/new.lua"; */
    /* char* path = "_lua/suf.lua"; */
    /* char* path = "_lua/while.lua"; */
    char* path = "_lua/simplify.lua";
    Lexer* lexer = init_lexer(path);
    Parser* parser = init_parser(lexer);
    ASTNode* root = parse(parser);
    /* proc_ast(root); */
    /* ast_dump(root); */
    Eval* e = init_eval();
    eval_chunk(e, root);
    annihilate_eval(e);
    annihilate_parser(&parser);

    return 0;
}
