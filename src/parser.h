#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

#define FAILED_EXPECTATION(expected)                                                     \
    LOG_FATAL("EXPECTED %s | RECEIVED %s", expected, token_to_str(parser->curr_token))

typedef struct {
    Lexer* lexer;
    Token* curr_token;
    Token* peeked_token;
} Parser;

Parser* init_parser(Lexer* lexer);
void annihilate_parser(Parser** parser);
ASTNode* parse(Parser* parser);

#endif
