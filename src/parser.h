#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

#define FAILED_EXPECTATION(expected)                                                     \
    FATAL("EXPECTED %s | RECEIVED %s", expected, token_to_str(parser->curr_token))

typedef struct {
    Scope* curr_scope;
    Scope* global;
} ScopeTracker;

typedef struct {
    Lexer* lexer;
    Token* curr_token;
    Token* peeked_token;
    ScopeTracker* scope_tracker;
} Parser;

Parser* init_parser(Lexer* lexer);
void annihilate_parser(Parser** parser);
ASTNode* parse(Parser* parser);

#endif
