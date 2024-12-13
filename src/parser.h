#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

#define CURR_TOKEN_IS(expected) (parser->curr_token->kind == (expected))

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

typedef struct {
    int left_bind;
    int right_bind;
} Precedence;

Parser* init_parser(Lexer* lexer);
void annihilate_parser(Parser** parser);
ASTNode* parse(Parser* parser);

#endif
