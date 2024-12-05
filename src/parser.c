#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "parser.h"
#include "shared.h"

static ASTNode* parse_local_assignment(Parser* parser);
static ASTNode* parse_return(Parser* parser);
static ASTNode* parse_expr(Parser* parser);
static ASTNode* parse_func_call(Parser* parser);
static ASTNode** parse_func_args(Parser* parser);
static ASTNode* parse_var_node(Parser* parser);
static Token* consume_token(Parser* parser, TokenKind expected);
static bool expect_token(Parser* parser, TokenKind expected);
static void advance_parser(Parser* parser);
static ASTNode* make_node(NodeKind kind);

Parser* init_parser(Lexer* lexer) {
    size_t len = sizeof(Parser);
    Parser* parser = malloc(len);
    memset(parser, 0, len);

    parser->lexer = lexer;

    parser->curr_token = next_token(lexer);
    parser->peeked_token = next_token(lexer);

    return parser;
}

ASTNode* parse(Parser* parser) {
    ASTNode* chunk = calloc(1, sizeof(ASTNode));
    chunk->kind = ASTNODE_CHUNK;
    // remove hardcoded 5
    chunk->chunk.stmteez = calloc(5, sizeof(ASTNode*));
    chunk->chunk.stmt_count = 0;

    while(parser->curr_token->kind != TOKEN_EOF) {
        if(parser->curr_token->kind == TOKEN_LOCAL) {
            ASTNode* assgmt = parse_local_assignment(parser);
            chunk->chunk.stmteez[chunk->chunk.stmt_count++] = assgmt;
        } else if(parser->curr_token->kind == TOKEN_RETURN) {
            ASTNode* ret_stmt = parse_return(parser);
            chunk->chunk.stmteez[chunk->chunk.stmt_count++] = ret_stmt;
        } else {
            ASTNode* expr = parse_expr(parser);
            chunk->chunk.stmteez[chunk->chunk.stmt_count++] = expr;
        }

        advance_parser(parser);
    }

    print_ast(chunk);

    return chunk;
}

static ASTNode* parse_local_assignment(Parser* parser) {
    if(!consume_token(parser, TOKEN_LOCAL)) {
        /* LOG_FATAL("EXPECTED LOCAL | RECEIVED %s", token_to_str(parser->curr_token)); */
        FAILED_EXPECTATION("LOCAL");
    }

    ASTNode* node = calloc(1, sizeof(ASTNode));
    node->kind = ASTNODE_LOCAL_VAR_DECLR;

    Token* ident = consume_token(parser, TOKEN_IDENT);
    if(!ident) {
        /* LOG_FATAL("EXPECTED IDENT | RECEIVED %s", token_to_str(parser->curr_token)); */
        FAILED_EXPECTATION("IDENT");
    }

    ASTNode* var = make_node(ASTNODE_VAR);
    var->var_node.name = strdup(ident->value);
    var->var_node.is_local = true;
    node->assignment.var_list = var;

    if(!consume_token(parser, TOKEN_ASSIGN)) {
        FAILED_EXPECTATION("ASSIGN");
    }

    ASTNode* literal = calloc(1, sizeof(ASTNode));
    literal->kind = ASTNODE_STR_LITERAL;

    literal->str_literal.str_val = strdup(parser->curr_token->value);
    node->assignment.expr_list = literal;

    return node;
}

static ASTNode* parse_return(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_RETURN_STMT);
    return node;
}
static ASTNode* parse_expr(Parser* parser) {
    ASTNode* node = NULL;

    if(parser->curr_token->kind == TOKEN_IDENT) {
        if(!consume_token(parser, TOKEN_LPAREN)) {
            node = parse_func_call(parser);
        } else {
            // other shit
            node = parse_var_node(parser);
        }
    }
    return node;
}

static ASTNode* parse_func_call(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_FUNC_CALL_STMT);
    node->func_call.prefix = NULL;
    node->func_call.name = strdup(parser->curr_token->value);
    advance_parser(parser);
    advance_parser(parser);

    ASTNode** args = parse_func_args(parser);
    node->func_call.args = args;

    /* LOG_INFO("IN func call parse %s", token_to_str(parser->curr_token)); */
    if(parser->curr_token->kind != TOKEN_RPAREN) {
        LOG_FATAL("EXPECTED RPAREN | RECEIVED %s", token_to_str(parser->curr_token));
    }

    return node;
}

static ASTNode** parse_func_args(Parser* parser) {
    size_t count = 0;
    ASTNode** args = calloc(2, sizeof(ASTNode*));
    ASTNode* var = NULL;
    var = parse_var_node(parser);
    args[count++] = var;

    if(expect_token(parser, TOKEN_COMMA)) {
        advance_parser(parser);
        var = NULL;
        var = parse_var_node(parser);
    }

    advance_parser(parser);

    return args;
}

static void advance_parser(Parser* parser) {
    Token* consumed_token = parser->curr_token;
    parser->curr_token = parser->peeked_token;
    annihilate_token(&consumed_token);
    parser->peeked_token = next_token(parser->lexer);
}

static ASTNode* parse_var_node(Parser* parser) {
    ASTNode* var = make_node(ASTNODE_VAR);
    var->var_node.name = strdup(parser->curr_token->value);

    return var;
}

static Token* consume_token(Parser* parser, TokenKind expected) {
    Token* curr_token = parser->curr_token;
    if(curr_token->kind != expected) {
        return NULL;
    }

    advance_parser(parser);
    return curr_token;
}

static bool expect_token(Parser* parser, TokenKind expected) {
    return parser->curr_token && parser->peeked_token->kind == expected;
}

void annihilate_parser(Parser** parser) {
    annihilate_token(&(*parser)->curr_token);
    annihilate_token(&(*parser)->peeked_token);

    annihilate_lexer(&(*parser)->lexer);

    free(*parser);
    *parser = NULL;
}

static ASTNode* make_node(NodeKind kind) {
    ASTNode* node = calloc(1, sizeof(ASTNode));
    node->kind = kind;
    return node;
}
