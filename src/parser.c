#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "shared.h"

// fail represents the token at which we stop processing the chunk
static ASTNode* parse_chunk(Parser* parser, TokenKind fail);
static ASTNode* parse_local_assignment(Parser* parser);
static ASTNode* parse_local_function(Parser* parser);
static Symbol** parse_func_params(Parser* parser);
static ASTNode* parse_return(Parser* parser);
static ASTNode* parse_expr(Parser* parser);
static ASTNode* parse_func_call(Parser* parser);
static ASTNode** parse_func_args(Parser* parser);
static ASTNode* parse_symbol(Parser* parser);

static Scope* enter_scope(ScopeTracker* tracker, ASTNode* curr_function);
static void exit_scope(ScopeTracker* tracker);
static Scope* create_scope(Scope* parent, ASTNode* curr_function);
static void add_symbol_to_scope(Scope* scope, Symbol* symbol);
static Symbol* resolve_symbol(Scope* curr_scope, const char* name);
static Symbol* look_for_symbol(Symbol** symbol_lookup, const char* name);

static ASTNode* make_node(NodeKind kind);
static Token* consume_token(Parser* parser, TokenKind expected);
static bool expect_token(Parser* parser, TokenKind expected);
static void advance_parser(Parser* parser);

Parser* init_parser(Lexer* lexer) {
    size_t len = sizeof(Parser);
    Parser* parser = malloc(len);
    memset(parser, 0, len);

    parser->lexer = lexer;

    ScopeTracker* scope_tracker = calloc(1, sizeof(ScopeTracker));
    Scope* global_scope = calloc(1, sizeof(Scope));
    global_scope->parent = NULL;
    global_scope->function = NULL;
    global_scope->must_be_closed = false;
    global_scope->name = "MAIN";
    global_scope->symbol_count = 0;
    global_scope->lookup_capacity = 10;
    global_scope->symbol_lookup = calloc(global_scope->lookup_capacity, sizeof(Symbol));
    scope_tracker->curr_scope = global_scope;
    scope_tracker->global = global_scope;
    parser->scope_tracker = scope_tracker;

    parser->curr_token = next_token(lexer);
    parser->peeked_token = next_token(lexer);

    return parser;
}

ASTNode* parse(Parser* parser) {
    ASTNode* chunk = parse_chunk(parser, TOKEN_EOF);
    ast_dump(chunk);

    return chunk;
}

static ASTNode* parse_chunk(Parser* parser, TokenKind fail) {
    ASTNode* node = calloc(1, sizeof(ASTNode));
    node->kind = ASTNODE_CHUNK;

    Chunk* chunk = calloc(1, sizeof(Chunk));
    chunk->stmteez = calloc(10, sizeof(ASTNode*));
    chunk->stmt_count = 0;

    while(parser->curr_token->kind != TOKEN_EOF && parser->curr_token->kind != fail) {
        if(parser->curr_token->kind == TOKEN_LOCAL) {
            ASTNode* assgmt = parse_local_assignment(parser);
            chunk->stmteez[chunk->stmt_count++] = assgmt;
        } else if(parser->curr_token->kind == TOKEN_RETURN) {
            ASTNode* ret_stmt = parse_return(parser);
            chunk->stmteez[chunk->stmt_count++] = ret_stmt;
        } else {
            ASTNode* expr = parse_expr(parser);
            chunk->stmteez[chunk->stmt_count++] = expr;
        }
        advance_parser(parser);
    }
    node->chunk = *chunk;

    return node;
}

static ASTNode* parse_local_assignment(Parser* parser) {
    if(!consume_token(parser, TOKEN_LOCAL)) {
        FAILED_EXPECTATION("LOCAL");
    }

    if(consume_token(parser, TOKEN_FUNCTION)) {
        return parse_local_function(parser);
    }

    ASTNode* node = calloc(1, sizeof(ASTNode));
    node->kind = ASTNODE_LOCAL_VAR_DECLR;

    Token* ident = consume_token(parser, TOKEN_IDENT);
    if(!ident) {
        FAILED_EXPECTATION("IDENT");
    }

    ASTNode* symbol = make_node(ASTNODE_SYMBOL);
    symbol->symbol.name = strdup(ident->value);
    symbol->symbol.is_local = true;
    symbol->symbol.scope = parser->scope_tracker->curr_scope;
    add_symbol_to_scope(parser->scope_tracker->curr_scope, &symbol->symbol);
    node->assignment.var_list = symbol;

    if(!consume_token(parser, TOKEN_ASSIGN)) {
        FAILED_EXPECTATION("ASSIGN");
    }

    ASTNode* literal = calloc(1, sizeof(ASTNode));
    literal->kind = ASTNODE_STR_LITERAL;

    literal->str_literal.str_val = strdup(parser->curr_token->value);
    node->assignment.expr_list = literal;

    return node;
}

static ASTNode* parse_local_function(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_FUNC_EXPR);
    FuncExpr* func = calloc(1, sizeof(FuncExpr));
    Symbol* func_name = &parse_symbol(parser)->symbol;
    func_name->scope = parser->scope_tracker->curr_scope;
    func->name = func_name;
    func->scope = func_name->scope; // change this stupidity
    advance_parser(parser);

    Scope* curr_scope = enter_scope(parser->scope_tracker, node);
    curr_scope->name = func_name->name;
    if(!consume_token(parser, TOKEN_LPAREN)) {
        FAILED_EXPECTATION("LPAREN");
    }

    if(!consume_token(parser, TOKEN_RPAREN)) {
        func->params = parse_func_params(parser);
    }

    ASTNode* chunk = parse_chunk(parser, TOKEN_END);
    func->body = chunk;
    advance_parser(parser);

    node->func_decl = *func;
    exit_scope(parser->scope_tracker);

    return node;
}

static Symbol** parse_func_params(Parser* parser) {
    Symbol** params = calloc(2, sizeof(Symbol));
    size_t count = 0;
    while(!expect_token(parser, TOKEN_RPAREN)) {
        Token* ident = consume_token(parser, TOKEN_IDENT);
        if(ident) {
            params[count++] = &parse_symbol(parser)->symbol;
        }
    }
    return params;
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
            node = parse_symbol(parser);
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
    var = parse_symbol(parser);
    args[count++] = var;

    if(expect_token(parser, TOKEN_COMMA)) {
        advance_parser(parser);
        var = NULL;
        var = parse_symbol(parser);
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

static ASTNode* parse_symbol(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_SYMBOL);
    Symbol* symbol = calloc(1, sizeof(Symbol));
    symbol->name = strdup(parser->curr_token->value);
    node->symbol = *symbol;

    return node;
}

static Symbol* resolve_symbol(Scope* curr_scope, const char* name) {
    while(curr_scope) {
        Symbol* symbol = look_for_symbol(curr_scope->symbol_lookup, name);
        if(symbol) {
            return symbol;
        }
        curr_scope = curr_scope->parent;
    }

    return NULL;
}

static Scope* enter_scope(ScopeTracker* tracker, ASTNode* curr_func) {
    Scope* new_scope = create_scope(tracker->curr_scope, curr_func);
    tracker->curr_scope = new_scope;
    return new_scope;
}

static void exit_scope(ScopeTracker* tracker) {
    if(tracker->curr_scope) {
        Scope* scope_to_close = tracker->curr_scope;

        if(scope_to_close->must_be_closed) {
            // TODO: implement closing values
        }

        tracker->curr_scope = scope_to_close->parent;

        // test these first
        free(scope_to_close->symbol_lookup);
        free(scope_to_close);
    }
}

static void add_symbol_to_scope(Scope* scope, Symbol* symbol) {
    if(scope->symbol_count >= scope->lookup_capacity) {
        scope->lookup_capacity += 5;
        scope->symbol_lookup =
            realloc(scope->symbol_lookup, scope->lookup_capacity * sizeof(Symbol*));
        if(!scope->symbol_lookup) {
            LOG_FATAL("FAILED TO REALLOC");
        }
        LOG_INFO("REALLOCED for SCOPE:<%s>", scope->name);
    }
    scope->symbol_lookup[scope->symbol_count++] = symbol;
}

static Symbol* look_for_symbol(Symbol** symbol_lookup, const char* name) {
    for(Symbol* symbol = *symbol_lookup; symbol; symbol++) {
        if(strcmp(symbol->name, name) == 0) {
            return symbol;
        }
    }

    return NULL;
}

static Scope* create_scope(Scope* parent, ASTNode* curr_function) {
    Scope* scope = calloc(1, sizeof(Scope));
    scope->function = curr_function;
    scope->parent = parent;
    scope->lookup_capacity = 10;
    scope->symbol_lookup = calloc(scope->lookup_capacity, sizeof(Symbol*));
    scope->must_be_closed = false;
    scope->symbol_count = 0;
    return scope;
}

static Token* consume_token(Parser* parser, TokenKind expected) {
    if(parser->curr_token->kind != expected) {
        return NULL;
    }

    Token* curr_token = malloc(sizeof(Token));
    *curr_token = *(parser->curr_token);
    if(parser->curr_token->value) {
        curr_token->value = strdup(parser->curr_token->value);
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
