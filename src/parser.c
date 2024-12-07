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
static SymbolList* parse_func_params(Parser* parser);
static ASTNode* parse_return(Parser* parser);
static ASTNode* parse_expr(Parser* parser);
static ASTNode* parse_func_call(Parser* parser);
static ASTNodeList* parse_func_args(Parser* parser);
static ASTNode* parse_symbol(Parser* parser);

static Scope* enter_scope(ScopeTracker* tracker, ASTNode* curr_function);
static void exit_scope(ScopeTracker* tracker);
static Scope* create_scope(Scope* parent, ASTNode* curr_function);
static void add_symbol_to_scope(Scope* scope, Symbol* symbol);
static Symbol* resolve_symbol(Scope* curr_scope, const char* name);
static Symbol* look_for_symbol(SymbolList* symbol_lookup, const char* name);

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
    global_scope->name = "_MAIN";
    global_scope->symbol_lookup = init_symbol_list();
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
    ASTNode* node = make_node(ASTNODE_CHUNK);

    Chunk* chunk = calloc(1, sizeof(Chunk));
    chunk->stmteez = init_ast_node_list();

    while(parser->curr_token->kind != TOKEN_EOF && parser->curr_token->kind != fail) {
        if(parser->curr_token->kind == TOKEN_LOCAL) {
            ASTNode* assgmt = parse_local_assignment(parser);
            add_to_ast_node_list(chunk->stmteez, assgmt);
        } else if(parser->curr_token->kind == TOKEN_RETURN) {
            ASTNode* ret_stmt = parse_return(parser);
            add_to_ast_node_list(chunk->stmteez, ret_stmt);
        } else {
            ASTNode* expr = parse_expr(parser);
            add_to_ast_node_list(chunk->stmteez, expr);
        }
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

    ASTNode* node = make_node(ASTNODE_LOCAL_VAR_DECLR);
    Assignment* asgmt = calloc(1, sizeof(Assignment));

    Token* ident = consume_token(parser, TOKEN_IDENT);
    if(!ident) {
        FAILED_EXPECTATION("IDENT");
    }

    ASTNode* symbol = make_node(ASTNODE_SYMBOL);
    symbol->symbol.name = strdup(ident->value);
    symbol->symbol.is_local = true;
    add_symbol_to_scope(parser->scope_tracker->curr_scope, &symbol->symbol);
    asgmt->var_list = symbol;

    if(!consume_token(parser, TOKEN_ASSIGN)) {
        FAILED_EXPECTATION("ASSIGN");
    }

    // TODO: string literal
    Token* literal_tok = consume_token(parser, TOKEN_STR);
    ASTNode* literal = calloc(1, sizeof(ASTNode));
    literal->kind = ASTNODE_STR_LITERAL;

    literal->str_literal.str_val = strdup(literal_tok->value);
    asgmt->expr_list = literal;

    node->assignment = *asgmt;

    return node;
}

static ASTNode* parse_local_function(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_FUNC_EXPR);
    FuncExpr* func = calloc(1, sizeof(FuncExpr));
    Symbol* func_name = &parse_symbol(parser)->symbol;
    func_name->scope = parser->scope_tracker->curr_scope;
    func->name = func_name;
    func->scope = func_name->scope; // change this stupidity
    func->params = NULL;

    add_symbol_to_scope(parser->scope_tracker->curr_scope, func_name);
    Scope* curr_scope = enter_scope(parser->scope_tracker, node);
    curr_scope->name = malloc(strlen("FUNC_") + strlen(func_name->name) + 1);
    sprintf(curr_scope->name, "FUNC_%s", func_name->name);
    if(!consume_token(parser, TOKEN_LPAREN)) {
        FAILED_EXPECTATION("LPAREN");
    }

    if(!consume_token(parser, TOKEN_RPAREN)) {
        func->params = parse_func_params(parser);
    }
    if(!consume_token(parser, TOKEN_RPAREN)) {
        FAILED_EXPECTATION("RPAREN");
    }

    /* LOG_FATAL("Parsed params now curr: %s", token_to_str(parser->curr_token)); */

    ASTNode* chunk = parse_chunk(parser, TOKEN_END);
    func->body = chunk;
    advance_parser(parser);

    /* consume_token(parser, TOKEN_RPAREN); */

    node->func_decl = *func;
    exit_scope(parser->scope_tracker);

    return node;
}

static SymbolList* parse_func_params(Parser* parser) {
    SymbolList* params = init_symbol_list();

    while(parser->curr_token->kind != TOKEN_RPAREN) {
        ASTNode* param = parse_symbol(parser);
        add_to_symbol_list(params, &param->symbol);

        if(parser->curr_token->kind == TOKEN_COMMA) {
            advance_parser(parser);
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
        if(expect_token(parser, TOKEN_LPAREN)) {
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
    FuncCall* func_call = calloc(1, sizeof(FuncCall));
    func_call->prefix = NULL;
    func_call->name = strdup(parser->curr_token->value);
    func_call->args = NULL;
    advance_parser(parser);
    advance_parser(parser); // eat the (

    if(!consume_token(parser, TOKEN_RPAREN)) {
        func_call->args = parse_func_args(parser);
        if(!consume_token(parser, TOKEN_RPAREN)) {
            FAILED_EXPECTATION("RPAREN");
        }
    }

    node->func_call = *func_call;
    return node;
}

static ASTNodeList* parse_func_args(Parser* parser) {
    ASTNodeList* args = init_ast_node_list();

    while(parser->curr_token->kind != TOKEN_RPAREN) {
        ASTNode* arg = parse_expr(parser);
        add_to_ast_node_list(args, arg);

        if(parser->curr_token->kind == TOKEN_COMMA) {
            advance_parser(parser);
        }
    }

    return args;
}

static void advance_parser(Parser* parser) {
    Token* consumed_token = parser->curr_token;
    parser->curr_token = parser->peeked_token;
    annihilate_token(&consumed_token);
    parser->peeked_token = next_token(parser->lexer);
}

static ASTNode* parse_symbol(Parser* parser) {
    Token* ident = consume_token(parser, TOKEN_IDENT);
    if(!ident) {
        FAILED_EXPECTATION("IDENT");
    }

    ASTNode* node = make_node(ASTNODE_SYMBOL);
    Symbol* symbol = calloc(1, sizeof(Symbol));
    symbol->name = strdup(ident->value);
    // TODO: Change scope resolution
    symbol->scope = parser->scope_tracker->curr_scope;
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
    add_to_symbol_list(scope->symbol_lookup, symbol);
}

static Symbol* look_for_symbol(SymbolList* symbol_lookup, const char* name) {
    for(size_t i = 0; i < symbol_lookup->count; i++) {
        if(strcmp(symbol_lookup->symbols[i]->name, name)) {
            return symbol_lookup->symbols[i];
        }
    }

    return NULL;
}

static Scope* create_scope(Scope* parent, ASTNode* curr_function) {
    Scope* scope = calloc(1, sizeof(Scope));
    scope->function = curr_function;
    scope->parent = parent;
    scope->symbol_lookup = init_symbol_list();
    scope->must_be_closed = false;
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
