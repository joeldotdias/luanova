#include <errno.h>
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
static SymbolList* parse_assignment_lhs(Parser* parser);
static ASTNodeList* parse_assignment_rhs(Parser* parser, size_t lhs_count);
static ASTNode* parse_function_stmt(Parser* parser, bool is_local);
static ASTNode* parse_func_expr(Parser* parser);
static SymbolList* parse_func_params(Parser* parser);
static ASTNode* parse_return(Parser* parser);
static ASTNode* parse_expr(Parser* parser);
static ASTNode* parse_func_call(Parser* parser);
static ASTNodeList* parse_func_args(Parser* parser);
static ASTNode* parse_table_literal(Parser* parser);
static ASTNode* parse_table_element(Parser* parser);
static ASTNode* parse_index_expr(Parser* parser);
static ASTNode* parse_symbol(Parser* parser, bool is_confined_to_curr_scope);
static ASTNode* parse_str_literal(Parser* parser);
static ASTNode* parse_num_literal(Parser* parser);

static Scope* enter_scope(ScopeTracker* tracker, ASTNode* curr_function);
static void leave_scope(ScopeTracker* tracker);
static Scope* create_scope(Scope* parent, ASTNode* curr_function);
static void add_symbol_to_scope(Scope* scope, Symbol* symbol);
static Symbol* resolve_symbol(Scope* curr_scope, const char* name);
static Symbol* look_for_symbol(SymbolList* symbol_lookup, const char* name);

static ASTNode* make_node(NodeKind kind);
static Token* consume_token(Parser* parser, TokenKind expected);
static bool expect_token(Parser* parser, TokenKind expected);
static void advance_parser(Parser* parser);
static char* get_name_val_from_node(const ASTNode* node);

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

static ASTNode* parse_expr(Parser* parser) {
    ASTNode* node = NULL;

    if(parser->curr_token->kind == TOKEN_IDENT) {
        if(expect_token(parser, TOKEN_LPAREN)) {
            node = parse_func_call(parser);
        } else {
            // other shit
            node = parse_symbol(parser, false);
        }
    } else if(consume_token(parser, TOKEN_FUNCTION)) {
        node = parse_func_expr(parser);
    } else if(consume_token(parser, TOKEN_LCURLY)) {
        node = parse_table_literal(parser);
    } else if(consume_token(parser, TOKEN_LBRACKET)) {
        node = parse_index_expr(parser);
    } else if(parser->curr_token->kind == TOKEN_STR) {
        node = parse_str_literal(parser);
    } else if(parser->curr_token->kind == TOKEN_NUMBER) {
        node = parse_num_literal(parser);
    }

    return node;
}

static ASTNode* parse_local_assignment(Parser* parser) {
    if(!consume_token(parser, TOKEN_LOCAL)) {
        FAILED_EXPECTATION("LOCAL");
    }

    if(consume_token(parser, TOKEN_FUNCTION)) {
        return parse_function_stmt(parser, true);
    }

    ASTNode* node = make_node(ASTNODE_LOCAL_VAR_DECL);
    Assignment* asgmt = calloc(1, sizeof(Assignment));
    asgmt->var_list = parse_assignment_lhs(parser);

    if(!consume_token(parser, TOKEN_ASSIGN)) {
        FAILED_EXPECTATION("ASSIGN");
    }

    asgmt->expr_list = parse_assignment_rhs(parser, asgmt->var_list->count);

    for(size_t i = 0; i < asgmt->expr_list->count; i++) {
        if(asgmt->expr_list->nodes[i]->kind == ASTNODE_FUNC_EXPR) {
            Symbol* assignee = asgmt->var_list->symbols[i];
            if(!assignee) {
                continue;
            }

            char* scope_name = malloc(strlen("FUNC_") + strlen(assignee->name) + 1);
            sprintf(scope_name, "FUNC_%s", assignee->name);
            asgmt->expr_list->nodes[i]->func_expr.scope->name = scope_name;
        }
    }

    // this is just a warning in lua but we will not allow this non sense
    if(asgmt->var_list->count < asgmt->expr_list->count) {
        FATAL("More values on RHS than vars on LHS of assignment");
    }

    node->assignment = *asgmt;
    return node;
}

static ASTNode* parse_function_stmt(Parser* parser, bool is_local) {
    ASTNode* node = make_node(ASTNODE_FUNC_STMT);
    FuncStmt* func_stmt = calloc(1, sizeof(FuncStmt));
    Symbol* func_name = &parse_symbol(parser, is_local)->symbol;
    func_stmt->name = func_name;

    add_symbol_to_scope(parser->scope_tracker->curr_scope, func_name);

    ASTNode* func_expr_node = parse_func_expr(parser);
    char* scope_name = malloc(strlen("FUNC_") + strlen(func_name->name) + 1);
    sprintf(scope_name, "FUNC_%s", func_name->name);
    if(!(&func_expr_node->func_expr)->scope) {
        FATAL("No scope allocated");
    }
    (&func_expr_node->func_expr)->scope->name = scope_name;

    func_stmt->func_expr = func_expr_node;

    node->func_stmt = *func_stmt;
    leave_scope(parser->scope_tracker);

    return node;
}

static ASTNode* parse_func_expr(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_FUNC_EXPR);
    FuncExpr* func = calloc(1, sizeof(FuncExpr));
    Scope* curr_scope = enter_scope(parser->scope_tracker, node);
    func->scope = curr_scope;

    if(!consume_token(parser, TOKEN_LPAREN)) {
        FAILED_EXPECTATION("LPAREN");
    }

    if(!consume_token(parser, TOKEN_RPAREN)) {
        func->params = parse_func_params(parser);
        if(!consume_token(parser, TOKEN_RPAREN)) {
            FAILED_EXPECTATION("RPAREN");
        }
    }

    ASTNode* chunk = parse_chunk(parser, TOKEN_END);
    func->body = chunk;
    if(!consume_token(parser, TOKEN_END)) {
        FAILED_EXPECTATION("FUNC END");
    }
    node->func_expr = *func;

    leave_scope(parser->scope_tracker);

    return node;
}

static ASTNode* parse_return(Parser* parser) {
    UNIMPLEMENTED();
    ASTNode* node = make_node(ASTNODE_RETURN_STMT);
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

static SymbolList* parse_func_params(Parser* parser) {
    SymbolList* params = init_symbol_list();

    while(parser->curr_token->kind != TOKEN_RPAREN) {
        ASTNode* param = parse_symbol(parser, true);
        add_to_symbol_list(params, &param->symbol);
        add_symbol_to_scope(parser->scope_tracker->curr_scope, &param->symbol);

        if(parser->curr_token->kind == TOKEN_COMMA) {
            advance_parser(parser);
        }
    }

    return params;
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

static ASTNode* parse_table_literal(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_TABLE_LITERAL);
    TableLiteralExpr* table_literal = calloc(1, sizeof(TableLiteralExpr));
    table_literal->expr_list = init_ast_node_list();

    while(parser->curr_token->kind != TOKEN_RCURLY) {
        ASTNode* table_elem = parse_table_element(parser);
        add_to_ast_node_list(table_literal->expr_list, table_elem);

        if(parser->curr_token->kind == TOKEN_COMMA ||
           parser->curr_token->kind == TOKEN_SEMICOLON) {
            advance_parser(parser);
        }
    }

    if(!consume_token(parser, TOKEN_RCURLY)) {
        FAILED_EXPECTATION("RCURLY");
    }

    node->table_literal = *table_literal;

    return node;
}

static ASTNode* parse_table_element(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_TABLE_ELEMENT);
    TableElement* elem = calloc(1, sizeof(TableElement));

    if(parser->curr_token->kind == TOKEN_IDENT) {
        elem->key = parse_symbol(parser, true);
        if(!consume_token(parser, TOKEN_ASSIGN)) {
            FAILED_EXPECTATION("ASSIGN");
        }
    } else if(parser->curr_token->kind == TOKEN_LBRACKET) {
        elem->key = parse_expr(parser);
        if(!consume_token(parser, TOKEN_ASSIGN)) {
            FAILED_EXPECTATION("ASSIGN");
        }
    } else {
        // list type tables
        elem->key = NULL;
    }
    elem->value = parse_expr(parser);
    if(elem->value->kind == ASTNODE_FUNC_EXPR) {
        if(elem->key) {
            char* identi = get_name_val_from_node(elem->key);
            char* scope_name = malloc(strlen("FUNC_INDEX_") + strlen(identi) + 1);
            sprintf(scope_name, "FUNC_INDEX_%s", identi);
            (&elem->value->func_expr)->scope->name = scope_name;
        }
    }

    node->table_elem = *elem;
    return node;
}

static ASTNode* parse_index_expr(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_INDEX_EXPR);
    IndexExpr* index = calloc(1, sizeof(IndexExpr));
    index->expr = parse_expr(parser);

    node->index_expr = *index;
    if(!consume_token(parser, TOKEN_RBRACKET)) {
        FAILED_EXPECTATION("RBRACKET");
    }
    return node;
}

static SymbolList* parse_assignment_lhs(Parser* parser) {
    SymbolList* vars = init_symbol_list();

    while(parser->curr_token->kind != TOKEN_ASSIGN) {
        ASTNode* var = parse_symbol(parser, true);
        Symbol* var_symbol = &var->symbol;
        var_symbol->scope = parser->scope_tracker->curr_scope;
        add_to_symbol_list(vars, var_symbol);
        add_symbol_to_scope(parser->scope_tracker->curr_scope, var_symbol);

        if(parser->curr_token->kind == TOKEN_COMMA) {
            advance_parser(parser);
        }
    }

    return vars;
}

static ASTNodeList* parse_assignment_rhs(Parser* parser, size_t lhs_count) {
    ASTNodeList* exprs = init_ast_node_list();

    for(size_t i = 0; i < lhs_count; i++) {
        ASTNode* expr = parse_expr(parser);
        if(!expr) {
            FATAL("HUHH");
        }
        add_to_ast_node_list(exprs, expr);

        if(parser->curr_token->kind == TOKEN_COMMA) {
            advance_parser(parser);
        }
    }

    return exprs;
}

static ASTNode* parse_str_literal(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_STR_LITERAL);
    StrLiteral* str = calloc(1, sizeof(StrLiteral));
    Token* str_tok = consume_token(parser, TOKEN_STR);
    if(!str_tok) {
        FAILED_EXPECTATION("STR LITERAL");
    }
    str->str_val = strdup(str_tok->value);
    node->str_literal = *str;

    return node;
}

static ASTNode* parse_num_literal(Parser* parser) {
    /* INFO("We be parsing this"); */
    ASTNode* node = make_node(ASTNODE_NUM_LITERAL);
    NumLiteral* num = calloc(1, sizeof(NumLiteral));
    Token* num_tok = consume_token(parser, TOKEN_NUMBER);
    if(!num_tok) {
        FAILED_EXPECTATION("NUM LITERAL");
    }
    char* end;
    errno = 0;
    double value = strtod(num_tok->value, &end);
    if(end == num_tok->value) {
        // should typically never happen
        FATAL("This shit was not a number");
    }
    if(errno == ERANGE) {
        FATAL("Got out of range decimal value for %s", num_tok->value);
    }

    num->num_val = value;
    node->num_literal = *num;

    return node;
}

static ASTNode* parse_symbol(Parser* parser, bool is_confined_to_curr_scope) {
    Token* ident = consume_token(parser, TOKEN_IDENT);
    if(!ident) {
        FAILED_EXPECTATION("IDENT");
    }

    ASTNode* node = make_node(ASTNODE_SYMBOL);

    if(is_confined_to_curr_scope) {
        Symbol* symbol = calloc(1, sizeof(Symbol));
        symbol->name = strdup(ident->value);
        symbol->scope = parser->scope_tracker->curr_scope;
        node->symbol = *symbol;
    } else {
        Symbol* resolved_symbol =
            resolve_symbol(parser->scope_tracker->curr_scope, ident->value);
        if(!resolved_symbol) {
            FATAL("Couldn't find symbol %s", ident->value);
        }
        node->symbol = *resolved_symbol;
    }

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

static void leave_scope(ScopeTracker* tracker) {
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

static void advance_parser(Parser* parser) {
    Token* consumed_token = parser->curr_token;
    parser->curr_token = parser->peeked_token;
    annihilate_token(&consumed_token);
    parser->peeked_token = next_token(parser->lexer);
}

static Symbol* look_for_symbol(SymbolList* symbol_lookup, const char* name) {
    for(size_t i = 0; i < symbol_lookup->count; i++) {
        if(strcmp(symbol_lookup->symbols[i]->name, name) == 0) {
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

static char* get_name_val_from_node(const ASTNode* node) {
    char* buf = malloc(50 * sizeof(char));

    switch(node->kind) {
        case ASTNODE_SYMBOL:
            return node->symbol.name;
        case ASTNODE_STR_LITERAL:
            return node->str_literal.str_val;
        case ASTNODE_NUM_LITERAL:
            snprintf(buf, 50, "%f", node->num_literal.num_val);
            return buf;
        case ASTNODE_INDEX_EXPR:
            return get_name_val_from_node(node->index_expr.expr);
        default:
            return "I DO NOT KNOW";
    }
}
