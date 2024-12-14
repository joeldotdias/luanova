#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "shared.h"

/* top level */
static ASTNode* parse_chunk(Parser* parser, TokenKind fail);

/* statements */
static ASTNode* parse_local_assignment(Parser* parser);
static ASTNode* parse_local_function_stmt(Parser* parser);
static ASTNode* parse_return(Parser* parser);
static ASTNode* parse_expr_stmt(Parser* parser);
static SymbolList* parse_assignment_lhs(Parser* parser);
static ASTNodeList* parse_assignment_rhs(Parser* parser, size_t lhs_count);

/* expressions */
static ASTNode* parse_expr(Parser* parser);
static ASTNode* parse_expr_with_context(Parser* parser, int prec_threshold,
                                        InfixOperator* unproccessed_op);
static ASTNode* parse_basic_expr(Parser* parser);

static ASTNode* parse_suffixed_expr(Parser* parser);
static ASTNode* parse_primary_expr(Parser* parser);
static ASTNode* parse_func_expr(Parser* parser);
static SymbolList* parse_func_params(Parser* parser);
static ASTNode* parse_func_call(Parser* parser);
static ASTNodeList* parse_func_args(Parser* parser);
static ASTNode* parse_table_literal(Parser* parser);
static ASTNode* parse_table_element(Parser* parser);
static ASTNode* parse_index_expr(Parser* parser);
static ASTNode* parse_symbol(Parser* parser, bool is_confined_to_curr_scope);
static ASTNode* parse_str_literal(Parser* parser);
static ASTNode* parse_num_literal(Parser* parser);

/* scope tracking */
static Scope* enter_scope(ScopeTracker* tracker, ASTNode* curr_function);
static void leave_scope(ScopeTracker* tracker);
static Symbol* resolve_symbol(Scope* curr_scope, const char* name);
static void add_symbol_to_scope(Scope* scope, Symbol* symbol);
static Symbol* look_for_symbol(SymbolList* symbol_lookup, const char* name);
static Scope* create_scope(Scope* parent, ASTNode* curr_function);

/* operator precedence */
static Precedence prec_from_infix_op(InfixOperator op);
static InfixOperator infix_op_from_tok(Token* tok);
static PrefixOperator prefix_op_from_tok(Token* tok);

/* helpers */
static ASTNode* make_node(NodeKind kind);
static Token* consume_token_and_clone(Parser* parser, TokenKind expected);
static bool consume_token(Parser* parser, TokenKind expected);
static bool expect_token(Parser* parser, TokenKind expected);
static void advance_parser(Parser* parser);

/* technically useless but great for debugging */
static void maybe_give_scope_a_name(ASTNode* lhs, ASTNode* rhs);
static char* get_name_val_from_node(const ASTNode* node, bool* needs_free);

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

/*
=====================================================
                    TOP LEVEL
=====================================================
*/

ASTNode* parse(Parser* parser) {
    ASTNode* chunk = parse_chunk(parser, TOKEN_EOF);
    return chunk;
}

// fail is the token at which we stop processing the chunk
static ASTNode* parse_chunk(Parser* parser, TokenKind fail) {
    ASTNode* node = make_node(ASTNODE_CHUNK);

    Chunk* chunk = calloc(1, sizeof(Chunk));
    chunk->stmteez = init_ast_node_list();

    ASTNode* stmt = NULL;
    // This part is technically the parse_statement function
    while(parser->curr_token->kind != TOKEN_EOF && parser->curr_token->kind != fail) {
        switch(parser->curr_token->kind) {
            case TOKEN_LOCAL:
                stmt = parse_local_assignment(parser);
                break;
            case TOKEN_RETURN:
                stmt = parse_return(parser);
                break;

            case TOKEN_SEMICOLON:
                advance_parser(parser);
                continue;
                break;

            default:
                stmt = parse_expr_stmt(parser);
        }

        add_to_ast_node_list(chunk->stmteez, stmt);
    }

    if(chunk != NULL) {
        node->chunk = *chunk;
        memcpy(&(node->chunk), chunk, sizeof(Chunk));
        free(chunk);
    }
    return node;
}

/*
=====================================================
                      STATEMENTS
=====================================================
*/

static ASTNode* parse_local_assignment(Parser* parser) {
    if(!consume_token(parser, TOKEN_LOCAL)) {
        FAILED_EXPECTATION("LOCAL");
    }

    if(consume_token(parser, TOKEN_FUNCTION)) {
        return parse_local_function_stmt(parser);
    }

    ASTNode* node = make_node(ASTNODE_LOCAL_VAR_DECL);
    LocalAssignment* asgmt = calloc(1, sizeof(LocalAssignment));
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
        } else if(asgmt->expr_list->nodes[i]->kind == ASTNODE_TABLE_LITERAL) {
            Symbol* assignee = asgmt->var_list->symbols[i];
            if(!assignee) {
                continue;
            }

            char* scope_name = malloc(strlen("FUNC_") + strlen(assignee->name) + 1);
            sprintf(scope_name, "TABLE_%s", assignee->name);
            asgmt->expr_list->nodes[i]->table_literal.scope->name = scope_name;
        }
    }

    // this is just a warning in lua but we will not allow this non sense
    if(asgmt->var_list->count < asgmt->expr_list->count) {
        FATAL("More values on RHS than vars on LHS of assignment");
    }

    memcpy(&node->assignment, asgmt, sizeof(LocalAssignment));
    free(asgmt);
    return node;
}

static ASTNode* parse_local_function_stmt(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_FUNC_STMT);
    FuncStmt* func_stmt = calloc(1, sizeof(FuncStmt));
    Symbol* func_name = &parse_symbol(parser, true)->symbol;
    func_stmt->name = func_name;

    add_symbol_to_scope(parser->scope_tracker->curr_scope, func_name);

    ASTNode* func_expr = parse_func_expr(parser);

    char* scope_name = malloc(strlen("FUNC_") + strlen(func_name->name) + 1);
    sprintf(scope_name, "FUNC_%s", func_name->name);
    if(!(&func_expr->func_expr)->scope) {
        FATAL("No scope allocated");
    }
    (&func_expr->func_expr)->scope->name = scope_name;

    func_stmt->func_expr = func_expr;

    memcpy(&node->func_stmt, func_stmt, sizeof(FuncStmt));
    free(func_stmt);

    return node;
}

static ASTNode* parse_return(Parser* parser) {
    if(!consume_token(parser, TOKEN_RETURN)) {
        FAILED_EXPECTATION("RETURN");
    }

    ASTNode* node = make_node(ASTNODE_RETURN_STMT);
    ReturnStmt* ret_stmt = calloc(1, sizeof(ReturnStmt));
    ret_stmt->return_val = parse_expr(parser);
    memcpy(&node->return_stmt, ret_stmt, sizeof(ReturnStmt));
    free(ret_stmt);
    return node;
}

static ASTNode* parse_expr_stmt(Parser* parser) {
    ASTNode* stmt = make_node(ASTNODE_EXPR_STMT);
    stmt->expr_stmt.var_expr_list = NULL;
    stmt->expr_stmt.expr_list = NULL;

    ASTNodeList* curr_list = init_ast_node_list();
    ASTNode* maybe_suffixed_expr = parse_suffixed_expr(parser);
    add_to_ast_node_list(curr_list, maybe_suffixed_expr);

    while(consume_token(parser, TOKEN_COMMA)) {
        ASTNode* maybe_still_a_suffixed_expr = parse_suffixed_expr(parser);
        add_to_ast_node_list(curr_list, maybe_still_a_suffixed_expr);
    }

    if(consume_token(parser, TOKEN_ASSIGN)) {
        stmt->expr_stmt.var_expr_list = curr_list;
        curr_list = NULL;
        curr_list = parse_assignment_rhs(parser, stmt->expr_stmt.var_expr_list->count);
    }

    stmt->expr_stmt.expr_list = curr_list;
    return stmt;
}

static SymbolList* parse_assignment_lhs(Parser* parser) {
    SymbolList* vars = init_symbol_list();

    while(parser->curr_token->kind != TOKEN_ASSIGN) {
        ASTNode* var = parse_symbol(parser, true);
        Symbol* var_symbol = &var->symbol;
        add_to_symbol_list(vars, var_symbol);
        var_symbol->scope = parser->scope_tracker->curr_scope;
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
            FATAL("Couldn't parse expression on rhs");
        }
        add_to_ast_node_list(exprs, expr);

        if(parser->curr_token->kind == TOKEN_COMMA) {
            advance_parser(parser);
        }
    }

    return exprs;
}

/*
=====================================================
                        EXPRESSIONS
=====================================================
*/

// rest of precedence is in the OPERATOR PRECEDENCE section
#define UNARY_PREC 12

static ASTNode* parse_expr(Parser* parser) {
    InfixOperator nothing;
    return parse_expr_with_context(parser, 0, &nothing);
}

static ASTNode* parse_expr_with_context(Parser* parser, int prec_threshold,
                                        InfixOperator* unproccessed_op) {
    ASTNode* expr = NULL;
    InfixOperator infix_op;
    PrefixOperator prefix_op;
    prefix_op = prefix_op_from_tok(parser->curr_token);

    if(prefix_op != NO_PREFIX) {
        advance_parser(parser);
        InfixOperator ignore;
        ASTNode* sub_expr = parse_expr_with_context(parser, UNARY_PREC, &ignore);
        expr = make_node(ASTNODE_UNARY_EXPR);
        expr->unary_expr.op = prefix_op;
        expr->unary_expr.operand = sub_expr;
    } else {
        expr = parse_basic_expr(parser);
    }

    infix_op = infix_op_from_tok(parser->curr_token);
    while(infix_op != NO_INFIX &&
          prec_from_infix_op(infix_op).left_bind > prec_threshold) {
        InfixOperator next_op;
        advance_parser(parser);
        ASTNode* right_expr = parse_expr_with_context(
            parser, prec_from_infix_op(infix_op).right_bind, &next_op);

        ASTNode* bin_expr = make_node(ASTNODE_BINARY_EXPR);
        bin_expr->binary_expr.left = expr;
        bin_expr->binary_expr.op = infix_op;
        bin_expr->binary_expr.right = right_expr;

        // make the entire parsed expr as the left expr for next iteration
        expr = bin_expr;
        infix_op = next_op;
    }

    *unproccessed_op = infix_op;

    return expr;
}

static ASTNode* parse_basic_expr(Parser* parser) {
    ASTNode* expr = NULL;

    switch(parser->curr_token->kind) {
        case TOKEN_FUNCTION:
            advance_parser(parser);
            expr = parse_func_expr(parser);
            break;
        case TOKEN_LCURLY:
            advance_parser(parser);
            expr = parse_table_literal(parser);
            break;
        case TOKEN_LPAREN:
            advance_parser(parser);
            expr = parse_expr(parser);
            print_ast_node(expr, 0);
            if(!consume_token(parser, TOKEN_RPAREN)) {
                FATAL("Unbalanced parens");
            }
            break;
        case TOKEN_STR:
            expr = parse_str_literal(parser);
            break;
        case TOKEN_NUMBER:
            expr = parse_num_literal(parser);
            break;
        default:
            expr = parse_suffixed_expr(parser);
    }

    return expr;
}

static ASTNode* parse_func_call(Parser* parser) {
    ASTNode* func_call = make_node(ASTNODE_FUNC_CALL_EXPR);
    FATAL("In func call");
}

static ASTNode* parse_suffixed_expr(Parser* parser) {
    ASTNode* expr = make_node(ASTNODE_SUFFIXED_EXPR);
    expr->suffixed_expr.primary_expr = parse_primary_expr(parser);
    expr->suffixed_expr.suffix_list = NULL;

    for(;;) {
        switch(parser->curr_token->kind) {
            case TOKEN_LPAREN:
                return parse_func_call(parser);
                break;
            default:
                return expr;
        }
    }
}

static ASTNode* parse_primary_expr(Parser* parser) {
    ASTNode* primary_expr = NULL;

    switch(parser->curr_token->kind) {
        case TOKEN_LPAREN:
            primary_expr = parse_expr(parser);
            if(!consume_token(parser, TOKEN_RPAREN)) {
                FATAL("Parens not balanced");
            }
            break;

        case TOKEN_IDENT:
            primary_expr = parse_symbol(parser, false);
            break;

        default:
            FATAL("Shoudln' have come here. Received %s",
                  token_to_str(parser->curr_token));
    }

    if(primary_expr == NULL) {
        FATAL("Couldn't parse expression");
    }

    return primary_expr;
}

static ASTNode* parse_func_expr(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_FUNC_EXPR);
    FuncExpr* func = calloc(1, sizeof(FuncExpr));
    Scope* curr_scope = enter_scope(parser->scope_tracker, node);
    func->scope = curr_scope;

    if(!consume_token(parser, TOKEN_LPAREN)) {
        FAILED_EXPECTATION("LPAREN");
    }

    func->params = NULL;
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

    memcpy(&node->func_expr, func, sizeof(FuncExpr));
    free(func);

    leave_scope(parser->scope_tracker);

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

static ASTNode* parse_table_literal(Parser* parser) {
    ASTNode* node = make_node(ASTNODE_TABLE_LITERAL);
    TableLiteralExpr* table_literal = calloc(1, sizeof(TableLiteralExpr));
    table_literal->expr_list = init_ast_node_list();
    Scope* ti_scope = enter_scope(parser->scope_tracker, node);
    table_literal->scope = ti_scope;

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

    memcpy(&node->table_literal, table_literal, sizeof(TableLiteralExpr));
    free(table_literal);
    leave_scope(parser->scope_tracker);

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
    maybe_give_scope_a_name(elem->key, elem->value);

    memcpy(&node->table_elem, elem, sizeof(TableElement));
    free(elem);

    return node;
}

static ASTNode* parse_symbol(Parser* parser, bool is_confined_to_curr_scope) {
    Token* ident = consume_token_and_clone(parser, TOKEN_IDENT);
    if(!ident) {
        FAILED_EXPECTATION("IDENT");
    }

    ASTNode* node = make_node(ASTNODE_SYMBOL);

    if(is_confined_to_curr_scope) {
        Symbol* symbol = calloc(1, sizeof(Symbol));
        symbol->name = strdup(ident->value);
        symbol->scope = parser->scope_tracker->curr_scope;
        memcpy(&node->symbol, symbol, sizeof(Symbol));
        free(symbol);
    } else {
        Symbol* resolved_symbol =
            resolve_symbol(parser->scope_tracker->curr_scope, ident->value);
        if(!resolved_symbol) {
            FATAL("Couldn't find symbol %s", ident->value);
        }
        node->symbol = *resolved_symbol;
    }

    annihilate_token(&ident);

    return node;
}

static ASTNode* parse_str_literal(Parser* parser) {
    Token* str_tok = consume_token_and_clone(parser, TOKEN_STR);
    if(!str_tok) {
        FAILED_EXPECTATION("STR LITERAL");
    }

    ASTNode* node = make_node(ASTNODE_STR_LITERAL);
    StrLiteral* str = calloc(1, sizeof(StrLiteral));

    str->str_val = strdup(str_tok->value);
    node->str_literal = *str;
    free(str);
    annihilate_token(&str_tok);

    return node;
}

static ASTNode* parse_num_literal(Parser* parser) {
    Token* num_tok = consume_token_and_clone(parser, TOKEN_NUMBER);
    if(!num_tok) {
        FAILED_EXPECTATION("NUM LITERAL");
    }

    ASTNode* node = make_node(ASTNODE_NUM_LITERAL);
    NumLiteral* num = calloc(1, sizeof(NumLiteral));

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
    memcpy(&(node->num_literal), num, sizeof(NumLiteral));
    free(num);
    annihilate_token(&num_tok);

    return node;
}

/*
=====================================================
                    SCOPE TRACKING
=====================================================
*/

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

static void add_symbol_to_scope(Scope* scope, Symbol* symbol) {
    add_to_symbol_list(scope->symbol_lookup, symbol);
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

static void advance_parser(Parser* parser) {
    Token* consumed_token = parser->curr_token;
    parser->curr_token = parser->peeked_token;
    annihilate_token(&consumed_token);
    parser->peeked_token = next_token(parser->lexer);
}

/*
=====================================================
                  OPERATOR PRECEDENCE
=====================================================
*/

static Precedence prec_from_infix_op(InfixOperator op) {
    int left_bind, right_bind = 0;

    switch(op) {
        case OP_ADD:
        case OP_SUB:
            left_bind = right_bind = 10;
            break;

        case OP_MUL:
        case OP_DIV:
        case OP_MODULO:
            left_bind = right_bind = 11;
            break;

        case OP_EXPO:
            left_bind = 14;
            right_bind = 13;
            break;

        case OP_LT:
        case OP_LE:
        case OP_GT:
        case OP_GE:
        case OP_EQ:
        case OP_NE:
            left_bind = right_bind = 3;
            break;

        case OP_AND:
            left_bind = right_bind = 2;
            break;

        case OP_OR:
            left_bind = right_bind = 1;
            break;

        case OP_CONCAT:
            left_bind = 9;
            right_bind = 8;
            break;

        case NO_INFIX:
            left_bind = right_bind = 0;
            break;

        default:
            FATAL("Didn't receive a infix operator");
    }

    return (Precedence){left_bind, right_bind};
}

static PrefixOperator prefix_op_from_tok(Token* tok) {
    switch(tok->kind) {
        case TOKEN_NOT:
            return OP_NOT;
        case TOKEN_MINUS:
            return OP_NEGATE;
        case TOKEN_OCTO:
            return OP_LENGTH;
        default:
            return NO_PREFIX;
    }
}

static InfixOperator infix_op_from_tok(Token* tok) {
    switch(tok->kind) {
        case TOKEN_PLUS:
            return OP_ADD;
        case TOKEN_MINUS:
            return OP_SUB;
        case TOKEN_ASTERISK:
            return OP_MUL;
        case TOKEN_FSLASH:
            return OP_DIV;
        case TOKEN_CARAT:
            return OP_EXPO;

        case TOKEN_EQ:
            return OP_EQ;
        case TOKEN_NOT_EQ:
            return OP_NE;
        case TOKEN_LESSER_THAN:
            return OP_LT;
        case TOKEN_LESSER_THAN_EQ:
            return OP_LE;
        case TOKEN_GREATER_THAN:
            return OP_GT;
        case TOKEN_GREATER_THAN_EQ:
            return OP_GE;

        case TOKEN_AND:
            return OP_AND;
        case TOKEN_OR:
            return OP_OR;

        case TOKEN_CONCAT:
            return OP_CONCAT;
        default:
            return NO_INFIX;
    }
}

static Token* consume_token_and_clone(Parser* parser, TokenKind expected) {
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

static bool consume_token(Parser* parser, TokenKind expected) {
    if(parser->curr_token->kind != expected) {
        return false;
    }
    advance_parser(parser);
    return true;
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

// this is serves no real purpose except making scopes clear while printing the ast
static void maybe_give_scope_a_name(ASTNode* lhs, ASTNode* rhs) {
    bool needs_free = false;
    char* ident = get_name_val_from_node(lhs, &needs_free);
    if(rhs->kind == ASTNODE_FUNC_EXPR) {
        char* scope_name = malloc(strlen("FUNC_INDEX_") + strlen(ident) + 1);
        sprintf(scope_name, "FUNC_INDEX_%s", ident);
        (&rhs->func_expr)->scope->name = scope_name;
    } else if(rhs->kind == ASTNODE_TABLE_LITERAL) {
        char* scope_name = malloc(strlen("TABLE_INDEX_") + strlen(ident) + 1);
        sprintf(scope_name, "TABLE_INDEX_%s", ident);
        (&rhs->table_literal)->scope->name = scope_name;
    }

    if(needs_free) {
        free(ident);
    }
}

static char* get_name_val_from_node(const ASTNode* node, bool* needs_free) {
    switch(node->kind) {
        case ASTNODE_SYMBOL:
            return node->symbol.name;
        case ASTNODE_STR_LITERAL:
            return node->str_literal.str_val;
        case ASTNODE_NUM_LITERAL:
            {
                char* buf = malloc(50 * sizeof(char));
                snprintf(buf, 50, "%f", node->num_literal.num_val);
                *needs_free = true;
                return buf;
            }
        case ASTNODE_INDEX_EXPR:
            return get_name_val_from_node(node->index_expr.expr, needs_free);
        default:
            return "I DO NOT KNOW";
    }
}
