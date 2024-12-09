#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "shared.h"

#define COLOR_RESET "\x1b[0m"
#define COLOR_KEY "\x1b[1;36m"
#define COLOR_SYMBOL "\x1b[1;33m"
#define COLOR_LITERAL "\x1b[1;31m"
#define COLOR_NAME "\x1b[1;34m"

static void print_chunk(Chunk* chunk, size_t indent);
static void print_assignment(Assignment* asgmt, size_t indent);
static void print_func_stmt(FuncStmt* func_stmt, size_t indent);
static void print_func_expr(FuncExpr* func_expr, size_t indent);
static void print_func_call(FuncCall* func_call, size_t indent);
static void print_symbol(Symbol* var, size_t indent);
static void print_str_literal(StrLiteral* str, size_t indent);
static const char* binary_op_str(BinaryOperator op);
static const char* unary_op_str(UnaryOperator op);

// helpers to display strings with escape sequences
// these don't change the actual representation of the parsed string
// they only produce a raw escaped string by escaping the escapes
// so as to not mess up the indenting while dipslaying
static char* escaped_str_to_display(const char* str);
static size_t find_escaped_len(const char* str);

void ast_dump(ASTNode* root) {
    INFO("=====AST=====");
    print_ast_node(root, 0);
}

void print_ast_node(ASTNode* node, size_t indent) {
    if(!node) {
        return;
    }

    switch(node->kind) {
        case ASTNODE_CHUNK:
            print_chunk(&node->chunk, indent);
            break;
        case ASTNODE_ASSIGN_STMT:
        case ASTNODE_LOCAL_VAR_DECLR:
            print_assignment(&node->assignment, indent);
            break;
        case ASTNODE_FUNC_CALL_STMT:
            print_func_call(&node->func_call, indent);
            break;
        case ASTNODE_SYMBOL:
            print_symbol(&node->symbol, indent);
            break;
        case ASTNODE_STR_LITERAL:
            print_str_literal(&node->str_literal, indent);
            break;
        case ASTNODE_FUNC_EXPR:
            print_func_expr(&node->func_expr, indent);
            break;
        case ASTNODE_FUNC_STMT:
            print_func_stmt(&node->func_stmt, indent);
            break;
        default:
            INFO("RECV %s", node_to_str(node));
            UNIMPLEMENTED();
    }
}

static void print_chunk(Chunk* chunk, size_t indent) {
    INDENTED(indent, COLOR_KEY "CHUNK:" COLOR_RESET);
    for(size_t i = 0; i < chunk->stmteez->count; i++) {
        print_ast_node(chunk->stmteez->nodes[i], indent + 1);
    }
}

static void print_assignment(Assignment* asgmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "ASSIGNMENT:" COLOR_RESET);

    for(size_t i = 0; i < asgmt->var_list->count; i++) {
        print_symbol(asgmt->var_list->symbols[i], indent + 1);
        INDENTED(indent + 1, COLOR_KEY "VALUE:" COLOR_RESET);
        print_ast_node(asgmt->expr_list->nodes[i], indent + 2);
    }
}

static void print_func_call(FuncCall* func_call, size_t indent) {
    INDENTED(indent, COLOR_KEY "FUNCTION CALL:" COLOR_RESET);
    INDENTED(indent + 1, COLOR_KEY "NAME: " COLOR_NAME "%s" COLOR_RESET, func_call->name);

    if(func_call->prefix) {
        INDENTED(indent + 1, COLOR_KEY "PREFIX:" COLOR_RESET);
        print_ast_node(func_call->prefix, indent + 2);
    }

    if(func_call->args) {
        INDENTED(indent + 1, COLOR_KEY "ARGS:" COLOR_RESET);
        for(size_t i = 0; i < func_call->args->count; i++) {
            print_ast_node(func_call->args->nodes[i], indent + 2);
        }
    }
}

static void print_func_expr(FuncExpr* func_expr, size_t indent) {
    INDENTED(indent, COLOR_KEY "FUNC EXPR:");
    if(func_expr->params) {
        INDENTED(indent + 1, COLOR_KEY "PARAMS:" COLOR_RESET);
        for(size_t i = 0; i < func_expr->params->count; i++) {
            print_symbol(func_expr->params->symbols[i], indent + 2);
        }
    }
    INDENTED(indent + 1, COLOR_KEY "BODY:" COLOR_RESET);
    print_ast_node(func_expr->body, indent + 2);
}

static void print_func_stmt(FuncStmt* func_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "FUNCTION<%s>:" COLOR_RESET,
             func_stmt->func_expr->scope ? func_stmt->func_expr->scope->name : "");

    INDENTED(indent + 1, COLOR_KEY "NAME: " COLOR_NAME "%s" COLOR_RESET,
             func_stmt->name->name);

    print_func_expr(func_stmt->func_expr, indent + 1);
}

static void print_symbol(Symbol* symbol, size_t indent) {
    if(symbol->scope) {
        INDENTED(indent, COLOR_SYMBOL "SYMBOL<%s>: %s" COLOR_RESET, symbol->scope->name,
                 symbol->name);
    } else {
        INDENTED(indent, COLOR_SYMBOL "SYMBOL:%s" COLOR_RESET, symbol->name);
    }
}

static void print_str_literal(StrLiteral* str, size_t indent) {
    char* escaped_str = escaped_str_to_display(str->str_val);
    INDENTED(indent, COLOR_KEY "STRING: " COLOR_LITERAL "%s" COLOR_RESET, escaped_str);
    free(escaped_str);
}

static const char* binary_op_str(BinaryOperator op) {
    switch(op) {
        case OP_ADD:
            return "+";
        case OP_SUB:
            return "-";
        case OP_MUL:
            return "*";
        case OP_DIV:
            return "/";
        case OP_MODULO:
            return "%";
        case OP_EXPO:
            return "^";
        case OP_LT:
            return "<";
        case OP_LE:
            return "<=";
        case OP_GT:
            return ">";
        case OP_GE:
            return ">=";
        case OP_EQ:
            return "==";
        case OP_NE:
            return "~=";
        case OP_AND:
            return "and";
        case OP_OR:
            return "or";
        case OP_CONCAT:
            return "..";
        default:
            return "UNKNOWN_OP";
    }
}

static const char* unary_op_str(UnaryOperator op) {
    switch(op) {
        case OP_NEGATE:
            return "-";
        case OP_NOT:
            return "not ";
        case OP_LENGTH:
            return "#";
        default:
            return "UNKNOWN_OP";
    }
}

static char* escaped_str_to_display(const char* str) {
    size_t len = strlen(str);
    size_t extra_chars = find_escaped_len(str);

    char* escaped = malloc(len + extra_chars + 1);
    char* display_str = escaped;

    while(*str) {
        switch(*str) {
            case '\n':
                *display_str++ = '\\';
                *display_str++ = 'n';
                break;
            case '\r':
                *display_str++ = '\\';
                *display_str++ = 'r';
                break;
            case '\t':
                *display_str++ = '\\';
                *display_str++ = 't';
                break;
            case '\\':
                *display_str++ = '\\';
                *display_str++ = '\\';
                break;
            case '"':
                *display_str++ = '\\';
                *display_str++ = '"';
                break;
            default:
                *display_str++ = *str;
        }
        str++;
    }
    *display_str = '\0';

    return escaped;
}

static size_t find_escaped_len(const char* str) {
    size_t extra_chars = 0;
    while(*str) {
        switch(*str) {
            case '\n':
            case '\r':
            case '\t':
            case '\\':
            case '"':
                extra_chars++;
                break;
        }
        str++;
    }

    return extra_chars;
}

char* node_to_str(ASTNode* node) {
    switch(node->kind) {
        case ASTNODE_CHUNK:
            return "ASTNODE_CHUNK";
        case ASTNODE_BLOCK:
            return "ASTNODE_BLOCK";

        // statements
        case ASTNODE_ASSIGN_STMT:
            return "ASTNODE_ASSIGN_STMT";
        case ASTNODE_FUNC_CALL_STMT:
            return "ASTNODE_FUNC_CALL_STMT";
        case ASTNODE_DO_STMT:
            return "ASTNODE_DO_STMT";
        case ASTNODE_WHILE_STMT:
            return "ASTNODE_WHILE_STMT";
        case ASTNODE_REPEAT_STMT:
            return "ASTNODE_REPEAT_STMT";
        case ASTNODE_IF_STMT:
            return "ASTNODE_IF_STMT";
        case ASTNODE_NUMERIC_STMT:
            return "ASTNODE_NUMERIC_STMT";
        case ASTNODE_GENERIC_STMT:
            return "ASTNODE_GENERIC_STMT";
        case ASTNODE_FUNC_DECLR:
            return "ASTNODE_FUNC_DECLR";
        case ASTNODE_LOCAL_FUNC_DECLR:
            return "ASTNODE_LOCAL_FUNC_DECLR";
        case ASTNODE_LOCAL_VAR_DECLR:
            return "ASTNODE_LOCAL_VAR_DECLR";
        case ASTNODE_RETURN_STMT:
            return "ASTNODE_RETURN_STMT";
        case ASTNODE_BREAK_STMT:
            return "ASTNODE_BREAK_STMT";
        case ASTNODE_FUNC_STMT:
            return "ASTNODE_FUNC_STMT";

        // expressions
        case ASTNODE_NIL_LITERAL:
            return "ASTNODE_NIL_LITERAL";
        case ASTNODE_BOOL_LITERAL:
            return "ASTNODE_BOOL_LITERAL";
        case ASTNODE_NUM_LITERAL:
            return "ASTNODE_NUM_LITERAL";
        case ASTNODE_STR_LITERAL:
            return "ASTNODE_STR_LITERAL";
        case ASTNODE_VARARG_EXPR:
            return "ASTNODE_VARARG_EXPR";
        case ASTNODE_FUNC_EXPR:
            return "ASTNODE_FUNC_EXPR";
        case ASTNODE_PREFIX_EXPR:
            return "ASTNODE_PREFIX_EXPR";
        case ASTNODE_TABLE_CONSTRUCTOR:
            return "ASTNODE_TABLE_CONSTRUCTOR";
        case ASTNODE_BINARY_EXPR:
            return "ASTNODE_BINARY_EXPR";
        case ASTNODE_UNARY_EXPR:
            return "ASTNODE_UNARY_EXPR";

        // variable stuff
        case ASTNODE_SYMBOL:
            return "ASTNODE_SYMBOL";
        case ASTNODE_INDEXED_VAR:
            return "ASTNODE_INDEXED_VAR";
        case ASTNODE_FIELD_VAR:
            return "ASTNODE_FIELD_VAR";

        // table related
        case ASTNODE_TABLE_FIELD:
            return "ASTNODE_TABLE_FIELD";
        case ASTNODE_TABLE_INDEXED_FIELD:
            return "ASTNODE_TABLE_INDEXED_FIELD";
        case ASTNODE_TABLE_NAMED_FIELD:
            return "ASTNODE_TABLE_NAMED_FIELD";

        default:
            return "UNKNOWN_NODE_KIND";
    }
}
