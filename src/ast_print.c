#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "shared.h"

static void print_ast_node(ASTNode* node, size_t indent);
static void print_chunk(Chunk* chunk, size_t indent);
static void print_assignment(Assignment* asgmt, size_t indent);
static void print_func_expr(FuncExpr* func_expr, size_t indent);
static void print_func_call(FuncCall* func_call, size_t indent);
static void print_symbol(Symbol* var, size_t indent);
static void print_str_literal(StrLiteral* str, size_t indent);
static const char* binary_op_str(BinaryOperator op);
static const char* unary_op_str(UnaryOperator op);

void ast_dump(ASTNode* root) {
    LOG_INFO("=====AST=====");
    print_ast_node(root, 0);
}

static void print_ast_node(ASTNode* node, size_t indent) {
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
            print_func_expr(&node->func_decl, indent);
            break;
        default:
            UNIMPLEMENTED();
    }
}

static void print_chunk(Chunk* chunk, size_t indent) {
    INDENTED(indent, "CHUNK:");
    for(size_t i = 0; i < chunk->stmt_count; i++) {
        print_ast_node(chunk->stmteez[i], indent + 1);
    }
}

static void print_assignment(Assignment* asgmt, size_t indent) {
    INDENTED(indent, "ASSIGNMENT:");

    print_ast_node(asgmt->var_list, indent + 1);
    INDENTED(indent + 1, "VALUE:");
    print_ast_node(asgmt->expr_list, indent + 2);
}

static void print_func_call(FuncCall* func_call, size_t indent) {
    INDENTED(indent, "FUNCTION CALL:");
    INDENTED(indent + 1, "NAME: %s", func_call->name);

    if(func_call->prefix) {
        INDENTED(indent + 1, "PREFIX: ");
        print_ast_node(func_call->prefix, indent + 2);
    }

    INDENTED(indent + 1, "ARGS:");
    for(ASTNode** arg = func_call->args; *arg != NULL; arg++) {
        print_ast_node(*arg, indent + 2);
    }
}

static void print_func_expr(FuncExpr* func_expr, size_t indent) {
    INDENTED(indent, "FUNCTION<%s>:", func_expr->scope ? func_expr->scope->name : "");
    INDENTED(indent + 1, "NAME: %s", func_expr->name->name);
    INDENTED(indent + 1, "BODY:");
    print_ast_node(func_expr->body, indent + 2);
}

static void print_symbol(Symbol* symbol, size_t indent) {
    if(symbol->scope) {

        INDENTED(indent, "VAR<%s>: %s ", symbol->scope->name, symbol->name);
    }
    INDENTED(indent, "VAR: %s ", symbol->name);
}

static void print_str_literal(StrLiteral* str, size_t indent) {
    INDENTED(indent, "STRING: %s", str->str_val);
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
