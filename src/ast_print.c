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
    INFO("=====AST=====");
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
            INFO("RECV %s", node_to_str(node));
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

    if(func_call->args) {
        INDENTED(indent + 1, "ARGS:");
        for(size_t i = 0; i < func_call->args->count; i++) {
            print_ast_node(func_call->args->nodes[i], indent + 2);
        }
    }
    /* for(ASTNode** arg = func_call->args; *arg != NULL; arg++) { */
    /*     print_ast_node(*arg, indent + 2); */
    /* } */
}

static void print_func_expr(FuncExpr* func_expr, size_t indent) {
    INDENTED(indent, "FUNCTION<%s>:", func_expr->scope ? func_expr->scope->name : "");
    INDENTED(indent + 1, "NAME: %s", func_expr->name->name);
    if(func_expr->params) {
        INDENTED(indent + 1, "PARAMS:");
        for(size_t i = 0; i < func_expr->params->count; i++) {
            print_symbol(func_expr->params->symbols[i], indent + 2);
        }
    }
    INDENTED(indent + 1, "BODY:");
    print_ast_node(func_expr->body, indent + 2);
}

static void print_symbol(Symbol* symbol, size_t indent) {
    if(symbol->scope) {
        INDENTED(indent, "SYMBOL<%s>: %s ", symbol->scope->name, symbol->name);
    } else {
        INDENTED(indent, "SYMBOL: %s ", symbol->name);
    }
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

        // function stuff
        case ASTNODE_FUNC_NAME:
            return "ASTNODE_FUNC_NAME";
        case ASTNODE_PARAMS:
            return "ASTNODE_PARAMS";
        case ASTNODE_ARGS:
            return "ASTNODE_ARGS";

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
