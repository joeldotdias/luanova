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
#define COLOR_OPERATOR "\x1b[1;35m"

static void print_chunk(Chunk* chunk, size_t indent);
static void print_assignment(LocalAssignment* asgmt, size_t indent);
static void print_func_stmt(FuncStmt* func_stmt, size_t indent);
static void print_if_stmt(IfStmt* if_stmt, size_t indent);
static void print_while_stmt(WhileStmt* while_stmt, size_t indent);
static void print_for_stmt(ForStmt* for_stmt, size_t indent);
static void print_return_stmt(ReturnStmt* ret_stmt, size_t indent);
static void print_expr_stmt(ExprStmt* expr_stmt, size_t indent);
static void print_func_expr(FuncExpr* func_expr, size_t indent);
static void print_func_call(FuncCall* func_call, size_t indent);
static void print_cond_then_block(CondThenBlock* block, size_t indent);
static void print_table_literal(TableLiteralExpr* table, size_t indent);
static void print_table_element(TableElement* elem, size_t indent);
static void print_index_expr(IndexExpr* index_expr, size_t indent);
static void print_field_selector(IndexExpr* index_expr, size_t indent);
static void print_suffixed_expr(SuffixedExpr* suffixed_expr, size_t indent);
static void print_unary_expr(UnaryExpr* expr, size_t indent);
static void print_binary_expr(BinaryExpr* expr, size_t indent);
static void print_symbol(Symbol* var, size_t indent);
static void print_str_literal(StrLiteral* str, size_t indent);
static void print_num_literal(NumLiteral* num, size_t indent);
static const char* binary_op_str(InfixOperator op);
static const char* unary_op_str(PrefixOperator op);

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
        case ASTNODE_LOCAL_VAR_DECL:
            print_assignment(&node->assignment, indent);
            break;
        case ASTNODE_FUNC_STMT:
            print_func_stmt(&node->func_stmt, indent);
            break;
        case ASTNODE_IF_STMT:
            print_if_stmt(&node->if_stmt, indent);
            break;
        case ASTNODE_WHILE_STMT:
            print_while_stmt(&node->while_stmt, indent);
            break;
        case ASTNODE_FOR_NUMERIC_STMT:
            print_for_stmt(&node->for_stmt, indent);
            break;
        case ASTNODE_RETURN_STMT:
            print_return_stmt(&node->return_stmt, indent);
            break;
        case ASTNODE_EXPR_STMT:
            print_expr_stmt(&node->expr_stmt, indent);
            break;
        case ASTNODE_FUNC_EXPR:
            print_func_expr(&node->func_expr, indent);
            break;
        case ASTNODE_COND_THEN_BLOCK:
            print_cond_then_block(&node->cond_then_block, indent);
            break;
        case ASTNODE_FUNC_CALL_EXPR:
            print_func_call(&node->func_call, indent);
            break;
        case ASTNODE_TABLE_LITERAL:
            print_table_literal(&node->table_literal, indent);
            break;
        case ASTNODE_TABLE_ELEMENT:
            print_table_element(&node->table_elem, indent);
            break;
        case ASTNODE_INDEX_EXPR:
            print_index_expr(&node->index_expr, indent);
            break;
        case ASTNODE_FIELD_SELECTOR:
            print_field_selector(&node->index_expr, indent);
            break;
        case ASTNODE_SUFFIXED_EXPR:
            print_suffixed_expr(&node->suffixed_expr, indent);
            break;
        case ASTNODE_BINARY_EXPR:
            print_binary_expr(&node->binary_expr, indent);
            break;
        case ASTNODE_UNARY_EXPR:
            print_unary_expr(&node->unary_expr, indent);
            break;
        case ASTNODE_SYMBOL:
            print_symbol(&node->symbol, indent);
            break;
        case ASTNODE_STR_LITERAL:
            print_str_literal(&node->str_literal, indent);
            break;
        case ASTNODE_NUM_LITERAL:
            print_num_literal(&node->num_literal, indent);
            break;
        default:
            INFO("RECV %s", node_to_str(node));
            UNIMPLEMENTED();
    }
}

static void print_chunk(Chunk* chunk, size_t indent) {
    INDENTED(indent, COLOR_KEY "CHUNK:");
    for(size_t i = 0; i < chunk->stmteez->count; i++) {
        print_ast_node(chunk->stmteez->nodes[i], indent + 1);
    }
}

static void print_assignment(LocalAssignment* asgmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "ASSIGNMENT:");

    for(size_t i = 0; i < asgmt->var_list->count; i++) {
        print_symbol(asgmt->var_list->symbols[i], indent + 1);
        INDENTED(indent + 1, COLOR_KEY "VALUE:");
        print_ast_node(asgmt->expr_list->nodes[i], indent + 2);
    }
}

static void print_func_stmt(FuncStmt* func_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "FUNCTION<%s>:",
             func_stmt->name->scope ? func_stmt->name->scope->name : "");

    INDENTED(indent + 1, COLOR_KEY "NAME: " COLOR_NAME "%s", func_stmt->name->name);

    print_ast_node(func_stmt->func_expr, indent + 1);
}

static void print_if_stmt(IfStmt* if_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "CONDITIONAL:");
    for(size_t i = 0; i < if_stmt->if_cond_then_blocks->count; i++) {
        if(i == 0) {
            INDENTED(indent + 1, COLOR_KEY "IF:");
        } else {
            INDENTED(indent + 1, COLOR_KEY "ELSE IF:");
        }
        print_ast_node(if_stmt->if_cond_then_blocks->nodes[i], indent + 2);
    }

    if(if_stmt->else_body) {
        INDENTED(indent + 1, COLOR_KEY "ELSE:");
        print_ast_node(if_stmt->else_body, indent + 2);
    }
}

static void print_while_stmt(WhileStmt* while_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "WHILE:");
    INDENTED(indent + 1, COLOR_KEY "COND:");
    print_ast_node(while_stmt->cond, indent + 2);
    INDENTED(indent + 1, COLOR_KEY "BODY:");
    print_ast_node(while_stmt->body, indent + 2);
}

static void print_for_stmt(ForStmt* for_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "FOR:");
    INDENTED(indent + 1, COLOR_KEY "START:");
    print_ast_node(for_stmt->start, indent + 2);
    INDENTED(indent + 1, COLOR_KEY "END:");
    print_ast_node(for_stmt->end, indent + 2);
    if(for_stmt->step) {
        INDENTED(indent + 1, COLOR_KEY "STEP:");
        print_ast_node(for_stmt->step, indent + 2);
    }

    INDENTED(indent + 1, COLOR_KEY "BODY:");
    print_ast_node(for_stmt->body, indent + 2);
}

static void print_return_stmt(ReturnStmt* ret_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "RETURN:");
    print_ast_node(ret_stmt->return_val, indent + 1);
}

static void print_expr_stmt(ExprStmt* expr_stmt, size_t indent) {
    INDENTED(indent, COLOR_KEY "EXPRESSION STMT:");
    if(expr_stmt->var_expr_list != NULL) {
        INDENTED(indent + 1, COLOR_KEY "LHS:");
        for(size_t i = 0; i < expr_stmt->var_expr_list->count; i++) {
            print_ast_node(expr_stmt->var_expr_list->nodes[i], indent + 2);
        }
        INDENTED(indent + 1, COLOR_KEY "RHS:");
    }

    for(size_t i = 0; i < expr_stmt->expr_list->count; i++) {
        print_ast_node(expr_stmt->expr_list->nodes[i], indent + 2);
    }
}

static void print_func_expr(FuncExpr* func_expr, size_t indent) {
    INDENTED(indent, COLOR_KEY "FUNC EXPR:");
    if(func_expr->params) {
        INDENTED(indent + 1, COLOR_KEY "PARAMS:");
        for(size_t i = 0; i < func_expr->params->count; i++) {
            print_symbol(func_expr->params->symbols[i], indent + 2);
        }
    }
    INDENTED(indent + 1, COLOR_KEY "BODY:");
    print_ast_node(func_expr->body, indent + 2);
}

static void print_func_call(FuncCall* func_call, size_t indent) {
    INDENTED(indent, COLOR_KEY "FUNCTION CALL:");
    if(func_call->method_name) {
        INDENTED(indent + 1, COLOR_KEY "METHOD: " COLOR_LITERAL "%s",
                 func_call->method_name);
    }

    if(func_call->args) {
        INDENTED(indent + 1, COLOR_KEY "ARGS:");
        for(size_t i = 0; i < func_call->args->count; i++) {
            print_ast_node(func_call->args->nodes[i], indent + 2);
        }
    }
}

static void print_cond_then_block(CondThenBlock* block, size_t indent) {
    INDENTED(indent, COLOR_KEY "COND:");
    print_ast_node(block->cond, indent + 1);
    INDENTED(indent, COLOR_KEY "BODY:");
    for(size_t i = 0; i < block->body->chunk.stmteez->count; i++) {
        print_ast_node(block->body->chunk.stmteez->nodes[i], indent + 2);
    }
}

static void print_table_literal(TableLiteralExpr* table, size_t indent) {
    INDENTED(indent, COLOR_KEY "TABLE:");
    for(size_t i = 0; i < table->expr_list->count; i++) {
        print_ast_node(table->expr_list->nodes[i], indent + 1);
    }
}

static void print_table_element(TableElement* elem, size_t indent) {
    INDENTED(indent, COLOR_KEY "TABLE ELEMENT:");
    if(elem->key) {
        INDENTED(indent + 1, COLOR_KEY "KEY:");
        print_ast_node(elem->key, indent + 2);
        INDENTED(indent + 1, COLOR_KEY "VALUE: ");
        print_ast_node(elem->value, indent + 2);
    } else {
        print_ast_node(elem->value, indent + 1);
    }
}

static void print_index_expr(IndexExpr* index_expr, size_t indent) {
    INDENTED(indent, COLOR_KEY "INDEX:");
    print_ast_node(index_expr->expr, indent + 1);
}

static void print_field_selector(IndexExpr* index_expr, size_t indent) {
    INDENTED(indent, COLOR_KEY "FIELD: " COLOR_LITERAL "%s",
             index_expr->expr->str_literal.str_val);
}

static void print_suffixed_expr(SuffixedExpr* suffixed_expr, size_t indent) {
    if(!suffixed_expr->suffix_list) {
        print_ast_node(suffixed_expr->primary_expr, indent);
        return;
    }

    INDENTED(indent, COLOR_KEY "SUFFIXED:");
    print_ast_node(suffixed_expr->primary_expr, indent + 1);

    if(suffixed_expr->suffix_list != NULL) {
        INDENTED(indent + 1, COLOR_KEY "SUFFIX LIST:");
        for(size_t i = 0; i < suffixed_expr->suffix_list->count; i++) {
            print_ast_node(suffixed_expr->suffix_list->nodes[i], indent + 2);
        }
    }
}

static void print_binary_expr(BinaryExpr* expr, size_t indent) {
    INDENTED(indent, COLOR_KEY "BINARY:");
    INDENTED(indent + 1, COLOR_KEY "OPERATOR: " COLOR_OPERATOR "%s",
             binary_op_str(expr->op));
    INDENTED(indent + 1, COLOR_KEY "LEFT:");
    print_ast_node(expr->left, indent + 2);
    INDENTED(indent + 1, COLOR_KEY "RIGHT:");
    print_ast_node(expr->right, indent + 2);
}

static void print_unary_expr(UnaryExpr* expr, size_t indent) {
    INDENTED(indent, COLOR_KEY "UNARY:");
    INDENTED(indent + 1, COLOR_KEY "OPERATOR: " COLOR_OPERATOR "%s",
             unary_op_str(expr->op));
    INDENTED(indent + 1, COLOR_KEY "OPERAND:");
    print_ast_node(expr->operand, indent + 2);
}

static void print_symbol(Symbol* symbol, size_t indent) {
    if(symbol->scope) {
        INDENTED(indent, COLOR_SYMBOL "SYMBOL<%s>: %s", symbol->scope->name,
                 symbol->name);
    } else {
        INDENTED(indent, COLOR_SYMBOL "SYMBOL:%s", symbol->name);
    }
}

static void print_str_literal(StrLiteral* str, size_t indent) {
    char* escaped_str = escaped_str_to_display(str->str_val);
    INDENTED(indent, COLOR_KEY "STRING: " COLOR_LITERAL "%s", escaped_str);
    free(escaped_str);
}

static void print_num_literal(NumLiteral* num, size_t indent) {
    INDENTED(indent, COLOR_KEY "NUMBER: " COLOR_LITERAL "%f", num->num_val);
}

static const char* binary_op_str(InfixOperator op) {
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

static const char* unary_op_str(PrefixOperator op) {
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

char* node_to_str(const ASTNode* node) {
    switch(node->kind) {
        case ASTNODE_CHUNK:
            return "ASTNODE_CHUNK";
        // statements
        case ASTNODE_LOCAL_VAR_DECL:
            return "ASTNODE_LOCAL_VAR_DECL";
        case ASTNODE_IF_STMT:
            return "ASTNODE_IF_STMT";
        case ASTNODE_DO_STMT:
            return "ASTNODE_DO_STMT";
        case ASTNODE_WHILE_STMT:
            return "ASTNODE_WHILE_STMT";
        case ASTNODE_FOR_NUMERIC_STMT:
            return "ASTNODE_FOR_NUMERIC_STMT";
        case ASTNODE_FOR_GENERIC_STMT:
            return "ASTNODE_FOR_GENERIC_STMT";
        case ASTNODE_REPEAT_STMT:
            return "ASTNODE_REPEAT_STMT";
        case ASTNODE_BREAK_STMT:
            return "ASTNODE_BREAK_STMT";
        case ASTNODE_FUNC_STMT:
            return "ASTNODE_FUNC_STMT";
        case ASTNODE_FUNC_CALL_EXPR:
            return "ASTNODE_FUNC_CALL_STMT";
        case ASTNODE_RETURN_STMT:
            return "ASTNODE_RETURN_STMT";
        case ASTNODE_LABEL_STMT:
            return "ASTNODE_LABEL_STMT";
        case ASTNODE_GOTO_STMT:
            return "ASTNODE_GOTO_STMT";
        case ASTNODE_EXPR_STMT:
            return "ASTNODE_EXPR_STMT";
        // expressions
        case ASTNODE_NIL_LITERAL:
            return "ASTNODE_NIL_LITERAL";
        case ASTNODE_BOOL_LITERAL:
            return "ASTNODE_BOOL_LITERAL";
        case ASTNODE_NUM_LITERAL:
            return "ASTNODE_NUM_LITERAL";
        case ASTNODE_STR_LITERAL:
            return "ASTNODE_STR_LITERAL";
        case ASTNODE_INDEX_EXPR:
            return "ASTNODE_INDEX_EXPR";
        case ASTNODE_COND_THEN_BLOCK:
            return "ASTNODE_COND_THEN_BLOCK";
        case ASTNODE_VARARG_EXPR:
            return "ASTNODE_VARARG_EXPR";
        case ASTNODE_FUNC_EXPR:
            return "ASTNODE_FUNC_EXPR";
        case ASTNODE_BINARY_EXPR:
            return "ASTNODE_BINARY_EXPR";
        case ASTNODE_UNARY_EXPR:
            return "ASTNODE_UNARY_EXPR";
        case ASTNODE_BUILTIN_EXPR:
            return "ASTNODE_BUILTIN_EXPR";
        case ASTNODE_SUFFIXED_EXPR:
            return "ASTNODE_SUFFIXED_EXPR";
        // variable stuff
        case ASTNODE_SYMBOL:
            return "ASTNODE_SYMBOL";
        case ASTNODE_INDEXED_VAR:
            return "ASTNODE_INDEXED_VAR";
        case ASTNODE_FIELD_SELECTOR:
            return "ASTNODE_FIELD_SELECTOR";

        // table related
        case ASTNODE_TABLE_LITERAL:
            return "ASTNODE_TABLE_LITERAL";
        case ASTNODE_TABLE_ELEMENT:
            return "ASTNODE_TABLE_FIELD";

        default:
            return "UNKNOWN_NODE_KIND";
    }
}
