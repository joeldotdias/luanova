#ifndef AST_H
#define AST_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define INDENT(depth)                                                                    \
    do {                                                                                 \
        for(size_t i = 0; i < (depth) * 2; i++) {                                        \
            printf(" ");                                                                 \
        }                                                                                \
    } while(0)

typedef enum {
    // top level
    ASTNODE_CHUNK,
    ASTNODE_BLOCK,

    // statements
    ASTNODE_ASSIGN_STMT,
    ASTNODE_FUNC_CALL_STMT,
    ASTNODE_DO_STMT,
    ASTNODE_WHILE_STMT,
    ASTNODE_REPEAT_STMT,
    ASTNODE_IF_STMT,
    ASTNODE_NUMERIC_STMT,
    ASTNODE_GENERIC_STMT,
    ASTNODE_FUNC_DECLR,
    ASTNODE_LOCAL_FUNC_DECLR,
    ASTNODE_LOCAL_VAR_DECLR,
    ASTNODE_RETURN_STMT,
    ASTNODE_BREAK_STMT,

    // expressions
    ASTNODE_NIL_LITERAL,
    ASTNODE_BOOL_LITERAL,
    ASTNODE_NUM_LITERAL,
    ASTNODE_STR_LITERAL,
    ASTNODE_VARARG_EXPR,
    ASTNODE_FUNC_EXPR,
    ASTNODE_PREFIX_EXPR,
    ASTNODE_TABLE_CONSTRUCTOR,
    ASTNODE_BINARY_EXPR,
    ASTNODE_UNARY_EXPR,

    // variable stuff
    ASTNODE_VAR,
    ASTNODE_INDEXED_VAR,
    ASTNODE_FIELD_VAR,

    // function stuff
    ASTNODE_FUNC_NAME,
    ASTNODE_PARAMS,
    ASTNODE_ARGS,

    // table related
    ASTNODE_TABLE_FIELD,
    ASTNODE_TABLE_INDEXED_FIELD,
    ASTNODE_TABLE_NAMED_FIELD,
} NodeKind;

typedef enum {
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MODULO,
    OP_EXPO,

    OP_LT,
    OP_LE,
    OP_GT,
    OP_GE,
    OP_EQ,
    OP_NE,

    OP_AND,
    OP_OR,

    OP_CONCAT,
} BinaryOperator;

typedef enum {
    OP_NEGATE,
    OP_NOT,
    OP_LENGTH,
} UnaryOperator;

typedef struct ASTNode ASTNode;

typedef struct {
    ASTNode** stmteez;
    size_t stmt_count;
    ASTNode* last_stmt;
} Chunk;

typedef struct {
    struct ASTNode* var_list;
    struct ASTNode* expr_list;
} Assignment;

typedef struct {
    ASTNode* cond;
    ASTNode* body;
} WhileStmt;

typedef struct {
    ASTNode* body;
    ASTNode* cond;
} RepeatStmt;

typedef struct {
    ASTNode* cond;
    ASTNode* then_block;
    ASTNode** elseif_conds;
    ASTNode** elseif_blocks;
    size_t elseif_count;
    ASTNode* else_block;
} IfStmt;

typedef struct {
    char* var;
    ASTNode* start;
    ASTNode* end;
    ASTNode* step;
    ASTNode* body;
} ForNumeric;

typedef struct {
    ASTNode* name_list;
    ASTNode* expr_list;
    ASTNode* body;
} ForGeneric;

typedef struct {
    ASTNode* name;
    ASTNode* params;
    ASTNode* body;
    bool is_method;
    bool is_local;
} FuncDecl;

typedef struct {
    char* name;
    ASTNode* prefix;
    ASTNode** args;
} FuncCall;

typedef struct {
    ASTNode* return_val;
} ReturnStmt;

typedef struct {
    char* name;
    bool is_local;
} VarNode;

typedef struct {
    ASTNode* prefix;
    ASTNode* index;
} IndexedVar;

typedef struct {
    ASTNode* left;
    BinaryOperator op;
    ASTNode* right;
} BinaryExpr;

typedef struct {
    UnaryOperator op;
    ASTNode* operand;
} UnaryExpr;

typedef struct {
    double num_val;
} NumLiteral;

typedef struct {
    char* str_val;
} StrLiteral;

typedef struct {
    bool bool_val;
} BoolLiteral;

typedef struct {
    ASTNode** fields;
    size_t field_count;
} TableConstructor;

typedef struct {
    ASTNode* key;
    ASTNode* val;
} TableField;

struct ASTNode {
    NodeKind kind;

    union {
        Chunk chunk;
        Assignment assignment;
        WhileStmt while_stmt;
        RepeatStmt reapeat_stmt;
        IfStmt if_stmt;
        ForNumeric for_numeric;
        ForGeneric for_generic;
        FuncDecl func_decl;
        FuncCall func_call;
        ReturnStmt return_stmt;
        VarNode var_node;
        IndexedVar indexed_var;
        BinaryExpr binary_expr;
        UnaryExpr unary_expr;
        NumLiteral num_literal;
        StrLiteral str_literal;
        BoolLiteral bool_literal;
    };
};

void print_ast(ASTNode* root);

#endif
