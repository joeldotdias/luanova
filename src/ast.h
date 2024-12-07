#ifndef AST_H
#define AST_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define INDENTED(depth, format, ...)                                                     \
    do {                                                                                 \
        for(size_t i = 0; i < (depth) * 2; i++) {                                        \
            printf(" ");                                                                 \
        }                                                                                \
        printf(format "\n", ##__VA_ARGS__);                                              \
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
    ASTNODE_SYMBOL,
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
typedef struct Scope Scope;
typedef struct Symbol Symbol;

typedef struct {
    ASTNode** nodes;
    size_t count, capacity;
} ASTNodeList;

typedef struct {
    Symbol** symbols;
    size_t count, capacity;
} SymbolList;

SymbolList* init_symbol_list();
void add_to_symbol_list(SymbolList* list, Symbol* symbol);
void print_symbol_list(const SymbolList* list);
ASTNodeList* init_ast_node_list();
void add_to_ast_node_list(ASTNodeList* list, ASTNode* node);

struct Scope {
    ASTNode* function; // type FuncExpr
    Scope* parent;     // NULL if belongs to main chunk
    char* name;        // really stupid
    SymbolList* symbol_lookup;
    bool must_be_closed; // if vars are in a closure, maybe move them to the heap
};

typedef struct {
    ASTNodeList* stmteez;
    // ASTNode* last_stmt;
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
    Symbol* name;
    Scope* scope;
    SymbolList* params;
    ASTNode* body;
    bool is_method;
    bool is_local;
} FuncExpr;

typedef struct {
    char* name;
    ASTNode* prefix;
    ASTNodeList* args;
} FuncCall;

typedef struct {
    ASTNode* return_val;
} ReturnStmt;

struct Symbol {
    char* name;
    Scope* scope;
    bool is_local;
    enum {
        SYM_LOCAL,
        SYM_GLOBAL,
        SYM_PARAM,
        SYM_LABEL,
    } symbol_kind;
};

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
        FuncExpr func_decl;
        FuncCall func_call;
        ReturnStmt return_stmt;
        Symbol symbol;
        IndexedVar indexed_var;
        BinaryExpr binary_expr;
        UnaryExpr unary_expr;
        NumLiteral num_literal;
        StrLiteral str_literal;
        BoolLiteral bool_literal;
    };
};

void ast_dump(ASTNode* root);
char* node_to_str(ASTNode* node);

#endif
