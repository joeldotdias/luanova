/* A lot of inspiration (technically just the suffixed expression type but that solved a
 * lotta problems) for how to structure the types in the AST has been taken from the
 * ravi-compiler. (https://github.com/dibyendumajumdar/ravi-compiler)
 */

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
        printf(format COLOR_RESET "\n", ##__VA_ARGS__);                                  \
    } while(0)

typedef enum {
    // top level
    ASTNODE_CHUNK,
    ASTNODE_BLOCK,

    // statements
    ASTNODE_LOCAL_VAR_DECL,
    ASTNODE_IF_STMT,
    ASTNODE_COND_THEN_BLOCK,
    ASTNODE_DO_STMT,
    ASTNODE_WHILE_STMT,
    ASTNODE_FOR_NUMERIC_STMT,
    ASTNODE_FOR_GENERIC_STMT, // for in
    ASTNODE_REPEAT_STMT,
    ASTNODE_BREAK_STMT,
    ASTNODE_FUNC_STMT,
    ASTNODE_RETURN_STMT,
    ASTNODE_LABEL_STMT,
    ASTNODE_GOTO_STMT,

    // expressions
    ASTNODE_EXPR_STMT,
    ASTNODE_REASSIGNMENT_EXPR,
    ASTNODE_FUNC_EXPR,
    ASTNODE_FUNC_CALL_EXPR,
    ASTNODE_INDEX_EXPR,
    ASTNODE_SUFFIXED_EXPR,
    ASTNODE_BINARY_EXPR,
    ASTNODE_UNARY_EXPR,
    ASTNODE_BUILTIN_EXPR,
    ASTNODE_VARARG_EXPR,
    ASTNODE_STR_LITERAL,
    ASTNODE_NUM_LITERAL,
    ASTNODE_BOOL_LITERAL,
    ASTNODE_NIL_LITERAL,

    // variable stuff
    ASTNODE_SYMBOL,
    ASTNODE_INDEXED_VAR,
    ASTNODE_FIELD_SELECTOR,

    // table related
    ASTNODE_TABLE_LITERAL,
    ASTNODE_TABLE_ELEMENT,
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

    NO_INFIX,
} InfixOperator;

typedef enum {
    OP_NEGATE,
    OP_NOT,
    OP_LENGTH,

    NO_PREFIX,
} PrefixOperator;

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

void ast_dump(ASTNode* root);
SymbolList* init_symbol_list();
void add_to_symbol_list(SymbolList* list, Symbol* symbol);
void print_symbol_list(const SymbolList* list);
ASTNodeList* init_ast_node_list();
// void add_to_ast_node_list(ASTNodeList* list, ASTNode* node);
void add_to_ast_node_list(ASTNodeList** list_ref, ASTNode* node);
void print_ast_node_list(const ASTNodeList* list);
void print_ast_node(ASTNode* node, size_t indent);
char* node_to_str(const ASTNode* node);

struct Scope {
    ASTNode* block;
    Scope* parent; // NULL if belongs to main chunk
    char* name;    // really stupid
    SymbolList* symbol_lookup;
    bool must_be_closed; // if vars are in a closure, maybe move them to the heap
};

typedef struct {
    ASTNodeList* stmteez;
} Chunk;

/* local a, b, c = "Hello", function() ... end, { ... } */
typedef struct {
    SymbolList* var_list;
    ASTNodeList* expr_list;
} LocalAssignment;

typedef struct {
    ASTNode* cond;
    ASTNode* body;
} WhileStmt;

typedef struct {
    ASTNode* body;
    ASTNode* cond;
} RepeatStmt;

typedef struct {
    ASTNodeList* if_cond_then_blocks; // type -> CondThenBlock
    Scope* else_scope;
    ASTNode* else_body; // type -> chunk
} IfStmt;

typedef struct {
    ASTNode* cond;
    ASTNode* body; // type -> chunk
    Scope* scope;
} CondThenBlock;

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
    ASTNodeList* var_expr_list;
    ASTNodeList* expr_list;
} ExprStmt;

/* a = "Hello" */
typedef struct {
    SymbolList* var_list;
    ASTNodeList* expr_list;
} ReassignExpr;

typedef struct {
    SymbolList* params;
    ASTNode* body;
    Scope* scope;
    bool is_method;
    bool is_local;
} FuncExpr;

typedef struct {
    Symbol* name;
    ASTNodeList* Selectors;
    // FuncExpr* func_expr;
    ASTNode* func_expr;
} FuncStmt;

typedef struct {
    ASTNodeList* args;
    const char* method_name; // if it is a method, else NULL
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

// typedef struct {
//     ASTNode* prefix;
//     ASTNode* index;
// } IndexedVar;

typedef struct {
    ASTNode* expr;
} IndexExpr;

/*
 * Quite integral and has several types
 * object.field -> { base: object; suffix: field_selector(field) }
 * array[5] -> { base: array; suffix: index_expr([5]) }
 * object.field -> { base: object; suffix: field_selector(field) }
 * object.field -> { base: object; suffix: field_selector(field) }
 * object.field -> { base: object; suffix: field_selector(field) }
 */
typedef struct {
    ASTNode* primary_expr;
    ASTNodeList* suffix_list;
} SuffixedExpr;

typedef struct {
    ASTNode* left;
    InfixOperator op;
    ASTNode* right;
} BinaryExpr;

typedef struct {
    PrefixOperator op;
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
    ASTNode* key; // might be null in case of a list
    ASTNode* value;
} TableElement;

typedef struct {
    Scope* scope;
    ASTNodeList* expr_list; // this will be of type TableElementAssignmnetExpr
} TableLiteralExpr;

typedef struct {
    ASTNode* key;
    ASTNode* val;
} TableField;

struct ASTNode {
    NodeKind kind;

    union {
        Chunk chunk;
        LocalAssignment assignment;
        WhileStmt while_stmt;
        RepeatStmt reapeat_stmt;
        IfStmt if_stmt;
        CondThenBlock cond_then_block;
        ForNumeric for_numeric;
        ForGeneric for_generic;
        FuncStmt func_stmt;
        FuncExpr func_expr;
        FuncCall func_call;
        ReassignExpr reassign_expr;
        TableLiteralExpr table_literal;
        TableElement table_elem;
        ReturnStmt return_stmt;
        Symbol symbol;
        IndexExpr index_expr;
        SuffixedExpr suffixed_expr;
        ExprStmt expr_stmt;
        // IndexedVar indexed_var;
        BinaryExpr binary_expr;
        UnaryExpr unary_expr;
        NumLiteral num_literal;
        StrLiteral str_literal;
        BoolLiteral bool_literal;
    };
};

#endif
