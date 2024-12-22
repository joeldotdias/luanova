#ifndef EVAL_H
#define EVAL_H

#include "ast.h"
#include "hashtab.h"
#include <stdlib.h>

struct Environment {
    HashTable* lookup;
    struct Environment* outer_env;
};

typedef struct Environment Environment;

typedef struct {
    Environment* env;
} Eval;

typedef struct {
    SymbolList* params;
    Chunk* body;
    Environment* environemnt;
} FuncVal;

typedef enum {
    BUILTIN_PRINT,
} BuiltinFunc;

typedef struct {
    enum {
        OBJECT_NUMBER,
        OBJECT_STRING,
        OBJECT_BOOLEAN,
        OBJECT_TABLE,
        OBJECT_FUNCTION,
        OBJECT_BUILTIN,
        OBJECT_NIL,
    } kind;
    union {
        double num_val;
        char* str_val;
        bool bool_val;
        FuncVal* func_val;
        BuiltinFunc builtin_func;
    };
} Object;

Eval* init_eval();
void eval_program(Eval* e, ASTNode* root);
Environment* make_env(Environment* outer);
Object* look_in_env(Environment* env, const char* key);
void env_upsert(Environment* env, const char* key, const Object* obj);
void remove_from_env(Environment* env, const char* key);
void show_env(Environment* env);
void annihilate_env(Environment* env);
void annihilate_eval(Eval* e);

typedef struct {
    Object** objects;
    size_t capacity;
} ObjectList;

ObjectList* objects_from_nodes(Eval* e, ASTNodeList* node_list);

#endif
