/* The entire purpose of this is just to have a function to quickly
 * run programs so I can make sure everything works as i take further
 * steps to simplify and lower the ast etc.
 */

#include "eval.h"
#include "ast.h"
#include "hashtab.h"
#include "shared.h"
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void eval_stmt(Eval* e, ASTNode* node);
static Object* atom_to_object(ASTNode* node);
static bool assert_binary_op_types(Object* left, Object* right, InfixOperator op);
static Object* eval_expression(Eval* e, ASTNode* expr);
static Object* eval_binary_expr(Object* left, Object* right, InfixOperator op);
static Object* eval_builtin(Object* builtin, ObjectList* args);
static Object* try_builtin(const char* name);
static bool is_truthy(Object* obj);
static void object_to_str(Object* obj);

Eval* init_eval() {
    Eval* e = malloc(sizeof(Eval));
    e->env = make_env(NULL);

    return e;
}

Environment* extend_func_env(const FuncVal* func_val, const ObjectList* args) {
    Environment* new_env = make_env(func_val->environemnt);
    if(args) {
        for(size_t i = 0; i < args->capacity; i++) {
            const char* param_name = func_val->params->symbols[i]->name;
            env_upsert(new_env, param_name, strlen(param_name) + 1, args->objects[i]);
        }
    }

    return new_env;
}

Object* eval_func_call(Eval* e, Object* func, ObjectList* args) {
    if(func->kind == OBJECT_BUILTIN) {
        return eval_builtin(func, args);
    }

    /* show_env(func->func_val->environemnt); */
    Environment* exteneded_env = extend_func_env(func->func_val, args);
    Environment* curr_env = e->env;
    Object* ret = malloc(sizeof(Object));
    ret->kind = OBJECT_NIL;
    e->env = exteneded_env;

    /* show_env(e->env); */

    for(size_t i = 0; i < func->func_val->body->stmteez->count; i++) {
        ASTNode* node = func->func_val->body->stmteez->nodes[i];
        if(node->kind == ASTNODE_RETURN_STMT) {
            ret = eval_expression(e, node->return_stmt.return_val);
            break;
        } else {
            eval_stmt(e, node);
        }
    }

    e->env = curr_env;

    return ret;
}

void print_table(TableVal* table);
Object* eval_suffixed_expr(Eval* e, SuffixedExpr* suffixed_expr) {
    ASTNode* primary = suffixed_expr->primary_expr;
    if(!suffixed_expr->suffix_list) {
        FATAL("Suffixed expr without suffix");
    }

    Object* initial = eval_expression(e, primary);
    if(!initial) {
        return NULL;
    }
    Object* res;

    for(size_t i = 0; i < suffixed_expr->suffix_list->count; i++) {
        ASTNode* suffix = suffixed_expr->suffix_list->nodes[i];
        switch(suffix->kind) {
            case ASTNODE_FUNC_CALL_EXPR:
                {
                    FuncCall func_call = suffix->func_call;
                    ObjectList* args = NULL;
                    if(func_call.args) {
                        args = objects_from_nodes(e, func_call.args);
                    }
                    res = eval_func_call(e, initial, args);
                    break;
                }
            case ASTNODE_INDEX_EXPR:
            case ASTNODE_FIELD_SELECTOR:
                {
                    TableVal* table = initial->table_val;
                    Object* idx = eval_expression(e, suffix->index_expr.expr);
                    // this is really bad. Find a better way
                    if(idx->kind == OBJECT_STRING) {
                        for(size_t i = 0; i < table->pairs->capacity; i++) {
                            HashEntry* entry = table->pairs->entries[i];
                            if(entry) {
                                Object* str_key = (Object*)entry->key;
                                if(str_key->kind != OBJECT_STRING) {
                                    continue;
                                }
                                if(strcmp(idx->str_val, str_key->str_val) == 0) {
                                    res = (Object*)entry->value;
                                    break;
                                }
                            }
                        }
                    } else {
                        res = ht_get(table->pairs, idx, sizeof(Object));
                    }

                    break;
                }
            default:
                UNIMPLEMENTED();
                INFO("Recvd suffix %s", node_to_str(suffix));
        }

        initial = res;
    }

    return res;
}

static Object* eval_func_expr(Eval* e, ASTNode* expr) {
    Object* func = malloc(sizeof(Object));
    func->kind = OBJECT_FUNCTION;
    func->func_val = malloc(sizeof(FuncVal));
    func->func_val->environemnt = e->env;
    func->func_val->params = expr->func_expr.params;
    func->func_val->body = &expr->func_expr.body->chunk;

    return func;
}
void print_table(TableVal* table) {
    if(!table || !table->pairs) {
        printf("nil\n");
        return;
    }

    printf("{\n");
    for(size_t i = 0; i < table->pairs->capacity; i++) {
        HashEntry* entry = table->pairs->entries[i];
        if(!entry || !entry->occupied)
            continue;

        // Print key
        Object* key = entry->key;
        printf("  [");
        if(key->kind == OBJECT_NUMBER) {
            printf("%.14g", key->num_val);
        } else if(key->kind == OBJECT_STRING) {
            printf("\"%s\"", key->str_val);
        } else {
            printf("<unknown key type>");
        }
        printf("] = ");

        // Print value
        Object* value = entry->value;
        if(!value) {
            printf("nil");
        } else {
            switch(value->kind) {
                case OBJECT_NUMBER:
                    printf("%.14g", value->num_val);
                    break;
                case OBJECT_STRING:
                    printf("\"%s\"", value->str_val);
                    break;
                case OBJECT_BOOLEAN:
                    printf("%s", value->bool_val ? "true" : "false");
                    break;
                case OBJECT_TABLE:
                    printf("<table>"); // Could recursively print but might cause cycles
                    break;
                case OBJECT_FUNCTION:
                    printf("<function>");
                    break;
                case OBJECT_BUILTIN:
                    printf("<builtin>");
                    break;
                case OBJECT_NIL:
                    printf("nil");
                    break;
                default:
                    printf("<unknown value type>");
            }
        }
        printf(",\n");
    }
    printf("}\n");
}

static Object* eval_table_expr(Eval* e, TableLiteralExpr* table_expr) {
    Object* table_obj = malloc(sizeof(Object));
    table_obj->kind = OBJECT_TABLE;
    TableVal* table = malloc(sizeof(TableVal));
    table_obj->table_val = table;

    table->pairs = init_ht();
    size_t curr_idx = 1;
    for(size_t i = 0; i < table_expr->expr_list->count; i++) {
        TableElement* elem = &table_expr->expr_list->nodes[i]->table_elem;
        Object* key;
        if(!elem->key) {
            key = malloc(sizeof(Object));
            key->kind = OBJECT_NUMBER;
            key->num_val = curr_idx++;
        } else {
            key = eval_expression(e, elem->key);
        }
        ht_insert(table->pairs, key, sizeof(Object), eval_expression(e, elem->value));
    }
    print_table(table);

    return table_obj;
}

static void eval_local_assignment(Eval* e, LocalAssignment* asgmt) {
    for(size_t i = 0; i < asgmt->var_list->count; i++) {
        Object* expr;
        if(asgmt->expr_list->count >= i) {
            expr = eval_expression(e, asgmt->expr_list->nodes[i]);
        } else {
            expr = malloc(sizeof(Object));
            expr->kind = OBJECT_NIL;
        }
        env_upsert(e->env, asgmt->var_list->symbols[i]->name,
                   strlen(asgmt->var_list->symbols[i]->name) + 1, expr);
    }
    /*show_env(e->env); */
}

static void eval_reassignment(Eval* e, ASTNodeList* lhs, ASTNodeList* rhs) {
    if(lhs->count != rhs->count) {
        FATAL("Counts on lhs and rhs didn't match");
    }

    for(size_t i = 0; i < lhs->count; i++) {
        if(lhs->nodes[i]->kind != ASTNODE_SYMBOL) {
            FATAL("Expected a symbol for reassignment | Got %s",
                  node_to_str(lhs->nodes[i]));
        }
        Object* expr = eval_expression(e, rhs->nodes[i]);
        env_upsert(e->env, lhs->nodes[i]->symbol.name,
                   strlen(lhs->nodes[i]->symbol.name) + 1, expr);
    }
    /* show_env(e->env); */
}

static void eval_local_func_assignment(Eval* e, FuncStmt* func_stmt) {
    Object* func = eval_func_expr(e, func_stmt->func_expr);
    env_upsert(e->env, func_stmt->name->name, strlen(func_stmt->name->name) + 1, func);
}

static void eval_expr_stmt(Eval* e, ExprStmt* expr_stmt) {
    if(expr_stmt->var_expr_list != NULL) {
        eval_reassignment(e, expr_stmt->var_expr_list, expr_stmt->expr_list);
    } else {
        for(size_t i = 0; i < expr_stmt->expr_list->count; i++) {
            /* Might cause problems */
            Object* _res = eval_expression(e, expr_stmt->expr_list->nodes[i]);
            free(_res);
        }
    }
}

static void eval_if_stmt(Eval* e, IfStmt* if_stmt) {
    bool evaled = false;

    for(size_t i = 0; i < if_stmt->if_cond_then_blocks->count; i++) {
        CondThenBlock block = if_stmt->if_cond_then_blocks->nodes[i]->cond_then_block;
        Object* cond = eval_expression(e, block.cond);
        if(is_truthy(cond)) {
            eval_chunk(e, block.body);
            evaled = true;
            break;
        }
    }

    if(!evaled && if_stmt->else_body) {
        eval_chunk(e, if_stmt->else_body);
    }
}

static void eval_numeric_for_stmt(Eval* e, ForStmt* for_stmt) {
    Object *start = eval_expression(e, for_stmt->start),
           *step = eval_expression(e, for_stmt->step),
           *end = eval_expression(e, for_stmt->end);

    if(for_stmt->loop_var->kind != ASTNODE_SYMBOL) {
        FATAL("Loop variable must be a symbol | Recvd %s",
              node_to_str(for_stmt->loop_var));
    }
    env_upsert(e->env, for_stmt->loop_var->symbol.name,
               strlen(for_stmt->loop_var->symbol.name) + 1, start);

    while(start->num_val < end->num_val) {
        eval_chunk(e, for_stmt->body);
        start->num_val += step->num_val;
        env_upsert(e->env, for_stmt->loop_var->symbol.name,
                   strlen(for_stmt->loop_var->symbol.name) + 1, start);
    }
}

static void eval_while_stmt(Eval* e, WhileStmt* while_stmt) {
    Object* loop_cond = eval_expression(e, while_stmt->cond);

    while(is_truthy(loop_cond)) {
        eval_chunk(e, while_stmt->body);
        loop_cond = eval_expression(e, while_stmt->cond);
    }
}

static Object* eval_expression(Eval* e, ASTNode* expr) {
    Object* res;

    switch(expr->kind) {
        case ASTNODE_FUNC_EXPR:
            res = eval_func_expr(e, expr);
            break;

        case ASTNODE_TABLE_LITERAL:
            res = eval_table_expr(e, &expr->table_literal);
            break;

        case ASTNODE_NUM_LITERAL:
        case ASTNODE_BOOL_LITERAL:
        case ASTNODE_STR_LITERAL:
        case ASTNODE_NIL_LITERAL:
            {
                res = atom_to_object(expr);
                break;
            }

        case ASTNODE_INDEX_EXPR:
            res = eval_expression(e, expr->index_expr.expr);
            break;

        case ASTNODE_BINARY_EXPR:
            {
                Object* left = eval_expression(e, expr->binary_expr.left);
                Object* right = eval_expression(e, expr->binary_expr.right);
                res = eval_binary_expr(left, right, expr->binary_expr.op);
                break;
            }

        case ASTNODE_SUFFIXED_EXPR:
            res = eval_suffixed_expr(e, &expr->suffixed_expr);
            break;

        case ASTNODE_SYMBOL:
            res = look_in_env(e->env, expr->symbol.name, strlen(expr->symbol.name) + 1);
            if(!res) {
                res = try_builtin(expr->symbol.name);
            }
            break;

        default:
            FATAL("Not yet implemented | Recvd %s", node_to_str(expr));
    }

    return res;
}

static Object* eval_builtin(Object* builtin, ObjectList* args) {
    Object* returned = malloc(sizeof(Object));
    switch(builtin->builtin_func) {
        case BUILTIN_PRINT:
            returned->kind = OBJECT_NIL;
            for(size_t i = 0; i < args->capacity; i++) {
                if(i > 0) {
                    printf(" ");
                }
                Object* arg = args->objects[i];
                switch(arg->kind) {
                    case OBJECT_NUMBER:
                        printf("%g", arg->num_val);
                        break;
                    case OBJECT_STRING:
                        printf("%s", arg->str_val);
                        break;
                    case OBJECT_BOOLEAN:
                        printf("%s", arg->bool_val ? "true" : "false");
                        break;
                    case OBJECT_NIL:
                        printf("nil");
                        break;
                    case OBJECT_FUNCTION:
                        printf("function: %p", arg->func_val);
                        break;
                    default:
                        UNIMPLEMENTED();
                }
            }
            printf("\n");
            break;
        default:
            FATAL("Not a builtin. Shouldn't ever happen");
    }

    return returned;
}

Object* eval_binary_expr(Object* left, Object* right, InfixOperator op) {
    if(!assert_binary_op_types(left, right, op)) {
        FATAL("Types do not match");
    }

    Object* result = malloc(sizeof(Object));
    switch(op) {
        case OP_ADD:
            result->kind = OBJECT_NUMBER;
            result->num_val = left->num_val + right->num_val;
            break;
        case OP_SUB:
            result->kind = OBJECT_NUMBER;
            result->num_val = left->num_val - right->num_val;
            break;
        case OP_MUL:
            result->kind = OBJECT_NUMBER;
            result->num_val = left->num_val * right->num_val;
            break;
        case OP_DIV:
            result->kind = OBJECT_NUMBER;
            result->num_val = left->num_val / right->num_val;
            break;
        case OP_MODULO:
            result->kind = OBJECT_NUMBER;
            result->num_val = fmod(left->num_val, right->num_val);
            break;
        case OP_EXPO:
            result->kind = OBJECT_NUMBER;
            result->num_val = pow(left->num_val, right->num_val);
            break;
        case OP_LT:
            result->kind = OBJECT_BOOLEAN;
            result->bool_val = left->num_val < right->num_val;
            break;
        case OP_LE:
            result->kind = OBJECT_BOOLEAN;
            result->bool_val = left->num_val <= right->num_val;
            break;
        case OP_GT:
            result->kind = OBJECT_BOOLEAN;
            result->bool_val = left->num_val > right->num_val;
            break;
        case OP_GE:
            result->kind = OBJECT_BOOLEAN;
            result->bool_val = left->num_val >= right->num_val;
            break;
        case OP_EQ:
            result->kind = OBJECT_BOOLEAN;
            result->bool_val = left->num_val == right->num_val;
            break;
        case OP_NE:
            result->kind = OBJECT_BOOLEAN;
            result->bool_val = left->num_val != right->num_val;
            break;

        case OP_AND:
            result->kind = OBJECT_NUMBER;
            result->num_val = right->num_val;
            break;
        case OP_OR:
            result->kind = OBJECT_NUMBER;
            result->num_val = left->num_val;
            break;

        case OP_CONCAT:
            UNIMPLEMENTED();
            break;

        case NO_INFIX:
            FATAL("Shouldn't have come here");
            break;
    }

    return result;
}

static void eval_stmt(Eval* e, ASTNode* node) {
    switch(node->kind) {
        case ASTNODE_LOCAL_VAR_DECL:
            eval_local_assignment(e, &node->assignment);
            break;
        case ASTNODE_FUNC_STMT:
            eval_local_func_assignment(e, &node->func_stmt);
            break;
        case ASTNODE_EXPR_STMT:
            eval_expr_stmt(e, &node->expr_stmt);
            break;
        case ASTNODE_IF_STMT:
            eval_if_stmt(e, &node->if_stmt);
            break;
        case ASTNODE_FOR_NUMERIC_STMT:
            eval_numeric_for_stmt(e, &node->for_stmt);
            break;
        case ASTNODE_WHILE_STMT:
            eval_while_stmt(e, &node->while_stmt);
            break;

        default:
            FATAL("Recvd %s", node_to_str(node));
    }
}

void eval_chunk(Eval* e, ASTNode* chunk) {
    if(chunk->kind != ASTNODE_CHUNK) {
        free(chunk);
        FATAL("Expected chunk");
    }
    for(size_t i = 0; i < chunk->chunk.stmteez->count; i++) {
        eval_stmt(e, chunk->chunk.stmteez->nodes[i]);
    }
}

/* Adding other builtin functions soon */
static Object* try_builtin(const char* name) {
    Object* builtin = malloc(sizeof(Object));
    if(strcmp("print", name) == 0) {
        builtin->kind = OBJECT_BUILTIN;
        builtin->builtin_func = BUILTIN_PRINT;
    } else {
        free(builtin);
        return NULL;
    }

    return builtin;
}

ObjectList* objects_from_nodes(Eval* e, ASTNodeList* node_list) {
    ObjectList* obj_list = malloc(sizeof(ObjectList));
    obj_list->capacity = node_list->count;
    obj_list->objects = calloc(obj_list->capacity, sizeof(Object*));

    for(size_t i = 0; i < node_list->count; i++) {
        obj_list->objects[i] = eval_expression(e, node_list->nodes[i]);
    }

    return obj_list;
}

Environment* make_env(Environment* outer) {
    Environment* e = malloc(sizeof(Environment));
    e->lookup = init_ht();
    e->outer_env = outer;

    return e;
}

Object* look_in_env(Environment* env, const void* key, size_t key_len) {
    Environment* curr_looking_in = env;

    while(curr_looking_in) {
        Object* found = ht_get(curr_looking_in->lookup, key, key_len);
        if(found) {
            return found;
        }
        curr_looking_in = curr_looking_in->outer_env;
    }

    return NULL;
}

void env_upsert(Environment* env, const void* key, size_t key_len, const Object* obj) {
    bool upserted = ht_insert(env->lookup, key, key_len, obj);
    if(!upserted) {
        FATAL("Couldn't upsert in env");
    }
}

void remove_from_env(Environment* env, const char* key, size_t key_len) {
    ht_delete(env->lookup, key, key_len);
}

static void object_to_str(Object* obj) {
    switch(obj->kind) {
        case OBJECT_FUNCTION:
            printf("function");
            break;
        case OBJECT_TABLE:
            printf("table");
            break;
        case OBJECT_NUMBER:
            printf("%g", obj->num_val);
            break;
        case OBJECT_STRING:
            printf("\"%s\"", obj->str_val);
            break;
        case OBJECT_BOOLEAN:
            printf("%s", obj->bool_val ? "true" : "false");
            break;
        case OBJECT_NIL:
            printf("nil");
            break;
        default:
            printf("<unknown type>");
    }
    printf("\n");
}

void show_env(Environment* env) {
    if(!env || !env->lookup) {
        printf("Empty or invalid environment\n");
        return;
    }

    INFO("=====ENVIRONMENT======");

    HashTable* table = env->lookup;
    for(size_t i = 0; i < table->capacity; i++) {
        HashEntry* entry = table->entries[i];
        if(entry->occupied && entry->value != NULL) {
            Object* obj = (Object*)entry->value;
            /* printf("%s = ", entry.key); */
            object_to_str(obj);
        }
    }
    INFO("======================");
}

static bool assert_binary_op_types(Object* left, Object* right, InfixOperator op) {
    switch(op) {
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_MODULO:
        case OP_EXPO:
            if(left->kind == OBJECT_NUMBER && right->kind == OBJECT_NUMBER) {
                return true;
            }
            break;

        case OP_LT:
        case OP_LE:
        case OP_GT:
        case OP_GE:
        case OP_EQ:
        case OP_NE:
            if(left->kind == right->kind && left->kind != OBJECT_NIL) {
                return true;
            }
            break;

        case OP_AND:
        case OP_OR:
            return true;
            break;

        case OP_CONCAT:
            if((left->kind == OBJECT_STRING || left->kind == OBJECT_NUMBER) &&
               (right->kind == OBJECT_STRING || right->kind == OBJECT_NUMBER)) {
                return true;
            }
            break;

        case NO_INFIX:
            return false;
            break;
    }

    return false;
}

static Object* atom_to_object(ASTNode* atom) {
    Object* obj = malloc(sizeof(Object));

    switch(atom->kind) {
        case ASTNODE_NIL_LITERAL:
            obj->kind = OBJECT_NIL;
            break;
        case ASTNODE_BOOL_LITERAL:
            obj->kind = OBJECT_BOOLEAN;
            obj->bool_val = atom->bool_literal.bool_val;
            break;
        case ASTNODE_NUM_LITERAL:
            obj->kind = OBJECT_NUMBER;
            obj->num_val = atom->num_literal.num_val;
            break;
        case ASTNODE_STR_LITERAL:
            obj->kind = OBJECT_STRING;
            obj->str_val = strdup(atom->str_literal.str_val);
            break;
        default:
            free(obj);
            obj = NULL;
    }

    return obj;
}

/* From the docs
 *  Conditionals consider false and nil as false
 * and anything else as true.
 */
static bool is_truthy(Object* obj) {
    bool res;
    if(obj->kind == OBJECT_BOOLEAN) {
        res = obj->bool_val;
    } else if(obj->kind == OBJECT_NIL) {
        res = false;
    } else {
        res = true;
    }

    return res;
};

void annihilate_env(Environment* env) {
    if(!env) {
        return;
    }

    if(env->outer_env) {
        annihilate_env(env->outer_env);
        env->outer_env = NULL;
    }

    /* fix this double free */
    /* if(env->lookup) {
        ht_free(env->lookup);
        env->lookup = NULL;
    } */
    free(env);
}

void annihilate_eval(Eval* e) {
    annihilate_env(e->env);
    free(e);
}
