/* The entire purpose of this is just to have a function to quickly
 * run programs so I can make sure everything works as i take further
 * steps to simplify and lower the ast etc.
 */

#include "eval.h"
#include "ast.h"
#include "hashtab.h"
#include "shared.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void atom_to_object(ASTNode* node, Object* obj);

Eval* init_eval() {
    Eval* e = malloc(sizeof(Eval));
    e->env = make_env(NULL);

    return e;
}

bool assert_binary_op_types(Object* left, Object* right, InfixOperator op) {
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

Object* eval_expression(Eval* e, ASTNode* expr);

/* Adding other builtin functions soon */
Object* try_builtin(const char* name) {
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
Object* apply_builtin(Object* builtin, Object** args) {
    Object* returned = malloc(sizeof(Object));
    switch(builtin->builtin_func) {
        case BUILTIN_PRINT:
            returned->kind = OBJECT_NIL;
            switch(args[0]->kind) {
                case OBJECT_NUMBER:
                    printf("%g", args[0]->num_val);
                    break;
                case OBJECT_STRING:
                    printf("%s", args[0]->str_val);
                    break;
                case OBJECT_BOOLEAN:
                    printf("%s", args[0]->bool_val ? "true" : "false");
                    break;
                case OBJECT_NIL:
                    printf("nil");
                    break;
                default:
                    UNIMPLEMENTED();
            }
            printf("\n");
            break;
        default:
            FATAL("Not a builtin. Shouldn't ever happen");
    }

    return returned;
}

Object* eval_suffixed_expr(Eval* e, SuffixedExpr* suffixed_expr) {
    ASTNode* primary = suffixed_expr->primary_expr;
    if(suffixed_expr->suffix_list->count == 1 &&
       suffixed_expr->suffix_list->nodes[0]->kind == ASTNODE_FUNC_CALL_EXPR) {
        Object* func = look_in_env(e->env, primary->symbol.name);
        FuncCall func_call = suffixed_expr->suffix_list->nodes[0]->func_call;
        if(!func) {
            func = try_builtin(primary->symbol.name);
            if(!func) {
                FATAL("not a function call %s", primary->symbol.name);
            }
            Object** args = calloc(func_call.args->count, sizeof(Object));
            for(size_t i = 0; i < func_call.args->count; i++) {
                args[i] = eval_expression(e, func_call.args->nodes[i]);
            }
            Object* returned = apply_builtin(func, args);
            return returned;
        }
    }

    return NULL;
}

Object* eval_expression(Eval* e, ASTNode* expr) {
    Object* obj = malloc(sizeof(Object));

    switch(expr->kind) {
        case ASTNODE_NUM_LITERAL:
        case ASTNODE_BOOL_LITERAL:
        case ASTNODE_STR_LITERAL:
        case ASTNODE_NIL_LITERAL:
            {
                atom_to_object(expr, obj);
                break;
            }

        case ASTNODE_BINARY_EXPR:
            {
                Object* left = eval_expression(e, expr->binary_expr.left);
                Object* right = eval_expression(e, expr->binary_expr.right);
                obj = eval_binary_expr(left, right, expr->binary_expr.op);
                break;
            }

        case ASTNODE_SUFFIXED_EXPR:
            eval_suffixed_expr(e, &expr->suffixed_expr);
            break;

        case ASTNODE_SYMBOL:
            obj = look_in_env(e->env, expr->symbol.name);
            break;

        default:
            free(obj);
            FATAL("Not yet implemented | Recvd %s", node_to_str(expr));
    }

    return obj;
}

void eval_local_assignment(Eval* e, LocalAssignment* asgmt) {
    for(size_t i = 0; i < asgmt->var_list->count; i++) {
        Object* expr = malloc(sizeof(Object));
        if(asgmt->expr_list->count >= i) {
            expr = eval_expression(e, asgmt->expr_list->nodes[i]);
        } else {
            expr->kind = OBJECT_NIL;
        }
        env_upsert(e->env, asgmt->var_list->symbols[i]->name, expr);
    }
    show_env(e->env);
}

void eval_reassignment(Eval* e, ASTNodeList* lhs, ASTNodeList* rhs) {
    if(lhs->count != rhs->count) {
        FATAL("Counts on lhs and rhs didn't match");
    }

    for(size_t i = 0; i < lhs->count; i++) {
        if(lhs->nodes[i]->kind != ASTNODE_SYMBOL) {
            FATAL("Expected a symbol for reassignment | Got %s",
                  node_to_str(lhs->nodes[i]));
        }
        Object* expr = eval_expression(e, rhs->nodes[i]);
        env_upsert(e->env, lhs->nodes[i]->symbol.name, expr);
    }
    show_env(e->env);
}

void eval_expr_stmt(Eval* e, ExprStmt* expr_stmt) {
    if(expr_stmt->var_expr_list != NULL) {
        eval_reassignment(e, expr_stmt->var_expr_list, expr_stmt->expr_list);
    } else {
        for(size_t i = 0; i < expr_stmt->expr_list->count; i++) {
            eval_expression(e, expr_stmt->expr_list->nodes[i]);
        }
    }
}

void eval_node(Eval* e, ASTNode* node) {
    switch(node->kind) {
        case ASTNODE_LOCAL_VAR_DECL:
            eval_local_assignment(e, &node->assignment);
            break;
        case ASTNODE_EXPR_STMT:
            eval_expr_stmt(e, &node->expr_stmt);
            break;

        default:
            FATAL("Recvd %s", node_to_str(node));
    }
}

void eval(Eval* e, ASTNode* root) {
    if(root->kind != ASTNODE_CHUNK) {
        FATAL("Expected chunk");
    }
    Chunk chunk = root->chunk;
    for(size_t i = 0; i < chunk.stmteez->count; i++) {
        eval_node(e, chunk.stmteez->nodes[i]);
    }
}

Environment* make_env(Environment* outer) {
    Environment* e = malloc(sizeof(Environment));
    e->lookup = init_ht();
    e->outer_env = outer;

    return e;
}

Object* look_in_env(Environment* env, const char* key) {
    return ht_get(env->lookup, key);
}

bool env_upsert(Environment* env, const char* key, const Object* obj) {
    return ht_insert(env->lookup, key, obj);
}

void remove_from_env(Environment* env, const char* key) { ht_delete(env->lookup, key); }

void show_env(Environment* env) {
    if(!env || !env->lookup) {
        printf("Empty or invalid environment\n");
        return;
    }

    INFO("=====ENVIRONMENT======");

    HashTable* table = env->lookup;
    for(size_t i = 0; i < table->capacity; i++) {
        HashEntry entry = table->entries[i];
        if(entry.occupied && entry.value != NULL) {
            Object* obj = (Object*)entry.value;
            printf("%s = ", entry.key);

            switch(obj->kind) {
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
    }
    INFO("======================");
}

static void atom_to_object(ASTNode* node, Object* obj) {
    switch(node->kind) {
        case ASTNODE_NIL_LITERAL:
            obj->kind = OBJECT_NIL;
            break;
        case ASTNODE_BOOL_LITERAL:
            obj->kind = OBJECT_BOOLEAN;
            obj->bool_val = node->bool_literal.bool_val;
            break;
        case ASTNODE_NUM_LITERAL:
            obj->kind = OBJECT_NUMBER;
            obj->num_val = node->num_literal.num_val;
            break;
        case ASTNODE_STR_LITERAL:
            obj->kind = OBJECT_STRING;
            obj->str_val = strdup(node->str_literal.str_val);
            break;
        default:
            free(obj);
            obj = NULL;
    }
}
