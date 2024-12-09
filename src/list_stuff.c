#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "shared.h"

SymbolList* init_symbol_list() {
    SymbolList* list = malloc(sizeof(SymbolList));
    if(!list) {
        FATAL("Couldn't allocate memory for symbol list");
    }

    list->symbols = malloc(10 * sizeof(Symbol*));
    if(!list->symbols) {
        free(list);
        FATAL("Couln't allocate symbols mem");
    }

    list->count = 0;
    list->capacity = 0;

    return list;
}

void add_to_symbol_list(SymbolList* list, Symbol* symbol) {
    // don't add if already exists
    for(size_t i = 0; i < list->count; i++) {
        if(strcmp(list->symbols[i]->name, symbol->name) == 0) {
            return;
        }
    }

    if(list->count >= list->capacity) {
        size_t new_capacity = list->capacity += 2;
        Symbol** new_symbols = realloc(list->symbols, new_capacity * sizeof(Symbol*));
        if(!new_symbols) {
            FATAL("Couldn't reallocate symbol list");
        }
        list->symbols = new_symbols;
        list->capacity = new_capacity;
    }

    list->symbols[list->count] = symbol;
    list->count++;
}

void print_symbol_list(const SymbolList* list) {
    for(size_t i = 0; i < list->count; i++) {
        printf("SYMBOL %zu: %s\n", i, list->symbols[i]->name);
    }
}

ASTNodeList* init_ast_node_list() {
    ASTNodeList* list = malloc(sizeof(SymbolList));
    if(!list) {
        FATAL("Couldn't allocate memory for ast node list");
    }

    list->nodes = malloc(10 * sizeof(Symbol*));
    if(!list->nodes) {
        free(list);
        FATAL("Couln't allocate nodes mem");
    }

    list->count = 0;
    list->capacity = 0;

    return list;
}

void add_to_ast_node_list(ASTNodeList* list, ASTNode* node) {
    if(list->count >= list->capacity) {
        size_t new_capacity = list->capacity += 2;
        ASTNode** new_nodes = realloc(list->nodes, new_capacity * sizeof(ASTNode*));
        if(!new_nodes) {
            FATAL("Couldn't reallocate node list");
        }
        list->nodes = new_nodes;
        list->capacity = new_capacity;
    }

    list->nodes[list->count] = node;
    list->count++;
}

void print_ast_node_list(const ASTNodeList* list) {
    for(size_t i = 0; i < list->count; i++) {
        print_ast_node(list->nodes[i], 0);
    }
}
