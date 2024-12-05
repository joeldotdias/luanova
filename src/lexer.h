#ifndef LEXER_H
#define LEXER_H

#include <stdbool.h>
#include <stdio.h>

typedef enum {
    TOKEN_IDENT,
    TOKEN_NUMBER,
    TOKEN_STR,
    TOKEN_TRUE,
    TOKEN_FALSE,

    TOKEN_ASSIGN,
    TOKEN_LABEL, // ::

    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_LCURLY,
    TOKEN_RCURLY,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_OCTO,

    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASTERISK,
    TOKEN_FSLASH,
    TOKEN_MODULO,
    TOKEN_CARAT,

    TOKEN_EQ,
    TOKEN_GREATER_THAN,
    TOKEN_LESSER_THAN,
    TOKEN_GREATER_THAN_EQ,
    TOKEN_LESSER_THAN_EQ,
    TOKEN_NOT_EQ,

    TOKEN_AND,
    TOKEN_OR,
    TOKEN_NOT,

    TOKEN_CONCAT, // ..
    TOKEN_DOTS,   // ...

    TOKEN_FUNCTION,
    TOKEN_RETURN,
    TOKEN_IF,
    TOKEN_ELSEIF,
    TOKEN_ELSE,
    TOKEN_THEN,
    TOKEN_END,
    TOKEN_FOR,
    TOKEN_IN,
    TOKEN_WHILE,
    TOKEN_REPEAT,
    TOKEN_UNTIL,
    TOKEN_DO,
    TOKEN_BREAK,
    TOKEN_GOTO,
    TOKEN_LOCAL,
    TOKEN_NIL,

    TOKEN_ILLEGAL,
    TOKEN_EOF,
} TokenKind;

typedef struct {
    size_t st_line, st_col, end_line, end_col;
} Span;

typedef struct {
    TokenKind kind;
    Span span;

    // for strings, chars, block comments
    char* value;
    bool terminated;
} Token;

typedef struct {
    const char* source;
    size_t source_len;
    size_t curr_pos;
    size_t peek_pos;
} Lexer;

Lexer* init_lexer(const char* filepath);
Token* next_token(Lexer* lexer);
char* token_to_str(Token* token);
void annihilate_lexer(Lexer** lexer);
void annihilate_token(Token** token);

#endif
