#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "shared.h"

static Token* read_ident(Lexer* lexer);
static Token* read_str_literal(Lexer* lexer, char delim);
static Token* read_num_literal(Lexer* lexer);
static Token* make_value_token(TokenKind kind, char* value);
static Token* make_sym_token(TokenKind kind);
static char peek(Lexer* lexer);
static char peek_peek(Lexer* lexer);
static void eat_whitespace(Lexer* lexer);
static void advance_lexer(Lexer* lexer);
static char* read_file(const char* path);

Lexer* init_lexer(const char* filepath) {
    size_t len = sizeof(Lexer);
    Lexer* lexer = malloc(len);
    memset(lexer, 0, len);

    lexer->source = read_file(filepath);
    lexer->source_len = strlen(lexer->source);
    lexer->curr_pos = 0;
    lexer->peek_pos = 1;

    return lexer;
}

Token* next_token(Lexer* lexer) {
    Token* token = NULL;
    char* value = NULL;

    eat_whitespace(lexer);

    char curr_char = lexer->source[lexer->curr_pos];

    switch(curr_char) {
        case '{':
            token = make_sym_token(TOKEN_LCURLY);
            break;
        case '}':
            token = make_sym_token(TOKEN_RCURLY);
            break;
        case '(':
            token = make_sym_token(TOKEN_LPAREN);
            break;
        case ')':
            token = make_sym_token(TOKEN_RPAREN);
            break;
        case '[':
            token = make_sym_token(TOKEN_LBRACKET);
            break;
        case ']':
            token = make_sym_token(TOKEN_RBRACKET);
            break;
        case '.':
            if(peek(lexer) == '.') {
                token = make_sym_token(TOKEN_CONCAT);
            } else if(peek_peek(lexer)) {
                token = make_sym_token(TOKEN_DOTS);
            } else {
                token = make_sym_token(TOKEN_DOT);
            }
            break;
        case ',':
            token = make_sym_token(TOKEN_COMMA);
            break;
        case ':':
            if(peek(lexer) == ':') {
                token = make_sym_token(TOKEN_LABEL);
            } else {
                token = make_sym_token(TOKEN_COLON);
            }
            break;
        case ';':
            token = make_sym_token(TOKEN_SEMICOLON);
            break;
        case '#':
            token = make_sym_token(TOKEN_OCTO);
            break;

        case '+':
            token = make_sym_token(TOKEN_PLUS);
            break;
        case '-':
            token = make_sym_token(TOKEN_MINUS);
            break;
        case '*':
            token = make_sym_token(TOKEN_ASTERISK);
            break;
        case '/':
            token = make_sym_token(TOKEN_FSLASH);
            break;
        case '%':
            token = make_sym_token(TOKEN_MODULO);
            break;
        case '^':
            token = make_sym_token(TOKEN_CARAT);
            break;

        case '=':
            if(peek(lexer) == '=') {
                token = make_sym_token(TOKEN_EQ);
            } else {
                token = make_sym_token(TOKEN_ASSIGN);
            }
            break;
        case '~':
            if(peek(lexer) == '=') {
                token = make_sym_token(TOKEN_NOT_EQ);
            } else {
                token = make_value_token(TOKEN_ILLEGAL, "~");
            }
            break;
        case '<':
            if(peek(lexer) == '=') {
                advance_lexer(lexer);
                token = make_sym_token(TOKEN_LESSER_THAN_EQ);
            } else {
                token = make_sym_token(TOKEN_LESSER_THAN);
            }
            break;
        case '>':
            if(peek(lexer) == '=') {
                advance_lexer(lexer);
                token = make_sym_token(TOKEN_GREATER_THAN_EQ);
            } else {
                token = make_sym_token(TOKEN_GREATER_THAN);
            }
            break;

        case '\'':
        case '"':
            // eat opening delimeter
            advance_lexer(lexer);
            token = read_str_literal(lexer, curr_char);
            break;

        case '\0':
            token = make_sym_token(TOKEN_EOF);
            break;

        default:
            if(isalpha(curr_char)) {
                token = read_ident(lexer);
                return token;
            } else if(isdigit(curr_char)) {
                token = read_num_literal(lexer);
                return token;
            } else {
                value = malloc(2);
                value[0] = curr_char;
                value[1] = '\0';
                token = make_value_token(TOKEN_ILLEGAL, value);
            }
    }

    advance_lexer(lexer);

    if(value != NULL) {
        free(value);
    }

    return token;
}

static Token* read_str_literal(Lexer* lexer, char delim) {
    Token* token = NULL;
    size_t buffer_size = 256;
    char* buffer = malloc(buffer_size);
    size_t len = 0;
    bool terminated = false;

    char curr_char;
    while((curr_char = lexer->source[lexer->curr_pos]) && curr_char != delim) {
        if(len >= buffer_size - 1) {
            // grow the buffer by 1.5x
            // seems better than just doubling it everytime
            // still a stupid method. need to come up with something better
            buffer_size += buffer_size / 2;
            buffer = realloc(buffer, buffer_size);
        }
        if(curr_char == '\\') {
            switch(peek(lexer)) {
                case 'n':
                    advance_lexer(lexer);
                    buffer[len++] = '\n';
                default:
                    advance_lexer(lexer);
                    curr_char = lexer->source[lexer->curr_pos];
            }
        }
        buffer[len++] = curr_char;
        advance_lexer(lexer);

        if(curr_char == EOF) {
            break;
        }
    }

    if(curr_char == '"') {
        terminated = true;
    }
    buffer[len] = '\0';
    token = make_value_token(TOKEN_STR, buffer);
    token->terminated = terminated;
    free(buffer);

    return token;
}

static Token* read_ident(Lexer* lexer) {
    Token* token = NULL;
    char* buffer = malloc(256);
    size_t len = 0;

    char curr_char;
    while((curr_char = lexer->source[lexer->curr_pos]) &&
          (isalpha(curr_char) || isdigit(curr_char) || curr_char == '_')) {
        buffer[len++] = curr_char;
        advance_lexer(lexer);
    }
    buffer[len] = '\0';

    if(strcmp(buffer, "and") == 0) {
        token = make_sym_token(TOKEN_AND);
    } else if(strcmp(buffer, "break") == 0) {
        token = make_sym_token(TOKEN_BREAK);
    } else if(strcmp(buffer, "do") == 0) {
        token = make_sym_token(TOKEN_DO);
    } else if(strcmp(buffer, "elseif") == 0) {
        token = make_sym_token(TOKEN_ELSEIF);
    } else if(strcmp(buffer, "else") == 0) {
        token = make_sym_token(TOKEN_ELSE);
    } else if(strcmp(buffer, "end") == 0) {
        token = make_sym_token(TOKEN_END);
    } else if(strcmp(buffer, "false") == 0) {
        token = make_sym_token(TOKEN_FALSE);
    } else if(strcmp(buffer, "for") == 0) {
        token = make_sym_token(TOKEN_FOR);
    } else if(strcmp(buffer, "function") == 0) {
        token = make_sym_token(TOKEN_FUNCTION);
    } else if(strcmp(buffer, "goto") == 0) {
        token = make_sym_token(TOKEN_GOTO);
    } else if(strcmp(buffer, "if") == 0) {
        token = make_sym_token(TOKEN_IF);
    } else if(strcmp(buffer, "in") == 0) {
        token = make_sym_token(TOKEN_IN);
    } else if(strcmp(buffer, "local") == 0) {
        token = make_sym_token(TOKEN_LOCAL);
    } else if(strcmp(buffer, "nil") == 0) {
        token = make_sym_token(TOKEN_NIL);
    } else if(strcmp(buffer, "not") == 0) {
        token = make_sym_token(TOKEN_NOT);
    } else if(strcmp(buffer, "repeat") == 0) {
        token = make_sym_token(TOKEN_REPEAT);
    } else if(strcmp(buffer, "return") == 0) {
        token = make_sym_token(TOKEN_RETURN);
    } else if(strcmp(buffer, "then") == 0) {
        token = make_sym_token(TOKEN_THEN);
    } else if(strcmp(buffer, "true") == 0) {
        token = make_sym_token(TOKEN_TRUE);
    } else if(strcmp(buffer, "while") == 0) {
        token = make_sym_token(TOKEN_WHILE);
    } else if(strcmp(buffer, "and") == 0) {
        token = make_sym_token(TOKEN_AND);
    } else if(strcmp(buffer, "or") == 0) {
        token = make_sym_token(TOKEN_OR);
    } else if(strcmp(buffer, "not") == 0) {
        token = make_sym_token(TOKEN_NOT);
    } else {
        // identifiers
        token = make_value_token(TOKEN_IDENT, buffer);
    }

    free(buffer);

    return token;
}

static Token* read_num_literal(Lexer* lexer) {
    Token* token = NULL;
    TokenKind kind;
    char* buffer = malloc(256);
    size_t len = 0;
    bool dot = false;

    char curr_char;
    while((curr_char = lexer->source[lexer->curr_pos]) &&
          (isdigit(curr_char) || curr_char == '.')) {
        // if a dot is already present in the number we break if another is
        // encountered
        if(curr_char == '.') {
            if(dot) {
                break;
            } else {
                dot = true;
            }
        }

        buffer[len++] = curr_char;
        advance_lexer(lexer);
    }
    buffer[len] = '\0';

    kind = TOKEN_NUMBER;

    token = make_value_token(kind, buffer);
    free(buffer);
    return token;
}

void annihilate_lexer(Lexer** lexer) {
    free((void*)(*lexer)->source);
    free(*lexer);
    *lexer = NULL;
}

void annihilate_token(Token** token) {
    if((*token)->value) {
        free((*token)->value);
        (*token)->value = NULL;
    }

    free(*token);
    *token = NULL;
}

static Token* make_value_token(TokenKind kind, char* value) {
    Token* token = make_sym_token(kind);

    // in case of identifiers, literals, doc comments(still a TODO), illegal tokens
    // not adding any additional TokenKind checks here
    if(value != NULL) {
        token->value = strdup(value);
    }

    return token;
}

static Token* make_sym_token(TokenKind kind) {
    size_t len = sizeof(Token);
    Token* token = malloc(len);
    memset(token, 0, len);

    token->kind = kind;
    token->value = NULL;

    return token;
}

static char peek(Lexer* lexer) {
    if(lexer->peek_pos >= lexer->source_len) {
        return EOF;
    } else {
        return lexer->source[lexer->peek_pos];
    }
}

static char peek_peek(Lexer* lexer) {
    if(lexer->peek_pos + 1 >= lexer->source_len) {
        return EOF;
    } else {
        return lexer->source[lexer->peek_pos + 1];
    }
}

static void advance_lexer(Lexer* lexer) {
    lexer->curr_pos = lexer->peek_pos;
    if(lexer->peek_pos < lexer->source_len) {
        ++lexer->peek_pos;
    }
}

static void eat_whitespace(Lexer* lexer) {
    while(isspace(lexer->source[lexer->curr_pos])) {
        advance_lexer(lexer);
    }
}

char* token_to_str(Token* token) {
    char buffer[512] = {0};

    switch(token->kind) {
        case TOKEN_IDENT:
            strcat(buffer, "IDENT: ");
            break;
        case TOKEN_NUMBER:
            strcat(buffer, "NUMBER: ");
            break;
        case TOKEN_STR:
            strcat(buffer, "STR: ");
            break;
        case TOKEN_TRUE:
            strcat(buffer, "TRUE");
            break;
        case TOKEN_FALSE:
            strcat(buffer, "FALSE");
            break;
        case TOKEN_ASSIGN:
            strcat(buffer, "ASSIGN");
            break;
        case TOKEN_LABEL:
            strcat(buffer, "LABEL");
            break;
        case TOKEN_LPAREN:
            strcat(buffer, "LPAREN");
            break;
        case TOKEN_RPAREN:
            strcat(buffer, "RPAREN");
            break;
        case TOKEN_LBRACKET:
            strcat(buffer, "LBRACKET");
            break;
        case TOKEN_RBRACKET:
            strcat(buffer, "RBRACKET");
            break;
        case TOKEN_LCURLY:
            strcat(buffer, "LCURLY");
            break;
        case TOKEN_RCURLY:
            strcat(buffer, "RCURLY");
            break;
        case TOKEN_DOT:
            strcat(buffer, "DOT");
            break;
        case TOKEN_COMMA:
            strcat(buffer, "COMMA");
            break;
        case TOKEN_SEMICOLON:
            strcat(buffer, "SEMICOLON");
            break;
        case TOKEN_COLON:
            strcat(buffer, "COLON");
            break;
        case TOKEN_OCTO:
            strcat(buffer, "OCTO");
            break;
        case TOKEN_PLUS:
            strcat(buffer, "PLUS");
            break;
        case TOKEN_MINUS:
            strcat(buffer, "MINUS");
            break;
        case TOKEN_ASTERISK:
            strcat(buffer, "ASTERISK");
            break;
        case TOKEN_FSLASH:
            strcat(buffer, "FSLASH");
            break;
        case TOKEN_MODULO:
            strcat(buffer, "MODULO");
            break;
        case TOKEN_CARAT:
            strcat(buffer, "CARAT");
            break;
        case TOKEN_EQ:
            strcat(buffer, "EQ");
            break;
        case TOKEN_GREATER_THAN:
            strcat(buffer, "GREATER_THAN");
            break;
        case TOKEN_LESSER_THAN:
            strcat(buffer, "LESSER_THAN");
            break;
        case TOKEN_GREATER_THAN_EQ:
            strcat(buffer, "GREATER_THAN_EQ");
            break;
        case TOKEN_LESSER_THAN_EQ:
            strcat(buffer, "LESSER_THAN_EQ");
            break;
        case TOKEN_NOT_EQ:
            strcat(buffer, "NOT_EQ");
            break;
        case TOKEN_AND:
            strcat(buffer, "AND");
            break;
        case TOKEN_OR:
            strcat(buffer, "OR");
            break;
        case TOKEN_NOT:
            strcat(buffer, "NOT");
            break;
        case TOKEN_CONCAT:
            strcat(buffer, "CONCAT");
            break;
        case TOKEN_DOTS:
            strcat(buffer, "DOTS");
            break;
        case TOKEN_FUNCTION:
            strcat(buffer, "FUNCTION");
            break;
        case TOKEN_RETURN:
            strcat(buffer, "RETURN");
            break;
        case TOKEN_IF:
            strcat(buffer, "IF");
            break;
        case TOKEN_ELSEIF:
            strcat(buffer, "ELSEIF");
            break;
        case TOKEN_ELSE:
            strcat(buffer, "ELSE");
            break;
        case TOKEN_THEN:
            strcat(buffer, "THEN");
            break;
        case TOKEN_END:
            strcat(buffer, "END");
            break;
        case TOKEN_FOR:
            strcat(buffer, "FOR");
            break;
        case TOKEN_IN:
            strcat(buffer, "IN");
            break;
        case TOKEN_WHILE:
            strcat(buffer, "WHILE");
            break;
        case TOKEN_REPEAT:
            strcat(buffer, "REPEAT");
            break;
        case TOKEN_UNTIL:
            strcat(buffer, "UNTIL");
            break;
        case TOKEN_DO:
            strcat(buffer, "DO");
            break;
        case TOKEN_BREAK:
            strcat(buffer, "BREAK");
            break;
        case TOKEN_GOTO:
            strcat(buffer, "GOTO");
            break;
        case TOKEN_LOCAL:
            strcat(buffer, "LOCAL");
            break;
        case TOKEN_NIL:
            strcat(buffer, "NIL");
            break;
        case TOKEN_ILLEGAL:
            strcat(buffer, "ILLEGAL: ");
            break;
        case TOKEN_EOF:
            strcat(buffer, "EOF");
            break;
        default:
            strcat(buffer, "UNKNOWN");
            break;
    }

    if(token->value) {
        strcat(buffer, " ");
        strcat(buffer, token->value);
    }

    char* result = malloc(strlen(buffer) + 1);
    if(result) {
        strcpy(result, buffer);
    }

    return result;
}

static char* read_file(const char* path) {
    FILE* file = fopen(path, "r");
    if(!file) {
        LOG_FATAL("Couldn't open %s", path);
    }

    fseek(file, 0, SEEK_END);
    size_t len = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(len + 1);
    size_t bytes_read = fread(buffer, sizeof(char), len, file);
    buffer[bytes_read] = '\0';

    fclose(file);
    return buffer;
}
