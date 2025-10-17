/*
zhrexx - MIT License - Copyright (c) 2025
Permission is granted to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of this software. Provided the copyright notice is included. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY.
*/

// TODO: maybe add continue

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>

typedef enum {
    TOK_EOF, TOK_INT, TOK_STRING, TOK_IDENT,
    TOK_FN, TOK_LET, TOK_RETURN, TOK_IF, TOK_ELSE, TOK_WHILE, TOK_FOR,
    TOK_NAMESPACE, TOK_STRUCT, TOK_IMPORT, TOK_EXTERN,
    TOK_BREAK,
    TOK_LPAREN, TOK_RPAREN, TOK_LBRACE, TOK_RBRACE,
    TOK_SEMICOLON, TOK_COLON, TOK_COMMA, TOK_DOT, TOK_ASSIGN,
    TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH,
    TOK_EQ, TOK_NE, TOK_LT, TOK_GT, TOK_LE, TOK_GE,
    TOK_ARROW, TOK_HASH, TOK_LBRACKET, TOK_RBRACKET, TOK_SCOPE_RESOLUTION
} TokenType;

typedef struct {
    TokenType type;
    char *value;
    int line;
} Token;

typedef struct Lexer {
    char *source;
    int pos;
    int line;
} Lexer;

typedef enum {
    VAL_INT, VAL_STRING, VAL_FUNCTION, VAL_STRUCT, VAL_NULL, VAL_BREAK
} ValueType;

typedef struct Value Value;
typedef struct Env Env;

typedef Value* (*NativeFn)(Value **args, int argc);

typedef struct {
    char *name;
    struct ASTNode **params;
    int param_count;
    struct ASTNode *body;
    Env *closure;
} Function;

typedef struct {
    char **field_names;
    Value **field_values;
    int field_count;
} StructInstance;

struct Value {
    ValueType type;
    union {
        int64_t int_val;
        char *str_val;
        Function fn_val;
        NativeFn native_fn;
        StructInstance struct_val;
    };
    int ref_count;
};

typedef struct EnvEntry {
    char *name;
    Value *value;
    struct EnvEntry *next;
} EnvEntry;

struct Env {
    EnvEntry *entries;
    Env *parent;
    int ref_count;
};

typedef enum {
    AST_INT, AST_STRING, AST_IDENT, AST_BINARY, AST_UNARY,
    AST_CALL, AST_FUNCTION, AST_RETURN, AST_BLOCK, AST_LET,
    AST_ASSIGN, AST_IF, AST_WHILE, AST_FOR, AST_NAMESPACE,
    AST_STRUCT_DEF, AST_STRUCT_INIT, AST_FIELD_ACCESS,
    AST_IMPORT, AST_EXTERN_FN, AST_BREAK, AST_SCOPE_RESOLUTION,
} ASTNodeType;

typedef struct ASTNode {
    ASTNodeType type;
    union {
        int64_t int_val;
        char *str_val;
        struct {
            char *name;
            struct ASTNode *namespace_body;
        } namespace_node;
        struct {
            struct ASTNode *left;
            struct ASTNode *right;
            char *op;
        } binary;
        struct {
            struct ASTNode *operand;
            char *op;
        } unary;
        struct {
            struct ASTNode *func;
            struct ASTNode **args;
            int arg_count;
        } call;
        struct {
            char *name;
            char **param_names;
            int param_count;
            struct ASTNode *body;
            char *return_type;
        } function;
        struct {
            struct ASTNode *value;
        } return_stmt;
        struct {
            struct ASTNode **stmts;
            int stmt_count;
        } block;
        struct {
            char *name;
            struct ASTNode *value;
        } let;
        struct {
            char *name;
            struct ASTNode *value;
        } assign;
        struct {
            struct ASTNode *condition;
            struct ASTNode *then_branch;
            struct ASTNode *else_branch;
        } if_stmt;
        struct {
            struct ASTNode *condition;
            struct ASTNode *body;
        } while_stmt;
        struct {
            struct ASTNode *init;
            struct ASTNode *condition;
            struct ASTNode *increment;
            struct ASTNode *body;
        } for_stmt;
        struct {
            char *name;
            char **fields;
            int field_count;
        } struct_def;
        struct {
            char *type_name;
            char **field_names;
            struct ASTNode **field_values;
            int field_count;
        } struct_init;
        struct {
            struct ASTNode *object;
            char *field;
        } field_access;
        struct {
            char *path;
        } import;
        struct {
            char *name;
            char **param_types;
            int param_count;
            char *return_type;
        } extern_fn;
        struct {
            char *namespace_name;
            char *member_name;
        } scope_resolution;
    };
} ASTNode;

typedef struct GCObject {
    Value *value;
    bool marked;
    struct GCObject *next;
} GCObject;

typedef struct {
    GCObject *objects;
    int count;
    int threshold;
} GC;

GC gc = {NULL, 0, 100};

Value* value_new_int(int64_t val);
Value* value_new_string(char *str);
Value* value_new_null(void);
Value* value_new_break(void);
void value_retain(Value *v);
void value_release(Value *v);
void gc_collect(Env *env);
void env_retain(Env *e);

Env* env_new(Env *parent) {
    Env *e = malloc(sizeof(Env));
    e->entries = NULL;
    e->parent = parent;
    e->ref_count = 1;
    if (parent) {
        env_retain(parent);
    }
    return e;
}

void env_retain(Env *e) {
    if (e) e->ref_count++;
}

void env_release(Env *e) {
    if (!e) return;
    e->ref_count--;
    if (e->ref_count == 0) {
        EnvEntry *entry = e->entries;
        while (entry) {
            EnvEntry *next = entry->next;
            free(entry->name);
            value_release(entry->value);
            free(entry);
            entry = next;
        }
        env_release(e->parent);
        free(e);
    }
}

void env_set(Env *e, char *name, Value *val) {
    EnvEntry *entry = e->entries;
    while (entry) {
        if (strcmp(entry->name, name) == 0) {
            value_release(entry->value);
            entry->value = val;
            value_retain(val);
            return;
        }
        entry = entry->next;
    }
    entry = malloc(sizeof(EnvEntry));
    entry->name = strdup(name);
    entry->value = val;
    value_retain(val);
    entry->next = e->entries;
    e->entries = entry;
}

Value* env_get(Env *e, char *name) {
    while (e) {
        EnvEntry *entry = e->entries;
        while (entry) {
            if (strcmp(entry->name, name) == 0) {
                return entry->value;
            }
            entry = entry->next;
        }
        e = e->parent;
    }
    return NULL;
}

void gc_register(Value *v) {
    GCObject *obj = malloc(sizeof(GCObject));
    obj->value = v;
    obj->marked = false;
    obj->next = gc.objects;
    gc.objects = obj;
    gc.count++;
}

Value* value_new_int(int64_t val) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_INT;
    v->int_val = val;
    v->ref_count = 1;
    gc_register(v);
    return v;
}

Value* value_new_string(char *str) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_STRING;
    v->str_val = strdup(str);
    v->ref_count = 1;
    gc_register(v);
    return v;
}

Value* value_new_break(void) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_BREAK;
    v->ref_count = 1;
    gc_register(v);
    return v;
}

Value* value_new_function(Function fn) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_FUNCTION;
    v->fn_val = fn;
    v->ref_count = 1;
    gc_register(v);
    return v;
}

Value* value_new_native(NativeFn fn) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_FUNCTION;
    v->native_fn = fn;
    v->ref_count = 1;
    gc_register(v);
    return v;
}

Value* value_new_struct(StructInstance inst) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_STRUCT;
    v->struct_val = inst;
    v->ref_count = 1;
    gc_register(v);
    return v;
}

Value* value_new_null(void) {
    Value *v = malloc(sizeof(Value));
    v->type = VAL_NULL;
    v->ref_count = 1;
    gc_register(v);
    return v;
}

void value_retain(Value *v) {
    if (v) v->ref_count++;
}

void value_release(Value *v) {
    if (!v) return;
    v->ref_count--;
    if (v->ref_count == 0) {
        if (v->type == VAL_STRING) {
            free(v->str_val);
        } else if (v->type == VAL_FUNCTION && v->fn_val.closure) {
            env_release(v->fn_val.closure);
        } else if (v->type == VAL_STRUCT) {
            for (int i = 0; i < v->struct_val.field_count; i++) {
                free(v->struct_val.field_names[i]);
                value_release(v->struct_val.field_values[i]);
            }
            free(v->struct_val.field_names);
            free(v->struct_val.field_values);
        }
    }
}

void gc_mark_value(Value *v) {
    if (!v) return;
    GCObject *obj = gc.objects;
    while (obj) {
        if (obj->value == v) {
            obj->marked = true;
            break;
        }
        obj = obj->next;
    }
}

void gc_mark_env(Env *e) {
    while (e) {
        EnvEntry *entry = e->entries;
        while (entry) {
            gc_mark_value(entry->value);
            entry = entry->next;
        }
        e = e->parent;
    }
}

void gc_collect(Env *env) {
    gc_mark_env(env);

    GCObject **obj = &gc.objects;
    while (*obj) {
        if (!(*obj)->marked) {
            GCObject *unreached = *obj;
            *obj = unreached->next;
            value_release(unreached->value);
            free(unreached->value);
            free(unreached);
            gc.count--;
        } else {
            (*obj)->marked = false;
            obj = &(*obj)->next;
        }
    }

    gc.threshold = gc.count * 2;
}

Lexer* lexer_new(char *source) {
    Lexer *l = malloc(sizeof(Lexer));
    l->source = source;
    l->pos = 0;
    l->line = 1;
    return l;
}

void skip_whitespace(Lexer *l) {
    while (l->source[l->pos] && isspace(l->source[l->pos])) {
        if (l->source[l->pos] == '\n') l->line++;
        l->pos++;
    }
}

void skip_comment(Lexer *l) {
    if (l->source[l->pos] == '/' && l->source[l->pos + 1] == '/') {
        while (l->source[l->pos] && l->source[l->pos] != '\n') {
            l->pos++;
        }
    }
}

Token* token_new(TokenType type, char *value, int line) {
    Token *t = malloc(sizeof(Token));
    t->type = type;
    t->value = value ? strdup(value) : NULL;
    t->line = line;
    return t;
}

Token* lexer_next(Lexer *l) {
    skip_whitespace(l);
    skip_comment(l);
    skip_whitespace(l);

    if (!l->source[l->pos]) return token_new(TOK_EOF, NULL, l->line);

    char c = l->source[l->pos];
    int line = l->line;

    if (isdigit(c)) {
        int start = l->pos;
        while (isdigit(l->source[l->pos])) l->pos++;
        int len = l->pos - start;
        char *num = malloc(len + 1);
        strncpy(num, l->source + start, len);
        num[len] = '\0';
        Token *t = token_new(TOK_INT, num, line);
        free(num);
        return t;
    }

    if (c == '"') {
        l->pos++;
        int start = l->pos;
        while (l->source[l->pos] && l->source[l->pos] != '"') {
            if (l->source[l->pos] == '\\') l->pos++;
            l->pos++;
        }
        int len = l->pos - start;
        char *str = malloc(len + 1);
        strncpy(str, l->source + start, len);
        str[len] = '\0';
        l->pos++;
        Token *t = token_new(TOK_STRING, str, line);
        free(str);
        return t;
    }

    if (isalpha(c) || c == '_') {
        int start = l->pos;
        while (isalnum(l->source[l->pos]) || l->source[l->pos] == '_') l->pos++;

        int len = l->pos - start;
        char *ident = malloc(len + 1);
        strncpy(ident, l->source + start, len);
        ident[len] = '\0';

        TokenType type = TOK_IDENT;
        if (strcmp(ident, "fn") == 0) type = TOK_FN;
        else if (strcmp(ident, "let") == 0) type = TOK_LET;
        else if (strcmp(ident, "return") == 0) type = TOK_RETURN;
        else if (strcmp(ident, "if") == 0) type = TOK_IF;
        else if (strcmp(ident, "else") == 0) type = TOK_ELSE;
        else if (strcmp(ident, "while") == 0) type = TOK_WHILE;
        else if (strcmp(ident, "for") == 0) type = TOK_FOR;
        else if (strcmp(ident, "namespace") == 0) type = TOK_NAMESPACE;
        else if (strcmp(ident, "struct") == 0) type = TOK_STRUCT;
        else if (strcmp(ident, "break") == 0) type = TOK_BREAK;

        Token *t = token_new(type, ident, line);
        free(ident);
        return t;
    }

    l->pos++;
    switch (c) {
        case '(': return token_new(TOK_LPAREN, NULL, line);
        case ')': return token_new(TOK_RPAREN, NULL, line);
        case '{': return token_new(TOK_LBRACE, NULL, line);
        case '}': return token_new(TOK_RBRACE, NULL, line);
        case '[': return token_new(TOK_LBRACKET, NULL, line);
        case ']': return token_new(TOK_RBRACKET, NULL, line);
        case ';': return token_new(TOK_SEMICOLON, NULL, line);
        case ',': return token_new(TOK_COMMA, NULL, line);
        case '.': return token_new(TOK_DOT, NULL, line);
        case '+': return token_new(TOK_PLUS, NULL, line);
        case '*': return token_new(TOK_STAR, NULL, line);
        case '/': return token_new(TOK_SLASH, NULL, line);
        case '#': return token_new(TOK_HASH, NULL, line);
        case '=':
            if (l->source[l->pos] == '=') {
                l->pos++;
                return token_new(TOK_EQ, NULL, line);
            }
            return token_new(TOK_ASSIGN, NULL, line);
        case '!':
            if (l->source[l->pos] == '=') {
                l->pos++;
                return token_new(TOK_NE, NULL, line);
            }
            break;
        case '<':
            if (l->source[l->pos] == '=') {
                l->pos++;
                return token_new(TOK_LE, NULL, line);
            }
            return token_new(TOK_LT, NULL, line);
        case '>':
            if (l->source[l->pos] == '=') {
                l->pos++;
                return token_new(TOK_GE, NULL, line);
            }
            return token_new(TOK_GT, NULL, line);
        case '-':
            if (l->source[l->pos] == '>') {
                l->pos++;
                return token_new(TOK_ARROW, NULL, line);
            }
            return token_new(TOK_MINUS, NULL, line);
        case ':':
            if (l->source[l->pos] == ':') {
                l->pos++;
                return token_new(TOK_SCOPE_RESOLUTION, NULL, line);
            }
            return token_new(TOK_COLON, NULL, line);
    }

    return token_new(TOK_EOF, NULL, line);
}

typedef struct Parser {
    Token **tokens;
    int pos;
    int count;
} Parser;

Parser* parser_new(Token **tokens, int count) {
    Parser *p = malloc(sizeof(Parser));
    p->tokens = tokens;
    p->pos = 0;
    p->count = count;
    return p;
}

Token* parser_current(Parser *p) {
    if (p->pos < p->count) return p->tokens[p->pos];
    return NULL;
}

Token* parser_advance(Parser *p) {
    if (p->pos < p->count) return p->tokens[p->pos++];
    return NULL;
}

bool parser_match(Parser *p, TokenType type) {
    Token *t = parser_current(p);
    return t && t->type == type;
}

Token* parser_expect(Parser *p, TokenType type) {
    Token *t = parser_current(p);
    if (!t || t->type != type) {
        fprintf(stderr, "Parse error: unexpected token\n");
        exit(1);
    }
    return parser_advance(p);
}

ASTNode* parse_expr(Parser *p);
ASTNode* parse_stmt(Parser *p);

ASTNode* parse_primary(Parser *p) {
    Token *t = parser_current(p);

    if (parser_match(p, TOK_INT)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_INT;
        node->int_val = atoll(t->value);
        return node;
    }

    if (parser_match(p, TOK_STRING)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_STRING;
        node->str_val = strdup(t->value);
        return node;
    }

    if (parser_match(p, TOK_IDENT)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_IDENT;
        node->str_val = strdup(t->value);
        return node;
    }

    if (parser_match(p, TOK_LPAREN)) {
        parser_advance(p);
        ASTNode *expr = parse_expr(p);
        parser_expect(p, TOK_RPAREN);
        return expr;
    }

    if (parser_match(p, TOK_LBRACE)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_BLOCK;
        node->block.stmt_count = 0;
        node->block.stmts = malloc(sizeof(ASTNode*) * 100);

        while (!parser_match(p, TOK_RBRACE)) {
            node->block.stmts[node->block.stmt_count++] = parse_stmt(p);
        }
        parser_expect(p, TOK_RBRACE);
        return node;
    }

    return NULL;
}

ASTNode* parse_postfix(Parser *p) {
    ASTNode *node = parse_primary(p);

    while (1) {
        if (parser_match(p, TOK_LPAREN)) {
            parser_advance(p);
            ASTNode *call = malloc(sizeof(ASTNode));
            call->type = AST_CALL;
            call->call.func = node;
            call->call.arg_count = 0;
            call->call.args = malloc(sizeof(ASTNode*) * 100);

            while (!parser_match(p, TOK_RPAREN)) {
                call->call.args[call->call.arg_count++] = parse_expr(p);
                if (parser_match(p, TOK_COMMA)) parser_advance(p);
            }
            parser_expect(p, TOK_RPAREN);
            node = call;
        } else if (parser_match(p, TOK_DOT)) {
            parser_advance(p);
            Token *field = parser_expect(p, TOK_IDENT);
            ASTNode *access = malloc(sizeof(ASTNode));
            access->type = AST_FIELD_ACCESS;
            access->field_access.object = node;
            access->field_access.field = strdup(field->value);
            node = access;
        } else if (parser_match(p, TOK_SCOPE_RESOLUTION)) {
            parser_advance(p);
            Token *member = parser_expect(p, TOK_IDENT);
            ASTNode *scope_res = malloc(sizeof(ASTNode));
            scope_res->type = AST_SCOPE_RESOLUTION;

            if (node->type == AST_IDENT) {
                scope_res->scope_resolution.namespace_name = strdup(node->str_val);
                scope_res->scope_resolution.member_name = strdup(member->value);
            } else if (node->type == AST_SCOPE_RESOLUTION) {
                char *chained = malloc(strlen(node->scope_resolution.namespace_name) +
                                     strlen("::") +
                                     strlen(node->scope_resolution.member_name) + 1);
                sprintf(chained, "%s::%s",
                       node->scope_resolution.namespace_name,
                       node->scope_resolution.member_name);
                scope_res->scope_resolution.namespace_name = chained;
                scope_res->scope_resolution.member_name = strdup(member->value);
            } else {
                fprintf(stderr, "Invalid namespace in scope resolution expected %d or %d got %d\n",
                       AST_IDENT, AST_SCOPE_RESOLUTION, node->type);
                exit(1);
            }
            free(node);
            node = scope_res;
        } else {
            break;
        }
    }

    return node;
}

ASTNode* parse_unary(Parser *p) {
    if (parser_match(p, TOK_MINUS)) {
        Token *op = parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_UNARY;
        node->unary.op = strdup(op->value ? op->value : "-");
        node->unary.operand = parse_unary(p);
        return node;
    }
    return parse_postfix(p);
}

ASTNode* parse_term(Parser *p) {
    ASTNode *left = parse_unary(p);

    while (parser_match(p, TOK_STAR) || parser_match(p, TOK_SLASH)) {
        Token *op = parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_BINARY;
        node->binary.left = left;
        node->binary.op = strdup(op->type == TOK_STAR ? "*" : "/");
        node->binary.right = parse_unary(p);
        left = node;
    }

    return left;
}

ASTNode* parse_addition(Parser *p) {
    ASTNode *left = parse_term(p);

    while (parser_match(p, TOK_PLUS) || parser_match(p, TOK_MINUS)) {
        Token *op = parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_BINARY;
        node->binary.left = left;
        node->binary.op = strdup(op->type == TOK_PLUS ? "+" : "-");
        node->binary.right = parse_term(p);
        left = node;
    }

    return left;
}

ASTNode* parse_comparison(Parser *p) {
    ASTNode *left = parse_addition(p);

    while (parser_match(p, TOK_LT) || parser_match(p, TOK_GT) ||
           parser_match(p, TOK_LE) || parser_match(p, TOK_GE) ||
           parser_match(p, TOK_EQ) || parser_match(p, TOK_NE)) {
        Token *op = parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_BINARY;
        node->binary.left = left;
        if (op->type == TOK_LT) node->binary.op = strdup("<");
        else if (op->type == TOK_GT) node->binary.op = strdup(">");
        else if (op->type == TOK_LE) node->binary.op = strdup("<=");
        else if (op->type == TOK_GE) node->binary.op = strdup(">=");
        else if (op->type == TOK_EQ) node->binary.op = strdup("==");
        else if (op->type == TOK_NE) node->binary.op = strdup("!=");
        node->binary.right = parse_addition(p);
        left = node;
    }

    return left;
}

ASTNode* parse_expr(Parser *p) {
    return parse_comparison(p);
}

ASTNode* parse_stmt(Parser *p) {
    if (parser_match(p, TOK_BREAK)) {
        parser_advance(p);
        parser_expect(p, TOK_SEMICOLON);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_BREAK;
        return node;
    }

    if (parser_match(p, TOK_RETURN)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_RETURN;
        node->return_stmt.value = parse_expr(p);
        parser_expect(p, TOK_SEMICOLON);
        return node;
    }

    if (parser_match(p, TOK_LET)) {
        parser_advance(p);
        Token *name = parser_expect(p, TOK_IDENT);
        parser_expect(p, TOK_ASSIGN);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_LET;
        node->let.name = strdup(name->value);
        node->let.value = parse_expr(p);
        parser_expect(p, TOK_SEMICOLON);
        return node;
    }

    if (parser_match(p, TOK_IF)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_IF;
        node->if_stmt.condition = parse_expr(p);
        node->if_stmt.then_branch = parse_stmt(p);
        node->if_stmt.else_branch = NULL;
        if (parser_match(p, TOK_ELSE)) {
            parser_advance(p);
            node->if_stmt.else_branch = parse_stmt(p);
        }
        return node;
    }

    if (parser_match(p, TOK_WHILE)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_WHILE;
        node->while_stmt.condition = parse_expr(p);
        node->while_stmt.body = parse_stmt(p);
        return node;
    }

    if (parser_match(p, TOK_FOR)) {
        parser_advance(p);
        parser_expect(p, TOK_LPAREN);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_FOR;
        node->for_stmt.init = parse_stmt(p);
        node->for_stmt.condition = parse_expr(p);
        parser_expect(p, TOK_SEMICOLON);
        node->for_stmt.increment = parse_expr(p);
        parser_expect(p, TOK_RPAREN);
        node->for_stmt.body = parse_stmt(p);
        return node;
    }

    if (parser_match(p, TOK_LBRACE)) {
        parser_advance(p);
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_BLOCK;
        node->block.stmt_count = 0;
        node->block.stmts = malloc(sizeof(ASTNode*) * 100);

        while (!parser_match(p, TOK_RBRACE)) {
            node->block.stmts[node->block.stmt_count++] = parse_stmt(p);
        }
        parser_expect(p, TOK_RBRACE);
        return node;
    }

    ASTNode *expr = parse_expr(p);

    if (parser_match(p, TOK_ASSIGN)) {
        parser_advance(p);
        if (expr->type != AST_IDENT) {
            fprintf(stderr, "Invalid assignment target\n");
            exit(1);
        }
        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_ASSIGN;
        node->assign.name = strdup(expr->str_val);
        node->assign.value = parse_expr(p);
        parser_expect(p, TOK_SEMICOLON);
        free(expr);
        return node;
    }

    parser_expect(p, TOK_SEMICOLON);
    return expr;
}

ASTNode* parse_toplevel(Parser *p) {
    if (parser_match(p, TOK_HASH)) {
        parser_advance(p);
        Token *t = parser_expect(p, TOK_IDENT);
        if (strcmp(t->value, "import") == 0) {
            Token *path = parser_expect(p, TOK_STRING);
            parser_expect(p, TOK_SEMICOLON);
            ASTNode *node = malloc(sizeof(ASTNode));
            node->type = AST_IMPORT;
            node->import.path = strdup(path->value);
            return node;
        }
        return NULL;
    }

    if (parser_match(p, TOK_FN)) {
        parser_advance(p);
        Token *name = parser_expect(p, TOK_IDENT);

        char *full_name = strdup(name->value);
        if (parser_match(p, TOK_SCOPE_RESOLUTION)) {
            parser_advance(p);
            Token *member = parser_expect(p, TOK_IDENT);
            char *temp = malloc(strlen(full_name) + strlen(member->value) + 3);
            sprintf(temp, "%s::%s", full_name, member->value);
            free(full_name);
            full_name = temp;
        }

        parser_expect(p, TOK_LPAREN);

        char **params = malloc(sizeof(char*) * 100);
        int param_count = 0;

        while (!parser_match(p, TOK_RPAREN)) {
            Token *param = parser_expect(p, TOK_IDENT);
            params[param_count++] = strdup(param->value);
            if (parser_match(p, TOK_COLON)) {
                parser_advance(p);
                parser_expect(p, TOK_IDENT);
            }
            if (parser_match(p, TOK_COMMA)) parser_advance(p);
        }
        parser_expect(p, TOK_RPAREN);

        char *return_type = NULL;
        if (parser_match(p, TOK_COLON)) {
            parser_advance(p);
            Token *rt = parser_expect(p, TOK_IDENT);
            return_type = strdup(rt->value);
        }

        ASTNode *body = NULL;
        if (parser_match(p, TOK_HASH)) {
            parser_advance(p);
            Token *t = parser_expect(p, TOK_IDENT);
            if (strcmp(t->value, "extern") == 0) {
                parser_expect(p, TOK_SEMICOLON);
                ASTNode *node = malloc(sizeof(ASTNode));
                node->type = AST_EXTERN_FN;
                node->extern_fn.name = full_name;
                node->extern_fn.param_types = params;
                node->extern_fn.param_count = param_count;
                node->extern_fn.return_type = return_type;
                return node;
            }
        } else {
            body = parse_stmt(p);
        }

        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_FUNCTION;
        node->function.name = full_name;
        node->function.param_names = params;
        node->function.param_count = param_count;
        node->function.body = body;
        node->function.return_type = return_type;
        return node;
    }

    if (parser_match(p, TOK_NAMESPACE)) {
        parser_advance(p);
        Token *name = parser_expect(p, TOK_IDENT);
        parser_expect(p, TOK_LBRACE);

        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_NAMESPACE;
        node->namespace_node.name = strdup(name->value);

        ASTNode *block = malloc(sizeof(ASTNode));
        block->type = AST_BLOCK;
        block->block.stmt_count = 0;
        block->block.stmts = malloc(sizeof(ASTNode*) * 100);

        while (!parser_match(p, TOK_RBRACE)) {
            ASTNode *decl = parse_toplevel(p);
            if (decl) {
                block->block.stmts[block->block.stmt_count++] = decl;
            }
        }
        parser_expect(p, TOK_RBRACE);

        node->namespace_node.namespace_body = block;
        return node;
    }

    if (parser_match(p, TOK_STRUCT)) {
        parser_advance(p);
        Token *name = parser_expect(p, TOK_IDENT);
        parser_expect(p, TOK_LBRACE);

        char **fields = malloc(sizeof(char*) * 100);
        int field_count = 0;

        while (!parser_match(p, TOK_RBRACE)) {
            Token *field = parser_expect(p, TOK_IDENT);
            fields[field_count++] = strdup(field->value);
            if (parser_match(p, TOK_COLON)) {
                parser_advance(p);
                parser_expect(p, TOK_IDENT);
            }
            if (parser_match(p, TOK_COMMA)) parser_advance(p);
            else if (parser_match(p, TOK_SEMICOLON)) parser_advance(p);
        }
        parser_expect(p, TOK_RBRACE);

        ASTNode *node = malloc(sizeof(ASTNode));
        node->type = AST_STRUCT_DEF;
        node->struct_def.name = strdup(name->value);
        node->struct_def.fields = fields;
        node->struct_def.field_count = field_count;
        return node;
    }

    return parse_stmt(p);
}

Value* eval_expr(ASTNode *node, Env *env);
Value* eval_stmt(ASTNode *node, Env *env);

Value* eval_expr(ASTNode *node, Env *env) {
    if (!node) return value_new_null();

    switch (node->type) {
        case AST_INT:
            return value_new_int(node->int_val);

        case AST_STRING:
            return value_new_string(node->str_val);

        case AST_IDENT: {
            Value *val = env_get(env, node->str_val);
            if (!val) {
                fprintf(stderr, "Undefined variable: %s\n", node->str_val);
                exit(1);
            }
            value_retain(val);
            return val;
        }

        case AST_BINARY: {
            Value *left = eval_expr(node->binary.left, env);
            Value *right = eval_expr(node->binary.right, env);
            Value *result = NULL;

            if (left->type == VAL_INT && right->type == VAL_INT) {
                if (strcmp(node->binary.op, "+") == 0) {
                    result = value_new_int(left->int_val + right->int_val);
                } else if (strcmp(node->binary.op, "-") == 0) {
                    result = value_new_int(left->int_val - right->int_val);
                } else if (strcmp(node->binary.op, "*") == 0) {
                    result = value_new_int(left->int_val * right->int_val);
                } else if (strcmp(node->binary.op, "/") == 0) {
                    result = value_new_int(left->int_val / right->int_val);
                } else if (strcmp(node->binary.op, "<") == 0) {
                    result = value_new_int(left->int_val < right->int_val);
                } else if (strcmp(node->binary.op, ">") == 0) {
                    result = value_new_int(left->int_val > right->int_val);
                } else if (strcmp(node->binary.op, "<=") == 0) {
                    result = value_new_int(left->int_val <= right->int_val);
                } else if (strcmp(node->binary.op, ">=") == 0) {
                    result = value_new_int(left->int_val >= right->int_val);
                } else if (strcmp(node->binary.op, "==") == 0) {
                    result = value_new_int(left->int_val == right->int_val);
                } else if (strcmp(node->binary.op, "!=") == 0) {
                    result = value_new_int(left->int_val != right->int_val);
                }
            } else if (left->type == VAL_STRING && right->type == VAL_STRING) {
                if (strcmp(node->binary.op, "+") == 0) {
                    char *concat = malloc(strlen(left->str_val) + strlen(right->str_val) + 1);
                    strcpy(concat, left->str_val);
                    strcat(concat, right->str_val);
                    result = value_new_string(concat);
                    free(concat);
                }
            }

            value_release(left);
            value_release(right);
            return result ? result : value_new_null();
        }

        case AST_UNARY: {
            Value *operand = eval_expr(node->unary.operand, env);
            Value *result = NULL;

            if (operand->type == VAL_INT && strcmp(node->unary.op, "-") == 0) {
                result = value_new_int(-operand->int_val);
            }

            value_release(operand);
            return result ? result : value_new_null();
        }

        case AST_CALL: {
            Value *func = eval_expr(node->call.func, env);

            if (func->type != VAL_FUNCTION) {
                fprintf(stderr, "Not a function\n");
                value_release(func);
                exit(1);
            }

            Value **args = malloc(sizeof(Value*) * node->call.arg_count);
            for (int i = 0; i < node->call.arg_count; i++) {
                args[i] = eval_expr(node->call.args[i], env);
            }

            Value *result = NULL;

            if (func->fn_val.body == NULL) {
                result = func->native_fn(args, node->call.arg_count);
            } else {
                Env *call_env = env_new(func->fn_val.closure);

                for (int i = 0; i < func->fn_val.param_count && i < node->call.arg_count; i++) {
                    if (func->fn_val.params[i]->type == AST_IDENT) {
                        env_set(call_env, func->fn_val.params[i]->str_val, args[i]);
                    }
                }

                result = eval_stmt(func->fn_val.body, call_env);
                env_release(call_env);
            }

            for (int i = 0; i < node->call.arg_count; i++) {
                value_release(args[i]);
            }
            free(args);
            value_release(func);

            return result ? result : value_new_null();
        }

        case AST_BLOCK: {
            Value *result = value_new_null();
            for (int i = 0; i < node->block.stmt_count; i++) {
                value_release(result);
                result = eval_stmt(node->block.stmts[i], env);
                if (node->block.stmts[i]->type == AST_RETURN) {
                    return result;
                }
            }
            return result;
        }

        case AST_FIELD_ACCESS: {
            Value *obj = eval_expr(node->field_access.object, env);
            if (obj->type != VAL_STRUCT) {
                fprintf(stderr, "Field access on non-struct\n");
                value_release(obj);
                exit(1);
            }

            for (int i = 0; i < obj->struct_val.field_count; i++) {
                if (strcmp(obj->struct_val.field_names[i], node->field_access.field) == 0) {
                    Value *val = obj->struct_val.field_values[i];
                    value_retain(val);
                    value_release(obj);
                    return val;
                }
            }

            value_release(obj);
            return value_new_null();
        }

        case AST_SCOPE_RESOLUTION: {
            char *qualified = malloc(strlen(node->scope_resolution.namespace_name) +
                                               strlen(node->scope_resolution.member_name) + 3);
            sprintf(qualified, "%s::%s",
                   node->scope_resolution.namespace_name,
                   node->scope_resolution.member_name);

            Value *val = env_get(env, qualified);
            if (!val) {
                fprintf(stderr, "Undefined namespaced identifier: %s\n", qualified);
                free(qualified);
                exit(1);
            }
            value_retain(val);
            free(qualified);
            return val;
        }

        default:
            return value_new_null();
    }
}

Value* eval_stmt(ASTNode *node, Env *env) {
    if (!node) return value_new_null();

    switch (node->type) {
        case AST_RETURN: {
            Value *val = eval_expr(node->return_stmt.value, env);
            return val;
        }

        case AST_LET: {
            Value *val = eval_expr(node->let.value, env);
            env_set(env, node->let.name, val);
            value_release(val);
            return value_new_null();
        }

        case AST_ASSIGN: {
            Value *val = eval_expr(node->assign.value, env);

            Env *target_env = env;
            bool found = false;

            while (target_env) {
                EnvEntry *entry = target_env->entries;
                while (entry) {
                    if (strcmp(entry->name, node->assign.name) == 0) {
                        found = true;
                        break;
                    }
                    entry = entry->next;
                }
                if (found) break;
                target_env = target_env->parent;
            }

            if (!target_env) target_env = env;

            env_set(target_env, node->assign.name, val);
            value_release(val);
            return value_new_null();
        }

        case AST_IF: {
            Value *cond = eval_expr(node->if_stmt.condition, env);
            bool is_true = (cond->type == VAL_INT && cond->int_val != 0);
            value_release(cond);

            if (is_true) {
                Value *result = eval_stmt(node->if_stmt.then_branch, env);
                if (result->type == VAL_BREAK) {
                    return result;
                }
                return result;
            } else if (node->if_stmt.else_branch) {
                Value *result = eval_stmt(node->if_stmt.else_branch, env);
                if (result->type == VAL_BREAK) {
                    return result;
                }
                return result;
            }
            return value_new_null();
        }
        case AST_BREAK:
            return value_new_break();

        case AST_WHILE: {
            Value *result = value_new_null();
            while (1) {
                Value *cond = eval_expr(node->while_stmt.condition, env);
                bool is_true = (cond->type == VAL_INT && cond->int_val != 0);
                value_release(cond);

                if (!is_true) break;

                value_release(result);
                result = eval_stmt(node->while_stmt.body, env);

                if (result->type == VAL_BREAK) {
                    value_release(result);
                    return value_new_null();
                }
            }
            return result;
        }

        case AST_FOR: {
            eval_stmt(node->for_stmt.init, env);
            Value *result = value_new_null();

            while (1) {
                Value *cond = eval_expr(node->for_stmt.condition, env);
                bool is_true = (cond->type == VAL_INT && cond->int_val != 0);
                value_release(cond);

                if (!is_true) break;

                value_release(result);
                result = eval_stmt(node->for_stmt.body, env);
                if (result->type == VAL_BREAK) {
                    value_release(result);
                    return value_new_null();
                }
                Value *inc = eval_expr(node->for_stmt.increment, env);
                value_release(inc);
            }
            return result;
        }

        case AST_BLOCK: {
            Env *block_env = env_new(env);
            Value *result = value_new_null();

            for (int i = 0; i < node->block.stmt_count; i++) {
                value_release(result);
                result = eval_stmt(node->block.stmts[i], block_env);
                if (node->block.stmts[i]->type == AST_RETURN || result->type == VAL_BREAK) {
                    env_release(block_env);
                    return result;
                }
            }

            env_release(block_env);
            return result;
        }

        case AST_FUNCTION: {
            Function fn;
            fn.name = node->function.name;
            fn.params = malloc(sizeof(ASTNode*) * node->function.param_count);
            for (int i = 0; i < node->function.param_count; i++) {
                fn.params[i] = malloc(sizeof(ASTNode));
                fn.params[i]->type = AST_IDENT;
                fn.params[i]->str_val = strdup(node->function.param_names[i]);
            }
            fn.param_count = node->function.param_count;
            fn.body = node->function.body;
            fn.closure = env;
            env_retain(env);

            Value *func_val = value_new_function(fn);
            env_set(env, node->function.name, func_val);
            value_release(func_val);
            return value_new_null();
        }

        case AST_NAMESPACE: {
            Env *ns_env = env_new(env);
            eval_stmt(node->namespace_node.namespace_body, ns_env);

            EnvEntry *entry = ns_env->entries;
            while (entry) {
                char *qualified = malloc(strlen(node->namespace_node.name) + strlen(entry->name) + 3);
                sprintf(qualified, "%s::%s", node->namespace_node.name, entry->name);
                env_set(env, qualified, entry->value);
                free(qualified);
                entry = entry->next;
            }

            env_release(ns_env);
            return value_new_null();
        }

        case AST_STRUCT_DEF: {
            return value_new_null();
        }

        default:
            return eval_expr(node, env);
    }
}

Value* builtin_print(Value **args, int argc) {
    for (int i = 0; i < argc; i++) {
        if (args[i]->type == VAL_INT) {
            printf("%lld", (long long)args[i]->int_val);
        } else if (args[i]->type == VAL_STRING) {
            printf("%s", args[i]->str_val);
        }
        if (i < argc - 1) printf(" ");
    }
    printf("\n");
    return value_new_null();
}

Value* builtin_console_flush(Value **args, int argc) {
    if (argc == 0) {
        fflush(stdout);
        return value_new_null();
    }

    if (argc == 1 && args[0]->type == VAL_INT) {
        int fd = (int)args[0]->int_val;
        FILE *f = fdopen(fd, "r+");
        if (f) {
            fflush(f);
            return value_new_null();
        } else {
            perror("fdopen");
            return value_new_null();
        }
    }

    fflush(stdout);
    return value_new_null();
}


Value* builtin_println(Value **args, int argc) {
    return builtin_print(args, argc);
}

Value* builtin_str(Value **args, int argc) {
    if (argc == 0) return value_new_string("");

    if (args[0]->type == VAL_INT) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%lld", (long long)args[0]->int_val);
        return value_new_string(buf);
    } else if (args[0]->type == VAL_STRING) {
        return value_new_string(args[0]->str_val);
    }

    return value_new_string("");
}

void register_extern_functions(Env *env) {
    Env *std_env = env_new(env);


    env_set(std_env, "print", value_new_native(builtin_print));
    env_set(std_env, "println", value_new_native(builtin_println));
    env_set(std_env, "str", value_new_native(builtin_str));
    env_set(env, "console::flush", value_new_native(builtin_console_flush));

    EnvEntry *entry = std_env->entries;
    while (entry) {
        char *qualified = malloc(strlen("std") + strlen(entry->name) + 3);
        sprintf(qualified, "std::%s", entry->name);
        env_set(env, qualified, entry->value);
        free(qualified);
        entry = entry->next;
    }

    env_release(std_env);
}

char* read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *content = malloc(size + 1);
    fread(content, 1, size, f);
    content[size] = '\0';
    fclose(f);

    return content;
}

void execute(char *source, Env *env) {
    Lexer *lexer = lexer_new(source);
    Token **tokens = malloc(sizeof(Token*) * 10000);
    int token_count = 0;

    Token *t;
    while ((t = lexer_next(lexer))->type != TOK_EOF) {
        tokens[token_count++] = t;
    }
    tokens[token_count++] = t;

    Parser *parser = parser_new(tokens, token_count);

    while (!parser_match(parser, TOK_EOF)) {
        ASTNode *node = parse_toplevel(parser);
        if (node) {
            if (node->type == AST_IMPORT) {
                char *imported = read_file(node->import.path);
                if (imported) {
                    execute(imported, env);
                    free(imported);
                }
            } else {
                Value *result = eval_stmt(node, env);
                value_release(result);
            }
        }
    }

    free(parser);
    for (int i = 0; i < token_count; i++) {
        if (tokens[i]->value) free(tokens[i]->value);
        free(tokens[i]);
    }
    free(tokens);
    free(lexer);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <script.scr>\n", argv[0]);
        return 1;
    }

    char *source = read_file(argv[1]);
    if (!source) {
        fprintf(stderr, "Could not read file: %s\n", argv[1]);
        return 1;
    }

    Env *global_env = env_new(NULL);
    register_extern_functions(global_env);

    execute(source, global_env);

    Value *main_fn = env_get(global_env, "main");
    if (main_fn && main_fn->type == VAL_FUNCTION) {
        Value *result;
        // Value *result = builtin_print((Value*[]){}, 0);
        if (main_fn->fn_val.body) {
            Env *call_env = env_new(main_fn->fn_val.closure);
            result = eval_stmt(main_fn->fn_val.body, call_env);
            env_release(call_env);
        }

        int exit_code = 0;
        if (result && result->type == VAL_INT) {
            exit_code = (int)result->int_val;
        }
        value_release(result);

        free(source);
        gc_collect(global_env);
        env_release(global_env);
        return exit_code;
    }

    free(source);
    gc_collect(global_env);
    env_release(global_env);
    return 0;
}