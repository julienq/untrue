#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#define STRING_SIZE 256

// Primitives
enum { RET = 0x80, ADD, SUB, MUL, DIV, NEG, EQ, GT, AND, OR, NOT, STORE, FETCH,
  CALL, DUP, DROP, SWAP, ROT, PICK, IF, WHILE, PUTS, PUTI, PUTC, GETC, NOP };

// Tokenizer state types
enum { chunk, string_token, number_token, comment, quote };

// Token types
enum { primitive, literal_string, literal_number, global, lambda };

// Tokens
typedef struct token {
  size_t type;
  union {
    size_t n;                   // numerical value: primitive, integer, address
    char *string;               // for literal strings
    struct token_list *body;    // for lambda
  } value;
  struct token *next;
} token;

typedef struct token_list {
  token *first;
  token *last;
  struct token_list *parent;
} token_list;

size_t lookup_primitive(int);
token_list *make_token_list(token_list *);
token_list *append(token_list *, token *);
token *make_lambda(token_list *);
token_list *tokenize(FILE *);

size_t lookup_primitive(int c) {
  switch (c) {
    case '+': return ADD;
    case '-': return SUB;
    case '*': return MUL;
    case '/': return DIV;
    case '_': return NEG;
    case '=': return EQ;
    case '>': return GT;
    case '&': return AND;
    case '|': return OR;
    case '~': return NOT;
    case ':': return STORE;
    case ';': return FETCH;
    case '!': return CALL;
    case '$': return DUP;
    case '\\': return DROP;
    case '%': return SWAP;
    case '@': return ROT;
    case '`': return PICK;
    case '?': return IF;
    case '#': return WHILE;
    case '.': return PUTI;
    case ',': return PUTC;
    case '^': return GETC;
  }
  return 0;
}

token_list *make_token_list(token_list *parent) {
  token_list *l = (token_list *)malloc(sizeof(token_list));
  if (l) {
    l->first = NULL;
    l->last = NULL;
    l->parent = parent;
  }
  return l;
}

token_list *append(token_list *l, token *t) {
  if (l->last) {
    l->last->next = t;
  } else {
    l->first = t;
  }
  l->last = t;
  return l;
}

token *make_lambda(token_list *parent) {
  token *t = (token *)malloc(sizeof(token));
  if (t) {
    t->value.body = make_token_list(parent);
    if (t->value.body) {
      t->type = lambda;
    } else {
      t = NULL;
    }
  }
  return t;
}

token_list *tokenize(FILE *f) {
  int state = chunk;
  token_list *tokens = make_token_list(NULL);
  token *t;
  char *string = NULL;
  size_t n = 0;
  for (int c = fgetc(f); !feof(f); c = fgetc(f)) {
next_char: switch (state) {
      case chunk:
        if (c == '[') {
          t = make_lambda(tokens);
          append(tokens, t);
          tokens = t->value.body;
        } else if (c == ']') {
          tokens = tokens->parent;
        } else if (c == '{') {
          state = comment;
          n = 1;
        } else if (c == '"') {
          state = string_token;
          string = (char *)malloc(STRING_SIZE);
          n = 0;
        } else if (c == '\'') {
          state = quote;
        } else if (c >= '0' && c <= '9') {
          state = number_token;
          n = (unsigned char) c - '0';
        } else if (c >= 'a' && c <= 'z') {
          t = (token *)malloc(sizeof(token));
          t->type = literal_number;
          t->value.n = 4 * ((unsigned char) c - 'a' + 2);
          append(tokens, t);
        } else {
          n = lookup_primitive(c);
          if (n > RET && n < NOP) {
            t = (token *)malloc(sizeof(token));
            t->type = primitive;
            t->value.n = n;
            append(tokens, t);
          }
        }
        break;
      case string_token:
        if (c == '"') {
          string[n++] = '\0';
          t = (token *)malloc(sizeof(token));
          t->type = literal_string;
          t->value.string = (char *)realloc(string, n);
          append(tokens, t);
          state = chunk;
        } else {
          string[n++] = (char) c;
          if (n % STRING_SIZE == 0) {
            string = (char *)realloc(string, n + STRING_SIZE);
          }
        }
        break;
      case number_token:
        if (c >= '0' && c <= '9') {
          n = 10 * n + ((unsigned char) c - '0');
        } else {
          t = (token *)malloc(sizeof(token));
          t->type = literal_number;
          t->value.n = n;
          append(tokens, t);
          state = chunk;
          goto next_char;
        }
        break;
      case comment:
        if (c == '{') {
          ++n;
        } else if (c == '}') {
          if (--n == 0) {
            state = chunk;
          }
        }
        break;
      case quote:
        t = (token *)malloc(sizeof(token));
        t->type = literal_number;
        t->value.n = (unsigned char) c;
        append(tokens, t);
        state = chunk;
    }
  }
  return tokens;
}

int main(int argc, char *argv[]) {
  if (argc < 1) {
    return 1;
  }
  FILE *f = strcmp(argv[1], "-") == 0 ? stdin : fopen(argv[1], "r");
  if (!f) {
    return 1;
  }
  static struct termios oldt, newt;
  tcgetattr(STDIN_FILENO, &oldt);
  newt = oldt;
  if (strcmp(argv[1], "-") != 0) {
    newt.c_lflag &= (unsigned long) ~(ICANON);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
  }
  (void) tokenize(f);
  tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
  return 0;
}

