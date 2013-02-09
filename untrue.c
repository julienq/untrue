#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#define STACK_SIZE 2048
#define MEMORY_INCR 256

typedef struct {
  unsigned char *ram;
  int ip;
  size_t cap;
  int *stack;
  int sp;
} machine;

enum { Ret = 0x80, Add, Sub, Mul, Div, Neg, Eq, Gt, And, Or, Not, Store, Fetch,
  Call, Dup, Drop, Swap, Rot, Pick, If, While, Puts, Puti, Putc, Getc, Nop };

void push(machine *m, int v) {
  m->stack[--m->sp] = v;
}

int pop(machine *m) {
  return m->stack[m->sp++];
}

void call(machine *, int);

void call_lambda(machine *m, int v) {
#ifdef TRACE
  fprintf(stderr, "  call_lambda: %08x -> %08x\n", m->ip, v);
#endif
  int ip = m->ip;
  m->ip = v;
  call(m, ip);
}

int read_int(machine *m) {
  return (m->ram[m->ip++] << 24) + (m->ram[m->ip++] << 16) +
    (m->ram[m->ip++] << 8) + m->ram[m->ip];
}

void call(machine *m, int r) {
  if (m->ip >= m->cap) {
    return;
  }
  while (1) {
#ifdef TRACE
    fprintf(stderr, "%08x %08x ", m->ip, m->sp);
#endif
    unsigned char op = m->ram[m->ip];
    int v;
    if (op < Ret) {
      push(m, read_int(m));
#ifdef TRACE
      fprintf(stderr, "Push [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Ret) {
#ifdef TRACE
      fprintf(stderr, "Ret (%08x)\n", r);
#endif
      m->ip = r;
      return;
    } else if (op == Add) {
      push(m, pop(m) + pop(m));
#ifdef TRACE
      fprintf(stderr, "Add [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Sub) {
      v = pop(m);
      push(m, pop(m) - v);
#ifdef TRACE
      fprintf(stderr, "Sub [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Mul) {
      push(m, pop(m) * pop(m));
#ifdef TRACE
      fprintf(stderr, "Mul [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Div) {
      v = pop(m);
      push(m, pop(m) / v);
#ifdef TRACE
      fprintf(stderr, "Div [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Neg) {
      push(m, -pop(m));
#ifdef TRACE
      fprintf(stderr, "Neg [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Eq) {
      push(m, pop(m) == pop(m) ? 0 : ~0);
#ifdef TRACE
      fprintf(stderr, "Neg [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Gt) {
      v = pop(m);
      push(m, pop(m) > v ? 0 : ~0);
#ifdef TRACE
      fprintf(stderr, "Gt [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == And) {
      push(m, pop(m) != -1 && pop(m) != -1 ? 0 : ~0);
#ifdef TRACE
      fprintf(stderr, "And [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Or) {
      push(m, pop(m) != -1 || pop(m) != -1 ? 0 : ~0);
#ifdef TRACE
      fprintf(stderr, "Or [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Not) {
      push(m, ~pop(m));
#ifdef TRACE
      fprintf(stderr, "Not [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Store) {
      v = pop(m);
      m->ram[v] = pop(m);
#ifdef TRACE
      fprintf(stderr, "Store (%c = %d)\n", (v - 6) / 4 + 'a', m->ram[v]);
#endif
    } else if (op == Fetch) {
      v = pop(m);
      push(m, m->ram[v]);
#ifdef TRACE
      fprintf(stderr, "Fetch %c [%08x ...]\n", (v - 6) / 4 + 'a',
          m->stack[m->sp]);
#endif
    } else if (op == Call) {
#ifdef TRACE
      fprintf(stderr, "Call %08x\n", m->stack[m->sp]);
#endif
      call_lambda(m, pop(m));
    } else if (op == Dup) {
      push(m, m->stack[m->sp]);
#ifdef TRACE
      fprintf(stderr, "Dup [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == Drop) {
      (void)pop(m);
#ifdef TRACE
      if (m->sp < STACK_SIZE) {
        fprintf(stderr, "Drop [%08x ...]\n", m->stack[m->sp]);
      } else {
        fprintf(stderr, "Drop []\n");
      }
#endif
    } else if (op == Swap) {
      v = m->stack[m->sp];
      m->stack[m->sp] = m->stack[m->sp + 1];
      m->stack[m->sp + 1] = v;
#ifdef TRACE
      fprintf(stderr, "Swap [%08x %08x ...]\n", m->stack[m->sp],
          m->stack[m->sp + 1]);
#endif
    } else if (op == Rot) {
      v = m->stack[m->sp];
      m->stack[m->sp] = m->stack[m->sp + 2];
      m->stack[m->sp + 2] = m->stack[m->sp + 1];
      m->stack[m->sp + 1] = v;
#ifdef TRACE
      fprintf(stderr, "Rot [%08x %08x %08x ...]\n", m->stack[m->sp],
          m->stack[m->sp + 1], m->stack[m->sp + 2]);
#endif
    } else if (op == Pick) {
      v = pop(m);
      push(m, m->stack[m->sp + v]);
#ifdef TRACE
      fprintf(stderr, "Pick [%08x ...]\n", m->stack[m->sp]);
#endif
    } else if (op == If) {
      v = pop(m);
#ifdef TRACE
      fprintf(stderr, "If [%08x ...] -> %08x\n", m->stack[m->sp], v);
#endif
      if (pop(m) != -1) {
        call_lambda(m, v);
      }
    } else if (op == While) {
      v = pop(m);
      int w = pop(m);
      while (1) {
#ifdef TRACE
      fprintf(stderr, "While [%08x ...] -> %08x\n", m->stack[m->sp], v);
#endif
        call_lambda(m, w);
        if (pop(m) == -1) {
          break;
        }
        call_lambda(m, v);
      }
    } else if (op == Puts) {
      ++m->ip;
      int n = read_int(m);
#ifdef TRACE
      fprintf(stderr, "Puts (x%d)\n", n);
#endif
      for (int i = 0; i < n; ++i) {
        fprintf(stdout, "%c", m->ram[++m->ip]);
      }
    } else if (op == Puti) {
      fprintf(stdout, "%d", pop(m));
    } else if (op == Putc) {
      fprintf(stdout, "%c", (unsigned char)pop(m));
    } else if (op == Getc) {
      int c = getchar();
      push(m, c);
#ifdef TRACE
      fprintf(stderr, "Getc (%c)\n", m->stack[m->sp]);
#endif
    } else if (op == Nop) {
#ifdef TRACE
      fprintf(stderr, "Nop\n");
#endif
    }
    ++m->ip;
  }
}

void dump_ram(machine *m) {
  size_t n;
  for (size_t i = 0; i < m->cap; ++i) {
    fprintf(stdout, "%08zx ", i);
    if (m->ram[i] < Ret) {
      n = m->ram[i++] << 24 | m->ram[i++] << 16 | m->ram[i++] << 8 | m->ram[i];
      fprintf(stdout, "%08zx\n", n);
    } else if (m->ram[i] == Ret) {
      fprintf(stdout, "Ret\n");
    } else if (m->ram[i] == Add) {
      fprintf(stdout, "Add\n");
    } else if (m->ram[i] == Sub) {
      fprintf(stdout, "Sub\n");
    } else if (m->ram[i] == Mul) {
      fprintf(stdout, "Mul\n");
    } else if (m->ram[i] == Div) {
      fprintf(stdout, "Div\n");
    } else if (m->ram[i] == Neg) {
      fprintf(stdout, "Neg\n");
    } else if (m->ram[i] == Eq) {
      fprintf(stdout, "Eq\n");
    } else if (m->ram[i] == Gt) {
      fprintf(stdout, "Gt\n");
    } else if (m->ram[i] == And) {
      fprintf(stdout, "And\n");
    } else if (m->ram[i] == Or) {
      fprintf(stdout, "Or\n");
    } else if (m->ram[i] == Not) {
      fprintf(stdout, "Not\n");
    } else if (m->ram[i] == Store) {
      fprintf(stdout, "Store\n");
    } else if (m->ram[i] == Fetch) {
      fprintf(stdout, "Fetch\n");
    } else if (m->ram[i] == Call) {
      fprintf(stdout, "Call\n");
    } else if (m->ram[i] == Dup) {
      fprintf(stdout, "Dup\n");
    } else if (m->ram[i] == Drop) {
      fprintf(stdout, "Drop\n");
    } else if (m->ram[i] == Swap) {
      fprintf(stdout, "Swap\n");
    } else if (m->ram[i] == Rot) {
      fprintf(stdout, "Rot\n");
    } else if (m->ram[i] == Pick) {
      fprintf(stdout, "Pick\n");
    } else if (m->ram[i] == If) {
      fprintf(stdout, "If\n");
    } else if (m->ram[i] == While) {
      fprintf(stdout, "While\n");
    } else if (m->ram[i] == Puts) {
      fprintf(stdout, "Puts\n");
      fprintf(stdout, "%08zx ", i + 1);
      n = m->ram[++i] << 24 | m->ram[++i] << 16 | m->ram[++i] << 8 |
        m->ram[++i];
      fprintf(stdout, "%08zx\n%08zx \"", n, ++i);
      for (size_t j = 0; j < n; ++j) {
        fprintf(stdout, "%c", m->ram[i + j]);
      }
      fprintf(stdout, "\"\n");
      i += n - 1;
    } else if (m->ram[i] == Puti) {
      fprintf(stdout, "Puti\n");
    } else if (m->ram[i] == Putc) {
      fprintf(stdout, "Putc\n");
    } else if (m->ram[i] == Getc) {
      fprintf(stdout, "Getc\n");
    } else if (m->ram[i] == Nop) {
      fprintf(stdout, "Nop\n");
    }
  }
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
    newt.c_lflag &= ~(ICANON);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
  }
  machine m;
  size_t n = 0;
  m.cap = MEMORY_INCR;
  m.ip = 0;
  m.sp = STACK_SIZE;
  m.ram = (unsigned char *)malloc(m.cap);
  m.stack = (int *)malloc(sizeof(int) * STACK_SIZE);
  for (unsigned char c = fgetc(f); !feof(f); c = fgetc(f)) {
    if (n == m.cap) {
      m.cap += MEMORY_INCR;
      m.ram = (unsigned char *)realloc(m.ram, m.cap);
    }
    m.ram[n++] = c;
  }
  m.cap = n;
  if (argc > 2 && strcmp(argv[2], "dump") == 0) {
    dump_ram(&m);
  } else {
    call(&m, m.cap);
  }
  tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
  return 0;
}
