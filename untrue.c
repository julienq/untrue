#include <stdio.h>
#include <stdlib.h>

#define STACK_SIZE 256
#define MEMORY_INCR 256

typedef struct {
  size_t cap;
  unsigned char *ram;
  unsigned char **ip;
  unsigned char **stack;
} machine;

void dump_ram(machine *m) {
  size_t n;
  for (size_t i = 0; i < m->cap; ++i) {
    fprintf(stderr, "%08zx ", i);
    if (m->ram[i] < 0x80) {
      n = m->ram[i++] << 24 | m->ram[i++] << 16 | m->ram[i++] << 8 | m->ram[i];
      fprintf(stderr, "%08zx\n", n);
    } else if (m->ram[i] == 0x80) {
      fprintf(stderr, "Ret\n");
    } else if (m->ram[i] == 0x81) {
      fprintf(stderr, "Add\n");
    } else if (m->ram[i] == 0x82) {
      fprintf(stderr, "Sub\n");
    } else if (m->ram[i] == 0x83) {
      fprintf(stderr, "Mul\n");
    } else if (m->ram[i] == 0x84) {
      fprintf(stderr, "Div\n");
    } else if (m->ram[i] == 0x85) {
      fprintf(stderr, "Neg\n");
    } else if (m->ram[i] == 0x86) {
      fprintf(stderr, "Eq\n");
    } else if (m->ram[i] == 0x87) {
      fprintf(stderr, "Gt\n");
    } else if (m->ram[i] == 0x88) {
      fprintf(stderr, "And\n");
    } else if (m->ram[i] == 0x89) {
      fprintf(stderr, "Or\n");
    } else if (m->ram[i] == 0x8a) {
      fprintf(stderr, "Not\n");
    } else if (m->ram[i] == 0x8b) {
      fprintf(stderr, "Store\n");
    } else if (m->ram[i] == 0x8c) {
      fprintf(stderr, "Fetch\n");
    } else if (m->ram[i] == 0x8d) {
      fprintf(stderr, "Call\n");
    } else if (m->ram[i] == 0x8e) {
      fprintf(stderr, "Dup\n");
    } else if (m->ram[i] == 0x8f) {
      fprintf(stderr, "Drop\n");
    } else if (m->ram[i] == 0x90) {
      fprintf(stderr, "Swap\n");
    } else if (m->ram[i] == 0x91) {
      fprintf(stderr, "Rot\n");
    } else if (m->ram[i] == 0x92) {
      fprintf(stderr, "Pick\n");
    } else if (m->ram[i] == 0x93) {
      fprintf(stderr, "If\n");
    } else if (m->ram[i] == 0x94) {
      fprintf(stderr, "While\n");
    } else if (m->ram[i] == 0x95) {
      fprintf(stderr, "Puts\n");
      n = m->ram[i++] << 24 | m->ram[i++] << 16 | m->ram[i++] << 8 |
        m->ram[i++];
      for (size_t j = i; j < n; ++j) {
        fprintf(stderr, "%c", m->ram[i++]);
      }
      fprintf(stderr, "\n");
    } else if (m->ram[i] == 0x96) {
      fprintf(stderr, "Puti\n");
    } else if (m->ram[i] == 0x97) {
      fprintf(stderr, "Putc\n");
    } else if (m->ram[i] == 0x98) {
      fprintf(stderr, "Getc\n");
    }
  }
}

int main(int argc, char *argv[]) {
  machine m;
  size_t n = 0;
  m.cap = MEMORY_INCR;
  m.ram = (unsigned char *)malloc(m.cap);
  for (unsigned char c = fgetc(stdin); !feof(stdin); c = fgetc(stdin)) {
    if (n == m.cap) {
      m.cap += MEMORY_INCR;
      m.ram = (unsigned char *)realloc(m.ram, m.cap);
    }
    m.ram[n++] = c;
  }
  m.cap = n;
  dump_ram(&m);
  return 0;
}
