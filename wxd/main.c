#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE* g_input_file;
FILE* g_output_file;
int g_num_colums = 16;
int g_group_size = 2;

void parse_cli(int argc, char** argv) {
  if (argc < 2 || strcmp(argv[1], "dump")) {
    puts("fuck you");
    exit(EXIT_FAILURE);
  }
  if (argc > 2) {
    g_input_file = fopen(argv[2], "r");
    if (!g_input_file) {
      perror("wat?");
      exit(EXIT_FAILURE);
    }
  }
  else {
    g_input_file = stdin;
  }
  if (argc > 3) {
    g_output_file = fopen(argv[3], "w");
    if (!g_output_file) {
      perror("wat?");
      exit(EXIT_FAILURE);
    }
  }
  else {
    g_output_file = stdout;
  }
  if (argc > 4) {
    puts("what are you saying?");
    exit(EXIT_FAILURE);
  }
}

void hex(unsigned long long value, int width, char* out) {
  static const char to_s[0xF + 1] = "0123456789abcdef";
  for (int i = width - 1; i >= 0; --i) {
    int d = value & 0xF;
    value = value >> 4;
    out[i] = to_s[d];
  }
}

int min(int a, int b) { return a < b ? a : b; }

void dump() {
  const int num_groups = -(g_num_colums / -g_group_size);
  const int hex_width = g_num_colums * 2 + num_groups - 1;
  const int line_width = 8 + 2 + hex_width + 2 + g_num_colums + 1;
  const int num_lines = 16 * 1024 / line_width;
  const int outbuf_size = line_width * num_lines;
  const int inbuf_size = g_num_colums * num_lines;
  char* const inbuf = (char*) malloc(inbuf_size);
  char* const outbuf = (char*) malloc(outbuf_size);
  if (!outbuf || !inbuf) {
    perror("allocation error");
    exit(EXIT_FAILURE);
  }
  size_t offset = 0;
  while (1) {
    int inbuf_len = fread(inbuf, 1, inbuf_size, g_input_file);
    int cursor;
    for (int inbase = 0; inbase < inbuf_len; inbase += g_num_colums) {
      const int line_no = inbase / g_num_colums;
      const int outbase = line_no * line_width;
      cursor = outbase;
      const int r = min(g_num_colums, inbuf_len - inbase);
      hex(offset, 8, &outbuf[cursor]);
      cursor += 8;
      outbuf[cursor++] = ':';
      for (int i = 0; i < r; ++i) {
        if (i % g_group_size == 0) {
          outbuf[cursor++] = ' ';
        }
        hex((unsigned char) inbuf[inbase + i], 2, &outbuf[cursor]);
        cursor += 2;
      }
      while (cursor < outbase + 10 + hex_width + 2) {
        outbuf[cursor++] = ' ';
      }
      for (int i = 0; i < r; ++i) {
        outbuf[cursor++] = 0x20 <= inbuf[inbase + i] && inbuf[inbase + i] <= 0x7e ? inbuf[inbase + i] : '.';
      }
      outbuf[cursor++] = '\n';
      offset += r;
    }
    if (fwrite(outbuf, 1, cursor, g_output_file) != cursor) {
      perror("output error");
      exit(EXIT_FAILURE);
    }
    if (inbuf_len < inbuf_size) {
      if (!feof(g_input_file)) {
        perror("input error");
        exit(EXIT_FAILURE);
      }
      else break;
    }
  }
  free(inbuf);
  free(outbuf);
}

int main(int argc, char** argv) {
  parse_cli(argc, argv);
  dump();
  return 0;
}
