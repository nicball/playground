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
  static const char to_s[0xF + 1] = "0123456789ABCDEF";
  for (int i = width - 1; i >= 0; --i) {
    int d = value & 0xF;
    value = value >> 4;
    out[i] = to_s[d];
  }
}

void dump() {
  char inbuf[g_num_colums];
  const int num_groups = -(g_num_colums / -g_group_size);
  const int hex_width = g_num_colums * 2 + num_groups - 1;
  const int line_width = 8 + 2 + hex_width + 2 + g_num_colums + 1;
  const int num_outbuf_lines = 16 * 1024 / line_width;
  const int outbuf_size = line_width * num_outbuf_lines;
  char* const outbuf = (char*) malloc(num_outbuf_lines * line_width);
  if (!outbuf) {
    fputs("allocation error\n", stderr);
    exit(EXIT_FAILURE);
  }
  int outbuf_cursor = 0;
  size_t offset = 0;
  while (1) {
    size_t r = fread(inbuf, 1, g_num_colums, g_input_file);
    size_t line_no = offset / g_num_colums;
    int base = line_no * line_width % outbuf_size;
    int cursor = base;
    if (r) {
      hex(offset, 8, &outbuf[cursor]);
      cursor += 8;
      outbuf[cursor++] = ':';
      for (int i = 0; i < r; ++i) {
        if (i % g_group_size == 0) {
          outbuf[cursor++] = ' ';
        }
        hex((unsigned char) inbuf[i], 2, &outbuf[cursor]);
        cursor += 2;
      }
      while (cursor < base + 10 + hex_width + 2) {
        outbuf[cursor++] = ' ';
      }
      for (int i = 0; i < r; ++i) {
        outbuf[cursor++] = 0x20 <= inbuf[i] && inbuf[i] <= 0x7e ? inbuf[i] : '.';
      }
      outbuf[cursor++] = '\n';
    }
    if (r < g_num_colums || cursor == outbuf_size) {
      if (fwrite(outbuf, 1, cursor, g_output_file) != cursor) {
        perror("output error");
        exit(EXIT_FAILURE);
      }
      if (r < g_num_colums) {
        if (!feof(g_input_file)) {
          perror("input error");
          exit(EXIT_FAILURE);
        }
        else break;
      }
    }
    offset += r;
  }
  free(outbuf);
}

int main(int argc, char** argv) {
  parse_cli(argc, argv);
  dump();
  return 0;
}
