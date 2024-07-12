#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <fcntl.h>
#include "render.h"

int min(int a, int b) { return a < b ? a : b; }

typedef enum {
  CMD_DUMP
} command_t;

typedef struct {
  int num_columns;
  int group_size;
  FILE* input_file;
  FILE* output_file;
} dump_args_t;

typedef struct {
  command_t cmd;
  union {
    dump_args_t dump_args;
  };
} cli_args_t;

void parse_dump(const int len, const char** args, dump_args_t* result) {
  const char* positionals[2] = { 0 };
  int got_positionals = 0;
  result->group_size = 2;
  result->num_columns = 16;
  result->input_file = stdin;
  result->output_file = stdout;
  for (int i = 0; i < len; ++i) {
    if (!strcmp(args[i], "-g") || !strcmp(args[i], "--group_size")) {
      ++i;
      if (!sscanf(args[i], "%d", &result->group_size) || result->group_size <= 0) {
        fprintf(stderr, "invalid group size: %s\n", args[i]);
        exit(EXIT_FAILURE);
      }
    }
    else if (!strcmp(args[i], "-c") || !strcmp(args[i], "--num-columns")) {
      ++i;
      if (!sscanf(args[i], "%d", &result->num_columns) || result->num_columns <= 0) {
        fprintf(stderr, "invalid columns number: %s\n", args[i]);
        exit(EXIT_FAILURE);
      }
    }
    else if (got_positionals < sizeof(positionals) / sizeof(positionals[0])) {
      positionals[got_positionals++] = args[i];
    }
    else {
      fprintf(stderr, "unrecognized options: %s\n", args[i]);
      exit(EXIT_FAILURE);
    }
  }
  if (positionals[0]) {
    FILE* file = fopen(positionals[0], "r");
    if (!file) {
      perror("cannot open input");
      exit(EXIT_FAILURE);
    }
    result->input_file = file;
  }
  if (positionals[1]) {
    FILE* file = fopen(positionals[1], "w");
    if (!file) {
      perror("cannot open output");
      exit(EXIT_FAILURE);
    }
    result->output_file = file;
  }
  result->group_size = min(result->group_size, result->num_columns);
}

void parse_cli(const int len, const char** args, cli_args_t* result) {
  if (len < 1 || strcmp(args[0], "dump")) {
    puts("fuck you");
    exit(EXIT_FAILURE);
  }
  result->cmd = CMD_DUMP;
  parse_dump(len - 1, args + 1, &result->dump_args);
}

int read_exactly(FILE* const file, uint8_t* buf, const int size) {
  size_t r = fread(buf, 1, size, file);
  if (r != size && ferror(file)) {
    return -1;
  }
  else {
    return r;
  }
}

int write_exactly(FILE* const file, uint8_t* buf, const int size) {
  size_t r = fwrite(buf, 1, size, file);
  if (r != size) {
    return -1;
  }
  return r;
}

int get_hex_width(const int num_columns, const int group_size) {
  int num_groups = num_columns / group_size;
  if (num_columns % group_size) ++num_groups;
  return num_columns * 2 + num_groups - 1;
}

int get_line_width(const int num_columns, const int group_size) {
  return 10 + get_hex_width(num_columns, group_size) + 2 + num_columns + 1;
}

int render_xxd(const uint8_t* const inbuf, const int inbuf_len, const int num_columns, const int group_size, size_t offset, const int8_t* next_rel, const int8_t* o2n_rel, uint8_t* outbuf) {
  int cursor = 0;
  const int line_width = get_line_width(num_columns, group_size);
  for (int inbase = 0; inbase < inbuf_len; inbase += num_columns) {
    const int outbase = inbase / num_columns * line_width;
    const int r = min(num_columns, inbuf_len - inbase);
    render_line_no(&offset, &outbuf[outbase]);
    outbuf[outbase + 8] = ':';
    outbuf[outbase + 9] = ' ';
    cursor = outbase + 10;
    cursor += render_hex(&inbuf[inbase], r, next_rel, o2n_rel, &outbuf[cursor]);
    while (cursor < outbase + line_width - num_columns - 1) {
      outbuf[cursor++] = ' ';
    }
    render_ascii(&inbuf[inbase], r, &outbuf[cursor]);
    outbuf[cursor + r] = '\n';
    cursor += r + 1;
    offset += r;
  }
  return cursor;
}

void dump(const dump_args_t* args) {
  const int line_width = get_line_width(args->num_columns, args->group_size);
  const int num_lines = 1024;
  const int inbuf_size = args->num_columns * num_lines;
  const int outbuf_size = line_width * num_lines;
  uint8_t* const inbuf = (uint8_t*) malloc(inbuf_size);
  uint8_t* const outbuf = (uint8_t*) malloc(outbuf_size);
  const int n2o_size = args->num_columns * 2;
  const int o2n_size = get_hex_width(args->num_columns, args->group_size) + 8;
  const int next_rel_size = args->num_columns / 8;
  int* const n2o = (int*) malloc(n2o_size * sizeof(int));
  int* const o2n = (int*) malloc(o2n_size * sizeof(int));
  int8_t* const next_rel = (int8_t*) malloc(next_rel_size);
  int8_t* const o2n_rel = (int8_t*) malloc(o2n_size);;
  if (!inbuf || !outbuf || !n2o || !o2n || !next_rel || !o2n_rel) {
    perror("allocation error");
    exit(EXIT_FAILURE);
  }
  int cursor = -1;
  for (int i = 0; i < n2o_size / 2; ++i) {
    if (i % args->group_size == 0) ++cursor;
    n2o[2 * i] = cursor++;
    n2o[2 * i + 1] = cursor++;
  }
  memset(o2n, -1, o2n_size * sizeof(int));
  for (int i = 0; i < n2o_size; ++i) {
    o2n[n2o[i]] = i;
  }
  int last_out = 0;
  for (int i = 0; i < next_rel_size; ++i) {
    next_rel[i] = n2o[((i + 1) * 8 - 1) * 2 + 1] + 1 - last_out;
    last_out += next_rel[i];
  }
  int out_off = 0;
  for (int i = 0; i < next_rel_size; out_off += next_rel[i], ++i)  {
    for (int j = 0; j < next_rel[i]; ++j) {
      o2n_rel[out_off + j] = (o2n[out_off + j] == -1) ? -1 : (o2n[out_off + j] - i * 8 * 2);
    }
  }
  for (; out_off < o2n_size; ++out_off) {
    o2n_rel[out_off] = o2n[out_off] == -1 ? -1 : (o2n[out_off] - next_rel_size * 8 * 2);
  }
  printf("n2o:\t\t");
  for (int i = 0; i < n2o_size; ++i) printf(" %3d", n2o[i]);
  printf("\no2n:\t\t");
  for (int i = 0; i < o2n_size; ++i) printf(" %3d", o2n[i]);
  printf("\nnext_rel:\t");
  for (int i = 0; i < next_rel_size; ++i) printf(" %3d", next_rel[i]);
  printf("\no2n_rel:\t");
  for (int i = 0; i < o2n_size; ++i) printf(" %3d", o2n_rel[i]);
  printf("\n");
  size_t offset = 0;
  int inbuf_read;
  do {
    inbuf_read = read_exactly(args->input_file, inbuf, inbuf_size);
    if (inbuf_read == -1) {
      perror("input error");
      exit(EXIT_FAILURE);
    }
    int written = render_xxd(inbuf, inbuf_read, args->num_columns, args->group_size, offset, next_rel, o2n_rel, outbuf);
    offset += inbuf_read;
    if (write_exactly(args->output_file, outbuf, written) != written) {
      perror("output error");
      exit(EXIT_FAILURE);
    }
  } while (inbuf_read == inbuf_size);
  free(inbuf);
  free(outbuf);
}

int main(int argc, const char** argv) {
  cli_args_t cli_args;
  parse_cli(argc - 1, argv + 1, &cli_args);
  switch (cli_args.cmd) {
  case CMD_DUMP:
    dump(&cli_args.dump_args);
    break;
  }
  return 0;
}
