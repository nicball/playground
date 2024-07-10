#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
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
  int input_fd;
  int output_fd;
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
  result->input_fd = 0;
  result->output_fd = 1;
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
    int fd = open(positionals[0], O_RDONLY);
    if (fd == -1) {
      perror("cannot open input");
      exit(EXIT_FAILURE);
    }
    result->input_fd = fd;
  }
  if (positionals[1]) {
    int fd = open(positionals[1], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
      perror("cannot open output");
      exit(EXIT_FAILURE);
    }
    result->output_fd = fd;
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

void hex(unsigned long long value, const int width, uint8_t* const out) {
  static const uint8_t to_s[] = "0123456789abcdef";
  for (int i = width - 1; i >= 0; --i) {
    int d = value & 0xF;
    value = value >> 4;
    out[i] = to_s[d];
  }
}

int read_exactly(const int fd, uint8_t* buf, const int size) {
  int count = 0;
  while (count != size) {
    ssize_t r = read(fd, buf, size - count);
    if (r == 0) return count;
    else if (r == -1) return -1;
    count += r;
    buf += r;
  }
  return count;
}

int write_exactly(const int fd, uint8_t* buf, const int size) {
  int count = 0;
  while (count != size) {
    ssize_t r = write(fd, buf, size - count);
    if (r == -1) return -1;
    count += r;
    buf += r;
  }
  return count;
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
    hex(offset, 8, &outbuf[outbase]);
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
  const int num_lines = 16 * 1024 / args->num_columns;
  const int inbuf_size = args->num_columns * num_lines;
  const int outbuf_size = line_width * num_lines;
  uint8_t* const inbuf = (uint8_t*) malloc(inbuf_size);
  uint8_t* const outbuf = (uint8_t*) malloc(outbuf_size);
  int32_t n2o[args->num_columns * 3];
  int32_t o2n[sizeof(n2o) / sizeof(n2o[0]) * 2];
  int32_t cursor = -1;
  for (int i = 0; i < sizeof(n2o) / sizeof(n2o[0]) / 2; ++i) {
    if (i % args->group_size == 0) ++cursor;
    n2o[2 * i] = cursor++;
    n2o[2 * i + 1] = cursor++;
  }
  memset(o2n, -1, sizeof(o2n));
  for (int i = 0; i < sizeof(n2o) / sizeof(n2o[0]); ++i) {
    o2n[n2o[i]] = i;
  }
  int8_t next_rel[args->num_columns / 8 + 1];
  int8_t o2n_rel[sizeof(o2n) / sizeof(o2n[0])];
  memset(o2n_rel, 0, sizeof(o2n_rel));
  int last_out = 0;
  for (int i = 0; i < sizeof(next_rel); ++i) {
    next_rel[i] = n2o[((i + 1) * 8 - 1) * 2 + 1] + 1 - last_out;
    last_out += next_rel[i];
  }
  for (int i = 0, out_off = 0; i < sizeof(next_rel); out_off += next_rel[i], ++i)  {
    for (int j = 0; j < next_rel[i]; ++j) {
      o2n_rel[out_off + j] = (o2n[out_off + j] == -1) ? -1 : (o2n[out_off + j] - i * 8 * 2);
    }
  }
  // printf("n2o:\t\t");
  // for (int i = 0; i < sizeof(n2o) / sizeof(n2o[0]); ++i) printf(" %3d", n2o[i]);
  // printf("\no2n:\t\t");
  // for (int i = 0; i < sizeof(o2n) / sizeof(o2n[0]); ++i) printf(" %3d", o2n[i]);
  // printf("\nnext_rel:\t");
  // for (int i = 0; i < sizeof(next_rel); ++i) printf(" %3d", next_rel[i]);
  // printf("\no2n_rel:\t");
  // for (int i = 0; i < sizeof(o2n_rel); ++i) printf(" %3d", o2n_rel[i]);
  // printf("\n");
  if (!outbuf || !inbuf) {
    perror("allocation error");
    exit(EXIT_FAILURE);
  }
  size_t offset = 0;
  int inbuf_read;
  do {
    inbuf_read = read_exactly(args->input_fd, inbuf, inbuf_size);
    if (inbuf_read == -1) {
      perror("input error");
      exit(EXIT_FAILURE);
    }
    int written = render_xxd(inbuf, inbuf_read, args->num_columns, args->group_size, offset, next_rel, o2n_rel, outbuf);
    offset += inbuf_read;
    if (write_exactly(args->output_fd, outbuf, written) != written) {
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
