#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

ssize_t min(ssize_t a, ssize_t b) { return a < b ? a : b; }

int get_hex_width(const int num_columns, const int group_size) {
  int num_groups = num_columns / group_size;
  if (num_columns % group_size) ++num_groups;
  return num_columns * 2 + num_groups - 1;
}

int get_line_width(const int num_columns, const int group_size) {
  return 10 + get_hex_width(num_columns, group_size) + 2 + num_columns + 1;
}

off_t translate_size(off_t input_size, int num_columns, int group_size) {
  off_t num_lines = input_size / num_columns;
  if (input_size % num_columns) ++num_lines;
  int line_width = get_line_width(num_columns, group_size);
  return num_lines * line_width;
}

typedef enum {
  CMD_DUMP
} command_t;

typedef struct {
  int num_columns;
  int group_size;
  int input_fd;
  off_t input_size;
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
  result->group_size = min(result->group_size, result->num_columns);
  if (got_positionals < 2) {
    fputs("this version of wxd only supports regular files", stderr);
    exit(EXIT_FAILURE);
  }
  int fd = open(positionals[0], O_RDONLY);
  if (fd == -1) {
    perror("cannot open input");
    exit(EXIT_FAILURE);
  }
  if ((result->input_size = lseek(fd, 0, SEEK_END)) == -1) {
    perror("input is unseekable");
    exit(EXIT_FAILURE);
  }
  if (lseek(fd, 0, SEEK_SET) == -1) {
    perror(0);
    exit(EXIT_FAILURE);
  }
  result->input_fd = fd;
  fd = open(positionals[1], O_RDWR | O_CREAT, 0644);
  if (fd == -1) {
    perror("cannot open output");
    exit(EXIT_FAILURE);
  }
  if (ftruncate(fd, translate_size(result->input_size, result->num_columns, result->group_size)) == -1) {
    perror("cannot resize output");
    exit(EXIT_FAILURE);
  }
  result->output_fd = fd;
}

void parse_cli(const int len, const char** args, cli_args_t* result) {
  if (len < 1 || strcmp(args[0], "dump")) {
    puts("fuck you");
    exit(EXIT_FAILURE);
  }
  result->cmd = CMD_DUMP;
  parse_dump(len - 1, args + 1, &result->dump_args);
}

void hex(unsigned long long value, const int width, char* const out) {
  static const char to_s[0xF + 1] = "0123456789abcdef";
  for (int i = width - 1; i >= 0; --i) {
    int d = value & 0xF;
    value = value >> 4;
    out[i] = to_s[d];
  }
}

int read_exactly(const int fd, char* buf, const int size) {
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

int write_exactly(const int fd, char* buf, const int size) {
  int count = 0;
  while (count != size) {
    ssize_t r = write(fd, buf, size - count);
    if (r == -1) return -1;
    count += r;
    buf += r;
  }
  return count;
}

ssize_t render_xxd(const char* const restrict inbuf, const ssize_t inbuf_len, const int num_columns, const int group_size, ssize_t offset, char* restrict outbuf) {
  ssize_t cursor;
  const int hex_width = get_hex_width(num_columns, group_size);
  const int line_width = get_line_width(num_columns, group_size);
  for (ssize_t inbase = 0; inbase < inbuf_len; inbase += num_columns) {
    const ssize_t outbase = inbase / num_columns * line_width;
    const int r = min(num_columns, inbuf_len - inbase);
    hex(offset, 8, &outbuf[outbase]);
    outbuf[outbase + 8] = ':';
    cursor = outbase + 9;
    for (int i = 0; i < r; ++i) {
      if (i % group_size == 0) {
        outbuf[cursor++] = ' ';
      }
      hex((unsigned char) inbuf[inbase + i], 2, &outbuf[cursor]);
      cursor += 2;
    }
    while (cursor < outbase + line_width - num_columns - 1) {
      outbuf[cursor++] = ' ';
    }
    for (int i = 0; i < r; ++i) {
      outbuf[cursor + i] = 0x20 <= inbuf[inbase + i] && inbuf[inbase + i] <= 0x7e ? inbuf[inbase + i] : '.';
    }
    outbuf[cursor + r] = '\n';
    cursor += r + 1;
    offset += r;
  }
  return cursor;
}

void dump(const dump_args_t* args) {
  const char* const inbuf = (char*) mmap(0, args->input_size, PROT_READ, MAP_PRIVATE, args->input_fd, 0);
  if (inbuf == MAP_FAILED) {
    perror("cannot map input");
    exit(EXIT_FAILURE);
  }
  char* const outbuf = (char*) mmap(0, translate_size(args->input_size, args->num_columns, args->group_size), PROT_READ | PROT_WRITE, MAP_SHARED, args->output_fd, 0);
  if (outbuf == MAP_FAILED) {
    perror("cannot map output");
    exit(EXIT_FAILURE);
  }
  render_xxd(inbuf, args->input_size, args->num_columns, args->group_size, 0, outbuf);
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
