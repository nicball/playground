#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>
#include <fcntl.h>
#include "render.h"
#include "util.h"

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

void dump(const dump_args_t* args) {
  const int line_width = get_line_width(args->num_columns, args->group_size);
  const int num_lines = 1024;
  const int inbuf_size = args->num_columns * num_lines;
  const int outbuf_size = line_width * num_lines;
  uint8_t* const inbuf = (uint8_t*) malloc(inbuf_size);
  uint8_t* const outbuf = (uint8_t*) malloc(outbuf_size);
  struct render_option ropt;
  initialize_render_option(args->num_columns, args->group_size, &ropt);
  if (!inbuf || !outbuf) {
    perror("allocation error");
    exit(EXIT_FAILURE);
  }
  size_t offset = 0;
  int inbuf_read;
  do {
    inbuf_read = read_exactly(args->input_file, inbuf, inbuf_size);
    if (inbuf_read == -1) {
      perror("input error");
      exit(EXIT_FAILURE);
    }
    int written = render_xxd(inbuf, inbuf_read, offset, &ropt, outbuf);
    offset += inbuf_read;
    if (write_exactly(args->output_file, outbuf, written) != written) {
      perror("output error");
      exit(EXIT_FAILURE);
    }
  } while (inbuf_read == inbuf_size);
  free(inbuf);
  free(outbuf);
  finalize_render_option(&ropt);
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
