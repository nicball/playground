#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <memory>
#include <optional>
#include <queue>
#include <span>
#include <string>
#include <string>
#include <thread>
#include <vector>
#include <fcntl.h>
#include <unistd.h>
#include "channel.h"
#include "log.h"
#include "render.h"

typedef enum {
  CMD_DUMP
} command_t;

typedef struct {
  int num_columns;
  int group_size;
  int num_jobs;
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
  result->num_jobs = 2;
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
    else if (!strcmp(args[i], "-j") || !strcmp(args[i], "--num-jobs")) {
      ++i;
      if (!sscanf(args[i], "%d", &result->num_jobs) || result->num_jobs <= 0) {
        fprintf(stderr, "invalid jobs number: %s\n", args[i]);
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
  result->group_size = std::min(result->group_size, result->num_columns);
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
  renderer renderer{args->num_columns, args->group_size};
  const int num_lines = 2048 * args->num_jobs;
  const int inbuf_size = args->num_columns * num_lines;
  const int outbuf_size = renderer.get_line_width() * num_lines;
  const int num_workers = args->num_jobs;
  using buffer_ptr = std::unique_ptr<std::vector<uint8_t>>;
  channel<buffer_ptr> inbuf_pool{"inbuf_pool"};
  for (int i = 0; i < num_workers * 4; ++i) {
    inbuf_pool.put(new std::vector<uint8_t>(inbuf_size));
  }
  channel<buffer_ptr> outbuf_pool{"outbuf_pool"};
  for (int i = 0; i < num_workers * 4; ++i) {
    outbuf_pool.put(new std::vector<uint8_t>(outbuf_size));
  }
  struct inblock {
    size_t offset;
    int inbuf_read;
    buffer_ptr inbuf;
    buffer_ptr outbuf;
  };
  channel<std::optional<inblock>> rd2xc{"rd2xc"};
  struct outblock {
    int written;
    buffer_ptr outbuf;
  };
  channel<std::optional<outblock>> xc2wr{"xc2wr"};
  std::jthread reader{[&] {
    size_t offset = 0;
    int inbuf_read;
    do {
      auto inbuf = inbuf_pool.take();
      inbuf_read = read_exactly(args->input_file, inbuf->data(), inbuf_size);
      if (inbuf_read == -1) {
        perror("input error");
        exit(EXIT_FAILURE);
      }
      auto outbuf = outbuf_pool.take();
      LOG_DEBUG("reader: allocated buf (%p, %p), read %d bytes.\n", (void*)inbuf.get(), (void*)outbuf.get(), inbuf_read);
      if (inbuf_read == 0) rd2xc.put(std::nullopt);
      else rd2xc.put(std::in_place, offset, inbuf_read, std::move(inbuf), std::move(outbuf));
      offset += inbuf_read;
    } while (inbuf_read != 0);
  }};
  std::jthread transcoder{[&] {
    struct work_item {
      int work_id;
      size_t offset;
      std::vector<uint8_t>* inbuf_base;
      std::span<uint8_t> inbuf;
      std::vector<uint8_t>* outbuf_base;
      std::span<uint8_t> outbuf;
    };
    std::vector<std::string> names;
    for (int i = 0; i < num_workers; ++i) {
      names.emplace_back("work_queue_" + std::to_string(i));
    }
    std::vector<channel<std::optional<work_item>>> work_queues{names.begin(), names.end()};
    std::vector<std::jthread> workers;
    struct commit_item {
      int work_id;
      int partly_written;
      std::vector<uint8_t>* inbuf;
      std::vector<uint8_t>* outbuf;
      bool operator<(const commit_item& other) const {
        return work_id > other.work_id;
      }
    };
    channel<std::optional<commit_item>> commit_queue{"commit_queue"};
    for (int i = 0; i < num_workers; ++i) {
      workers.emplace_back([&, worker_id = i] {
        for (;;) {
          auto work = work_queues[worker_id].take();
          if (!work) {
            commit_queue.put(std::nullopt);
            break;
          }
          LOG_DEBUG("worker %d: got buffer %p with %d bytes.\n", worker_id, (void*)work->inbuf.data(), (int)work->inbuf.size());
          int written = renderer.render_xxd(
            work->inbuf.data(),
            work->inbuf.size(),
            work->offset,
            work->outbuf.data()
          );
          commit_queue.put(std::in_place, work->work_id, written, work->inbuf_base, work->outbuf_base);
        }
      });
    }
    std::jthread sequencer{[&] {
      int next_work_id = 0;
      int current_size = 0;
      std::vector<uint8_t>* current_inbuf;
      std::vector<uint8_t>* current_outbuf;
      int current_written;
      int done_count = 0;
      std::priority_queue<commit_item> wait_queue;
      for (;;) {
        auto commit = commit_queue.take();
        if (!commit) {
          ++done_count;
          if (done_count == num_workers) {
            xc2wr.put(std::nullopt);
            break;
          }
        }
        else {
          LOG_DEBUG("sequencer: got commit %d\n", commit->work_id);
          wait_queue.emplace(std::move(*commit));
          while (wait_queue.top().work_id == next_work_id) {
            ++next_work_id;
            if (current_size == 0) {
              current_inbuf = wait_queue.top().inbuf;
              current_outbuf = wait_queue.top().outbuf;
              current_written = wait_queue.top().partly_written;
            }
            else {
              assert(wait_queue.top().inbuf == current_inbuf);
              assert(wait_queue.top().outbuf == current_outbuf);
              current_written += wait_queue.top().partly_written;
            }
            ++current_size;
            wait_queue.pop();
            if (current_size == num_workers) {
              current_size = 0;
              inbuf_pool.put(buffer_ptr{current_inbuf});
              xc2wr.put(std::in_place, current_written, buffer_ptr{current_outbuf});
            }
          }
        }
      }
    }};
    /* balancer */ {
      int work_count = 0;
      for (;;) {
        auto inblock = rd2xc.take();
        if (!inblock) {
          for (int i = 0; i < num_workers; ++i) {
            work_queues[i].put(std::nullopt);
          }
          break;
        }
        else {
          int worker_num_lines = inblock->inbuf_read / args->num_columns / num_workers;
          int inbuf_size = worker_num_lines * args->num_columns;
          int outbuf_size = worker_num_lines * renderer.get_line_width();
          int rest = inblock->inbuf_read - (inbuf_size * (num_workers - 1));
          std::vector<uint8_t>* inbuf = inblock->inbuf.release();
          std::vector<uint8_t>* outbuf = inblock->outbuf.release();
          for (int i = 0; i < num_workers; ++i) {
            LOG_DEBUG("balancer: dispatching work to worker %d\n", i);
            work_queues[i].put(
              std::in_place,
              work_count + i,
              inblock->offset + inbuf_size * i,
              inbuf,
              std::span{&inbuf->data()[inbuf_size * i], static_cast<size_t>((i == num_workers - 1) ? rest : inbuf_size)},
              outbuf,
              std::span{&outbuf->data()[outbuf_size * i], static_cast<size_t>(outbuf_size)}
            );
          }
          work_count += num_workers;
        }
      }
    }
  }};
  std::jthread writer{[&] {
    for (;;) {
      auto outblock = xc2wr.take();
      if (!outblock) break;
      LOG_DEBUG("received outbuf %p with %d bytes.\n", (void*)outblock->outbuf.get(), outblock->written);
      if (write_exactly(args->output_file, outblock->outbuf->data(), outblock->written) != outblock->written) {
        perror("output error");
        exit(EXIT_FAILURE);
      }
      LOG_DEBUG("freeing outbuf %p.\n", (void*)outblock->outbuf.get());
      outbuf_pool.put(std::move(outblock->outbuf));
    }
  }};
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
