#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <CL/cl.h>
#include "clerror.h"

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

void check_cl_error(const char* desc, cl_int errcode) {
  if (errcode != CL_SUCCESS) {
    fprintf(stderr, "%s: %s(%d)\n", desc, clGetErrorString(errcode), errcode);
    exit(EXIT_FAILURE);
  }
}

cl_context g_ocl_context = 0;
cl_command_queue g_ocl_queue = 0;
cl_program g_ocl_program = 0;
cl_kernel g_ocl_kernel = 0;

void destroy_renderer() {
  if (g_ocl_kernel) clReleaseKernel(g_ocl_kernel);
  if (g_ocl_program) clReleaseProgram(g_ocl_program);
  if (g_ocl_queue) clReleaseCommandQueue(g_ocl_queue);
  if (g_ocl_context) clReleaseContext(g_ocl_context);
}

extern const char _binary_render_kernel_spv_start;
extern char _binary_render_kernel_spv_size[];

void initialize_renderer() {
  atexit(destroy_renderer);
  cl_int ok;
  cl_platform_id platform;
  check_cl_error("cl platform", clGetPlatformIDs(1, &platform, 0));
  g_ocl_context = clCreateContextFromType((cl_context_properties[]){ CL_CONTEXT_PLATFORM, platform, 0 }, CL_DEVICE_TYPE_GPU, 0, 0, &ok);
  check_cl_error("cl context", ok);
  cl_device_id device;
  check_cl_error("cl get root device", clGetContextInfo(g_ocl_context, CL_CONTEXT_DEVICES, sizeof(device), &device, 0));
  g_ocl_queue = clCreateCommandQueueWithProperties(g_ocl_context, device, 0, &ok);
  check_cl_error("cl command queue", ok);
  // g_ocl_program = clCreateProgramWithIL(g_ocl_context, &_binary_render_kernel_spv_start, (size_t)&_binary_render_kernel_spv_size, &ok);
  g_ocl_program = clCreateProgramWithSource(g_ocl_context, 1, (char*[]){ &_binary_render_kernel_spv_start }, (size_t[]){ (size_t)_binary_render_kernel_spv_size }, &ok);
  check_cl_error("cl program", ok);
  ok = clBuildProgram(g_ocl_program, 1, &device, 0, 0, 0);
  if (ok == CL_BUILD_PROGRAM_FAILURE){
    static char buf[4096];
    size_t log_size;
    check_cl_error("cl get build log", clGetProgramBuildInfo(g_ocl_program, device, CL_PROGRAM_BUILD_LOG, sizeof(buf), buf, &log_size));
    fprintf(stderr, "build log:\n%.*s\n", (int)log_size, buf);
  }
  check_cl_error("cl build program", ok);
  if (0) {
    static char names[1024];
    check_cl_error("cl get kernel names", clGetProgramInfo(g_ocl_program, CL_PROGRAM_KERNEL_NAMES, sizeof(names), names, 0));
    fprintf(stderr, "available kernels: %s\n", names);
  }
  g_ocl_kernel = clCreateKernel(g_ocl_program, "render_xxd", &ok);
  check_cl_error("cl kernel", ok);
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

int get_hex_width(const int num_columns, const int group_size) {
  int num_groups = num_columns / group_size;
  if (num_columns % group_size) ++num_groups;
  return num_columns * 2 + num_groups - 1;
}

int get_line_width(const int num_columns, const int group_size) {
  return 10 + get_hex_width(num_columns, group_size) + 2 + num_columns + 1;
}

int render_xxd(const char* const inbuf, const int inbuf_len, const int num_columns, const int group_size, size_t offset, char* outbuf) {
  const int num_full_lines = inbuf_len / num_columns;
  const int last_line_columns = inbuf_len % num_columns;
  const int num_lines = num_full_lines + (last_line_columns ? 1 : 0);
  const int num_full_groups = num_columns / group_size;
  const int last_group_columns = num_columns % group_size;
  const int hex_width = num_full_groups * (group_size * 2 + 1) - 1 +
    (last_group_columns ? 1 + last_group_columns * 2 : 0);
  const int ascii_width = num_columns;
  const int line_width = 10 + hex_width + 2 + ascii_width + 1;
  check_cl_error("set kernel args", clSetKernelArgSVMPointer(g_ocl_kernel, 0, inbuf));
  check_cl_error("set kernel args", clSetKernelArg(g_ocl_kernel, 1, sizeof(inbuf_len), &inbuf_len));
  check_cl_error("set kernel args", clSetKernelArg(g_ocl_kernel, 2, sizeof(num_columns), &num_columns));
  check_cl_error("set kernel args", clSetKernelArg(g_ocl_kernel, 3, sizeof(group_size), &group_size));
  check_cl_error("set kernel args", clSetKernelArg(g_ocl_kernel, 4, sizeof(offset), &offset));
  check_cl_error("set kernel args", clSetKernelArgSVMPointer(g_ocl_kernel, 5, outbuf));
  cl_event event;
  check_cl_error("execute kernel", clEnqueueNDRangeKernel(g_ocl_queue, g_ocl_kernel, 1, 0, (size_t[]){ num_lines }, 0, 0, 0, &event));
  check_cl_error("kernel completion", clWaitForEvents(1, &event));
  return num_full_lines * line_width +
    (last_line_columns ? line_width - (num_columns - last_line_columns) : 0);
}

void dump(const dump_args_t* args) {
  const int line_width = get_line_width(args->num_columns, args->group_size);
  const int num_lines = 16 * 1024 * 1024 / args->num_columns;
  const int inbuf_size = args->num_columns * num_lines;
  const int outbuf_size = line_width * num_lines;
  char* const inbuf = (char*) clSVMAlloc(g_ocl_context, CL_MEM_READ_WRITE | CL_MEM_SVM_FINE_GRAIN_BUFFER, inbuf_size, 0);
  char* const outbuf = (char*) clSVMAlloc(g_ocl_context, CL_MEM_READ_WRITE | CL_MEM_SVM_FINE_GRAIN_BUFFER, outbuf_size, 0);
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
    int written = render_xxd(inbuf, inbuf_read, args->num_columns, args->group_size, offset, outbuf);
    offset += inbuf_read;
    if (write_exactly(args->output_fd, outbuf, written) != written) {
      perror("output error");
      exit(EXIT_FAILURE);
    }
  } while (inbuf_read == inbuf_size);
  clSVMFree(g_ocl_context, inbuf);
  clSVMFree(g_ocl_context, outbuf);
}

int main(int argc, const char** argv) {
  cli_args_t cli_args;
  parse_cli(argc - 1, argv + 1, &cli_args);
  initialize_renderer();
  switch (cli_args.cmd) {
  case CMD_DUMP:
    dump(&cli_args.dump_args);
    break;
  }
  return 0;
}
