#include <stdint.h>

struct render_option {
  int num_columns;
  int group_size;
  int8_t* next_rel;
  int8_t* o2n_rel;
};

int get_line_width(const int num_columns, const int group_size);

int render_xxd(const uint8_t* const inbuf, const int inbuf_len, size_t offset, struct render_option* ropt, uint8_t* outbuf);

void initialize_render_option(int num_columns, int group_size, struct render_option* ropt);

void finalize_render_option(struct render_option* ropt);
