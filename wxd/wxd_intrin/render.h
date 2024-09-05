#include <stdint.h>
#include <vector>

class renderer {
  int num_columns;
  int group_size;
  std::vector<int> n2o;
  std::vector<int8_t> next_rel;
  std::vector<int8_t> o2n_rel;

  int get_hex_width();
  void render_line_no(const uint64_t* const line_no, uint8_t* const outbuf);
  void render_ascii(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf);
  int32_t render_hex(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf);

public:
  renderer(int num_columns, int group_size);
  int render_xxd(const uint8_t* const inbuf, const int inbuf_len, size_t offset, uint8_t* outbuf);
  int get_line_width();
};
