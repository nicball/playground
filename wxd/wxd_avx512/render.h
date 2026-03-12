#include <stdint.h>
#include <vector>
#include <immintrin.h>

class renderer {
  const int num_columns;
  const int group_size;
  int num_batches;
  const int hex_width;
  std::vector<__mmask64> spread_masks;
  std::vector<int> in_offs;
  std::vector<int> out_offs;
  std::vector<__mmask32> in_masks;
  std::vector<__mmask64> out_masks;

  int get_hex_width();
  void render_line_no(const uint64_t* const line_no, uint8_t* const outbuf);
  void render_ascii(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf);
  int32_t render_hex(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf);

public:
  renderer(int num_columns, int group_size);
  int render_xxd(const uint8_t* const inbuf, const int inbuf_len, size_t offset, uint8_t* outbuf);
  int get_line_width();
};
