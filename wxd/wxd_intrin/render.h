#include <stdint.h>

void render_ascii(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf);

int32_t render_hex(const uint8_t* const inbuf, const int32_t inbuf_len, const int8_t* const next_rel, const int8_t* const o2n_rel, uint8_t* const outbuf);
