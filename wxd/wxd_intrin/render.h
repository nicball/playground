#include <stdint.h>

void render_ascii(const uint8_t* const inbuf, const int inbuf_len, uint8_t* const outbuf);

void render_hex(const uint8_t* const inbuf, const int8_t inbuf_len, const int8_t* const n2o, const int8_t* const o2n, uint8_t* const outbuf);
