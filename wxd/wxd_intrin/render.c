#include <immintrin.h>
#include "render.h"

void render_ascii(const uint8_t* const inbuf, const int inbuf_len, uint8_t* const outbuf) {
  int i;
  for (i = 0; i + 16 <= inbuf_len; i += 16) {
    __m128i byte = _mm_loadu_si128((__m128i*)&inbuf[i]);
    __m128i ascii = _mm_blendv_epi8(
      _mm_set1_epi8('.'),
      byte,
      _mm_and_si128(
        _mm_cmpgt_epi8(byte, _mm_set1_epi8(' ' - 1)),
        _mm_cmpgt_epi8(_mm_set1_epi8('~' + 1), byte)));
    _mm_storeu_si128((__m128i*)&outbuf[i], ascii);
  }
  for(; i < inbuf_len; ++i)
    outbuf[i] = (' ' <= inbuf[i] && inbuf[i] <= '~') ? inbuf[i] : '.';
}

static inline __m128i i16x8_to_i8x8(__m128i a) {
  return _mm_shuffle_epi8(
    a,
    _mm_set_epi8(
      -1, -1, -1, -1, -1, -1, -1, -1,
      0x0E, 0x0C, 0x0A, 0x08, 0x06, 0x04, 0x02, 0x00));
}

void render_hex(
  const uint8_t* const inbuf,
  const int8_t inbuf_len,
  const int8_t* const n2o,
  const int8_t* const o2n,
  uint8_t* const outbuf
) {
  int8_t base;
  int8_t out_off = 0;
  for (base = 0; base + 8 <= inbuf_len; base += 8) {
    __m128i byte = _mm_cvtepu8_epi16(_mm_loadu_si64((__m128i*)&inbuf[base]));
    __m128i byte_hi = _mm_srli_epi16(byte, 4);
    __m128i byte_lo = _mm_and_si128(byte, _mm_set1_epi16(0xF));
    __m128i nib_hi = 
      _mm_blendv_epi8(
        _mm_add_epi16(byte_hi, _mm_set1_epi16('0')),
        _mm_add_epi16(byte_hi, _mm_set1_epi16('a' - 10)),
        _mm_cmpgt_epi16(byte_hi, _mm_set1_epi16(9)));
    __m128i nib_lo = 
      _mm_blendv_epi8(
        _mm_add_epi16(byte_lo, _mm_set1_epi16('0')),
        _mm_add_epi16(byte_lo, _mm_set1_epi16('a' - 10)),
        _mm_cmpgt_epi16(byte_lo, _mm_set1_epi16(9)));
    __m128i nib = _mm_unpacklo_epi8(i16x8_to_i8x8(nib_hi), i16x8_to_i8x8(nib_lo));
    int8_t out_end = n2o[(base + 7) * 2] + 2;
    __m128i out_full_idx = _mm_loadu_si128((__m128i*)&o2n[out_off]);
    __m128i out_full = _mm_blendv_epi8(
      _mm_shuffle_epi8(nib, _mm_sub_epi8(out_full_idx, _mm_set1_epi8(base * 2))),
      _mm_set1_epi8(' '),
      _mm_cmpeq_epi8(out_full_idx, _mm_set1_epi8(-1)));
    _mm_storeu_si128((__m128i*)&outbuf[out_off], out_full);
    __m128i out_rest_idx = _mm_loadu_si64((__m128i*)&o2n[out_off + 16]);
    __m128i out_rest = _mm_blendv_epi8(
      _mm_shuffle_epi8(nib, _mm_sub_epi8(out_rest_idx, _mm_set1_epi8(base * 2))),
      _mm_set1_epi8(' '),
      _mm_cmpeq_epi8(out_rest_idx, _mm_set1_epi8(-1)));
    uint64_t rest = _mm_cvtsi128_si64(out_rest);
    for (int8_t i = out_off + 16; i < out_end; ++i)
      outbuf[i] = rest >> ((i - (out_off + 16)) * 8);
    out_off = out_end;
  }
  uint8_t nibs[(inbuf_len - base) * 2];
  for (int8_t i = base; i < inbuf_len; ++i) {
    uint8_t b = inbuf[i];
    uint8_t hi = b >> 4;
    uint8_t lo = b & 0xF;
    nibs[(i - base) * 2] = (hi < 10) ? (hi + '0') : (hi - 10 + 'a');
    nibs[(i - base) * 2 + 1] = (lo < 10) ? (lo + '0') : (lo - 10 + 'a');
  }
  int8_t end = n2o[inbuf_len * 2];
  for (int8_t i = out_off; i < end; ++i) {
    int8_t idx = o2n[i];
    outbuf[i] = idx == -1 ? ' ' : nibs[idx - 2 * base];
  }
}
