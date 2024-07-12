#include <immintrin.h>
#include "render.h"

void render_line_no(const uint64_t* const line_no, uint8_t* const outbuf) {
  const __m128i to_s = _mm_set_epi8(
    'f', 'e', 'd', 'c', 'b', 'a', '9', '8', '7', '6', '5', '4', '3', '2', '1', '0');
  const __m128i byte = _mm_cvtepu8_epi16(_mm_cvtsi64_si128(_loadbe_i32(line_no)));
  const __m128i byte_hi = _mm_srli_epi16(byte, 4);
  const __m128i byte_lo = _mm_and_si128(_mm_slli_epi16(byte, 8), _mm_set1_epi8(0xF));
  const __m128i nib_value = _mm_or_si128(byte_hi, byte_lo);
  const __m128i nib = _mm_shuffle_epi8(to_s, nib_value);
  _mm_storeu_si64(outbuf, nib);
}

void render_ascii(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf) {
  int i;
  for (i = 0; i + 32 <= inbuf_len; i += 32) {
    const __m256i byte = _mm256_loadu_si256((__m256i*)&inbuf[i]);
    const __m256i ascii = _mm256_blendv_epi8(
      _mm256_set1_epi8('.'),
      byte,
      _mm256_and_si256(
        _mm256_cmpgt_epi8(byte, _mm256_set1_epi8(' ' - 1)),
        _mm256_cmpgt_epi8(_mm256_set1_epi8('~' + 1), byte)));
    _mm256_storeu_si256((__m256i*)&outbuf[i], ascii);
  }
  for (; i + 16 <= inbuf_len; i += 16) {
    const __m128i byte = _mm_loadu_si128((__m128i*)&inbuf[i]);
    const __m128i ascii = _mm_blendv_epi8(
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

int32_t render_hex(
  const uint8_t* const inbuf,
  const int32_t inbuf_len,
  const int8_t* const next_rel,
  const int8_t* const o2n_rel,
  uint8_t* const outbuf
) {
  int32_t base;
  int32_t out_off = 0;
  const __m128i to_s = _mm_set_epi8(
    'f', 'e', 'd', 'c', 'b', 'a', '9', '8', '7', '6', '5', '4', '3', '2', '1', '0');
  for (base = 0; base + 8 <= inbuf_len; base += 8) {
    const __m128i byte = _mm_cvtepu8_epi16(_mm_loadu_si64((__m128i*)&inbuf[base]));
    const __m128i byte_hi = _mm_srli_epi16(byte, 4);
    const __m128i byte_lo = _mm_and_si128(_mm_slli_epi16(byte, 8), _mm_set1_epi8(0xF));
    const __m128i nib_value = _mm_or_si128(byte_hi, byte_lo);
    const __m128i nib = _mm_shuffle_epi8(to_s, nib_value);
    const int32_t out_end = out_off + next_rel[base >> 3];
    const __m128i out_full_idx = _mm_loadu_si128((__m128i*)&o2n_rel[out_off]);
    const __m128i out_full = _mm_blendv_epi8(
      _mm_shuffle_epi8(nib, out_full_idx),
      _mm_set1_epi8(' '),
      _mm_cmpeq_epi8(out_full_idx, _mm_set1_epi8(-1)));
    _mm_storeu_si128((__m128i*)&outbuf[out_off], out_full);
    const __m128i out_rest_idx = _mm_loadu_si64((__m128i*)&o2n_rel[out_off + 16]);
    const __m128i out_rest = _mm_blendv_epi8(
      _mm_shuffle_epi8(nib, out_rest_idx),
      _mm_set1_epi8(' '),
      _mm_cmpeq_epi8(out_rest_idx, _mm_set1_epi8(-1)));
    uint64_t rest = _mm_cvtsi128_si64(out_rest);
    for (int32_t i = out_off + 16; i < out_end; ++i)
      outbuf[i] = rest >> ((i - (out_off + 16)) * 8);
    out_off = out_end;
  }
  uint8_t nibs[(inbuf_len - base) * 2];
  for (int32_t i = base; i < inbuf_len; ++i) {
    const uint8_t b = inbuf[i];
    const uint8_t hi = b >> 4;
    const uint8_t lo = b & 0xF;
    nibs[(i - base) * 2] = (hi < 10) ? (hi + '0') : (hi - 10 + 'a');
    nibs[(i - base) * 2 + 1] = (lo < 10) ? (lo + '0') : (lo - 10 + 'a');
  }
  int8_t idx;
  while ((idx = o2n_rel[out_off]) < (int8_t)sizeof(nibs)){
    outbuf[out_off++] = idx == -1 ? ' ' : nibs[idx];
    if (idx == sizeof(nibs) - 1) break;
  }
  return out_off;
}
