#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <algorithm>
#include "render.h"

int renderer::get_hex_width() {
  int num_groups = num_columns / group_size;
  if (num_columns % group_size) ++num_groups;
  return num_columns * 2 + num_groups - 1;
}

int renderer::get_line_width() {
  return 10 + get_hex_width() + 2 + num_columns + 1;
}

renderer::renderer(int num_columns, int group_size): num_columns{num_columns}, group_size{group_size}, num_batches{0}, hex_width{get_hex_width()} {
  int cursor = 0;
  const int batch_size = 64;
  int in_base = 0;
  int out_base = 0;
  uint64_t sm = 0;
  auto commit = [&](int in_off) {
    // printf("COMMIT cursor=%d out_base=%d (%d) in_base=%d in_off=%d (%d)\n", cursor, out_base, cursor - out_base, in_base, in_off, in_off - in_base);
    int out_len = cursor - out_base;
    out_masks.push_back(out_len == 64 ? (-1ULL) : ((1ULL << out_len) - 1));
    out_offs.push_back(out_base);
    out_base = cursor;
    spread_masks.push_back(_cvtu64_mask64(sm));
    sm = 0;
    in_offs.push_back(in_base);
    int in_len = in_off - in_base;
    in_masks.push_back(in_len == 32 ? (-1U) : ((1U << in_len) - 1));
    in_base = in_off;
    ++num_batches;
  };
  int i = 0;
  for (; i < num_columns; ++i) {
    bool space_prefix = i != 0 && i % group_size == 0;
    int out_size = space_prefix ? 3 : 2;
    if (cursor - out_base + out_size > batch_size) commit(i);
    if (space_prefix) ++cursor;
    sm |= 0b11ULL << (cursor - out_base);
    cursor += 2;
    if (cursor - out_base >= batch_size) commit(i + 1);
  }
  if (cursor != out_base) commit(i);
  // for (int i = 0; i < spread_masks.size(); ++i) {
  //   printf("%llx %d %d %x %llx\n", spread_masks[i], in_offs[i], out_offs[i], in_masks[i], out_masks[i]);
  // }
}

void renderer::render_line_no(const uint64_t* const line_no, uint8_t* const outbuf) {
  const __m128i to_s = _mm_set_epi8(
    'f', 'e', 'd', 'c', 'b', 'a', '9', '8', '7', '6', '5', '4', '3', '2', '1', '0');
  const __m128i byte = _mm_cvtepu8_epi16(_mm_cvtsi64_si128(_loadbe_i32(line_no)));
  const __m128i byte_hi = _mm_srli_epi16(byte, 4);
  const __m128i byte_lo = _mm_and_si128(_mm_slli_epi16(byte, 8), _mm_set1_epi8(0xF));
  const __m128i nib_value = _mm_or_si128(byte_hi, byte_lo);
  const __m128i nib = _mm_shuffle_epi8(to_s, nib_value);
  _mm_storeu_si64(outbuf, nib);
}

void renderer::render_ascii(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf) {
  for (int i = 0; i < inbuf_len; i += 64) {
    const int len = std::min(inbuf_len - i, 64);
    const __mmask64 prefix_k = len == 64 ? (-1ULL) : ((1ULL << len) - 1);
    const __m512i bytes = _mm512_maskz_loadu_epi8(prefix_k, inbuf + i);
    const __mmask64 printable_k = _kand_mask64(
      _mm512_cmpgt_epu8_mask(bytes, _mm512_set1_epi8(' ' - 1)),
      _mm512_cmpgt_epu8_mask(_mm512_set1_epi8('~' + 1), bytes));
    const __m512i ascii = _mm512_mask_blend_epi8(
      printable_k,
      _mm512_set1_epi8('.'),
      bytes);
    _mm512_mask_storeu_epi8(outbuf + i, prefix_k, ascii);
  }
}

int32_t renderer::render_hex(const uint8_t* const inbuf, const int32_t inbuf_len, uint8_t* const outbuf) {
  if (inbuf_len == num_columns) {
    const __m512i to_s = _mm512_broadcast_i32x4(_mm_set_epi8(
      'f', 'e', 'd', 'c', 'b', 'a', '9', '8', '7', '6', '5', '4', '3', '2', '1', '0'));
    for (int i = 0; i < num_batches; ++i) {
      const __m512i bytes = _mm512_cvtepu8_epi16(_mm256_maskz_loadu_epi8(in_masks[i], inbuf + in_offs[i]));
      const __m512i bytes_hi = _mm512_srli_epi16(bytes, 4);
      const __m512i bytes_lo = _mm512_and_epi32(_mm512_set1_epi8(0xF), _mm512_slli_epi16(bytes, 8));
      const __m512i nibs = _mm512_shuffle_epi8(to_s, _mm512_or_epi32(bytes_hi, bytes_lo));
      const __m512i out = _mm512_mask_expand_epi8(_mm512_set1_epi8(' '), spread_masks[i], nibs);
      _mm512_mask_storeu_epi8(outbuf + out_offs[i], out_masks[i], out);
    }
    return hex_width;
  }
  else {
    int cursor = 0;
    for (int i = 0; i < inbuf_len; ++i) {
      if (i != 0 && i % group_size == 0) outbuf[cursor++] = ' ';
      const char to_s[] = "0123456789abcdef";
      outbuf[cursor++] = to_s[inbuf[i] >> 4];
      outbuf[cursor++] = to_s[inbuf[i] & 0xF];
    }
    return cursor;
  }
}

int renderer::render_xxd(const uint8_t* const inbuf, const int inbuf_len, size_t offset, uint8_t* outbuf) {
  int cursor = 0;
  const int line_width = get_line_width();
  for (int inbase = 0; inbase < inbuf_len; inbase += num_columns) {
    const int outbase = inbase / num_columns * line_width;
    const int r = std::min(num_columns, inbuf_len - inbase);
    render_line_no(&offset, &outbuf[outbase]);
    outbuf[outbase + 8] = ':';
    outbuf[outbase + 9] = ' ';
    cursor = outbase + 10;
    cursor += render_hex(&inbuf[inbase], r, &outbuf[cursor]);
    if (r == num_columns) {
      outbuf[cursor] = outbuf[cursor + 1] = ' ';
      cursor += 2;
    }
    else while (cursor < outbase + line_width - num_columns - 1) {
      outbuf[cursor++] = ' ';
    }
    render_ascii(&inbuf[inbase], r, &outbuf[cursor]);
    outbuf[cursor + r] = '\n';
    cursor += r + 1;
    offset += r;
  }
  return cursor;
}
