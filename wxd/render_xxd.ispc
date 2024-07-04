// static uniform const int8 to_s[0x10] = {
//   0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
//   0x61, 0x62, 0x63, 0x64, 0x65, 0x66
// };

static inline int8 nibble(const int8 lf) {
  const bool c = lf < 10;
  return select(c, lf + 0x30, lf - 10 + 0x61);
}

static inline uniform int8 nibble(uniform const int8 lf) {
  uniform const bool c = lf < 10;
  return select(c, lf + 0x30, lf - 10 + 0x61);
}

static inline void hex(uniform const uint32 value, int8* const uniform strbuf) {
  strbuf[0] = nibble(value >> 28);
  strbuf[1] = nibble((value >> 24) & 0xF);
  strbuf[2] = nibble((value >> 20) & 0xF);
  strbuf[3] = nibble((value >> 16) & 0xF);
  strbuf[4] = nibble((value >> 12) & 0xF);
  strbuf[5] = nibble((value >> 8) & 0xF);
  strbuf[6] = nibble((value >> 4) & 0xF);
  strbuf[7] = nibble(value & 0xF);
}

static inline void hex(const uint8 value, int8* const strbuf) {
  strbuf[0] = nibble(value >> 4);
  strbuf[1] = nibble(value & 0xF);
}

static inline uniform int get_hex_width(uniform const int num_columns, uniform const int group_size) {
  uniform int num_groups = num_columns / group_size;
  if (num_columns % group_size) ++num_groups;
  return num_columns * 2 + num_groups - 1;
}

static inline uniform int get_line_width(uniform const int num_columns, uniform const int group_size) {
  return 10 + get_hex_width(num_columns, group_size) + 2 + num_columns + 1;
}

export uniform int render_xxd(
  const int8* const uniform inbuf,
  uniform const int inbuf_len,
  uniform const int num_columns,
  uniform const int group_size,
  uniform size_t offset,
  int8* uniform outbuf
) {
  uniform const int hex_width = get_hex_width(num_columns, group_size);
  uniform const int line_width = get_line_width(num_columns, group_size);
  uniform const int num_full_rows = inbuf_len / num_columns;
  uniform const int last_row = inbuf_len % num_columns;
  uniform const int num_rows = num_full_rows + (last_row ? 1 : 0);
  for (uniform int line_no = 0; line_no < num_rows; ++line_no) {
    uniform const int inbase = line_no * num_columns;
    uniform const int outbase = inbase / num_columns * line_width;
    uniform const int to_end = inbuf_len - inbase;
    uniform const int r = to_end > num_columns ? num_columns : to_end;
    hex((uniform uint32) (offset + inbase), &outbuf[outbase]);
    outbuf[outbase + 8] = 0x3a;
    uniform const int hex_start = outbase + 9;
    uniform const int num_full_groups = r / group_size;
    uniform const int last_group = r % group_size;
    uniform const int num_groups = num_full_groups + (last_group ? 1 : 0);
    foreach (gid = 0 ... num_groups) {
      const int gbase = hex_start + gid * (group_size * 2 + 1);
      outbuf[gbase] = 0x20;
      for (uniform int cing = 0; cing < group_size; ++cing) {
        hex((uint8) inbuf[inbase + gid * group_size + cing], &outbuf[gbase + 1 + cing * 2]);
      }
    }
    uniform const int hex_end = hex_start + num_groups * (group_size * 2 + 1);
    uniform const int ascii_start = outbase + line_width - num_columns - 1;
    foreach (i = hex_end ... ascii_start) {
      outbuf[i] = 0x20;
    }
    foreach (i = 0 ... r) {
      int8 b = inbuf[inbase + i];
      outbuf[ascii_start + i] = select(and(0x20 <= b, b <= 0x7e), b, (varying int8) 0x2e);
    }
    uniform const int ascii_end = ascii_start + r;
    foreach (i = ascii_end ... outbase + line_width - 1) {
      outbuf[i] = 0x20;
    }
    outbuf[outbase + line_width - 1] = 0x0a;
  }
  return num_rows * line_width;
}
