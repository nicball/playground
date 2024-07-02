static inline void hex(int value, uniform const int width, int8* const out) {
  static uniform const int8 to_s[0x10] = {
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
    0x61, 0x62, 0x63, 0x64, 0x65, 0x66
  };
  for (int i = 0; i < width; ++i) {
    int8 d = (value >> (4 * i)) & 0xF;
    int8 c = d < 10;
    out[width - 1 - i] = c * (d + 0x30) + ((int8) !c) * (d - 10 + 0x61);
  }
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
    hex(offset + inbase, 8, &outbuf[outbase]);
    outbuf[outbase + 8] = 0x3a;
    uniform const int hex_start = outbase + 9;
    uniform const int num_full_groups = r / group_size;
    uniform const int last_group = r % group_size;
    uniform const int num_groups = num_full_groups + (last_group ? 1 : 0);
    foreach (gid = 0 ... num_groups) {
      const int gbase = hex_start + gid * (group_size * 2 + 1);
      outbuf[gbase] = 0x20;
      for (int cing = 0; cing < group_size; ++cing) {
        hex((unsigned int8) inbuf[inbase + gid * group_size + cing], 2, &outbuf[gbase + 1 + cing * 2]);
      }
    }
    uniform const int hex_end = hex_start + num_groups * (group_size * 2 + 1);
    uniform const int ascii_start = outbase + line_width - num_columns - 1;
    foreach (i = hex_end ... ascii_start) {
      outbuf[i] = 0x20;
    }
    foreach (i = 0 ... r) {
      outbuf[ascii_start + i] = 0x20 <= inbuf[inbase + i] && inbuf[inbase + i] <= 0x7e ? inbuf[inbase + i] : 0x2e;
    }
    uniform const int ascii_end = ascii_start + r;
    foreach (i = ascii_end ... outbase + line_width - 1) {
      outbuf[i] = 0x20;
    }
    outbuf[outbase + line_width - 1] = 0x0a;
  }
  return num_rows * line_width;
}
