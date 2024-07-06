void hex(const unsigned int value, const int width, global char* const out) {
  for (int i = 0; i < width; ++i) {
    char d = (value >> (i * 4)) & 0xF;
    out[width - 1 - i] = d < 10 ? d + '0' : d - 10 + 'a';
  }
}

kernel void render_xxd(global const char* const inbuf, const int inbuf_len, const int num_columns, const int group_size, const long offset, global char* outbuf) {
  const int line_no = get_global_id(0);
  const int inbase = line_no * num_columns;
  const int num_full_groups = num_columns / group_size;
  const int last_group_columns = num_columns % group_size;
  const int hex_width = num_full_groups * (group_size * 2 + 1) - 1 +
    (last_group_columns ? 1 + last_group_columns * 2 : 0);
  const int ascii_width = num_columns;
  const int line_width = 10 + hex_width + 2 + ascii_width + 1;
  const int outbase = line_no * line_width;
  const int hex_start = outbase + 10;
  const int ascii_start = outbase + 10 + hex_width + 2;
  const int r = min(num_columns, inbuf_len - inbase);
  hex(offset + inbase, 8, &outbuf[outbase]);
  outbuf[outbase + 8] = ':';
  int cursor = outbase + 9;
  for (int i = 0; i < r; ++i) {
    if (i % group_size == 0) {
      outbuf[cursor++] = ' ';
    }
    hex((unsigned char) inbuf[inbase + i], 2, &outbuf[cursor]);
    cursor += 2;
  }
  while (cursor < ascii_start) {
    outbuf[cursor++] = ' ';
  }
  for (int i = 0; i < r; ++i) {
    outbuf[ascii_start + i] = 0x20 <= inbuf[inbase + i] && inbuf[inbase + i] <= 0x7e ? inbuf[inbase + i] : '.';
  }
  outbuf[ascii_start + r] = '\n';
}
