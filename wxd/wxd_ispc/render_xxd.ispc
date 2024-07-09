export void render_ascii(const uint8* const uniform inbuf, uniform const int inbuf_len, uint8* const uniform outbuf) {
  foreach (i = 0 ... inbuf_len) {
    uint8 b = inbuf[i];
    outbuf[i] = select(and(0x20 <= b, b <= 0x7e), b, (varying int8) 0x2e);
  }
}

export void render_hex(const uint8* const uniform inbuf, uniform const int inbuf_len, const int* const uniform n2o, const int* const uniform o2n, uint8* const uniform outbuf) {
  uniform int out_off = 0;
  for (uniform int base = 0; base < inbuf_len; base += programCount) {
    uniform int len = min(programCount, inbuf_len - base);
    uint8 byte = 0x61;
    cif (base + programIndex < inbuf_len) byte = inbuf[base + programIndex];
    int8 nib_hi = byte >> 4;
    nib_hi = select(nib_hi < 10, nib_hi + 0x30, nib_hi - 10 + 0x61);
    int8 nib_lo = byte & 0xF;
    nib_lo = select(nib_lo < 10, nib_lo + 0x30, nib_lo - 10 + 0x61);
    int nib_base = select((programIndex & 1) == 1, programCount, (varying int) 0);
    int8 nib_0 = shuffle(nib_hi, nib_lo, (programIndex >> 1) + nib_base);
    int8 nib_1 = shuffle(nib_hi, nib_lo, ((programCount + programIndex) >> 1) + nib_base);
    uniform int out_end = n2o[(base + len - 1) * 2] + 2;
    int out_0_idx = o2n[out_off +                    programIndex];
    int out_1_idx = o2n[out_off +     programCount + programIndex];
    int out_2_idx = o2n[out_off + 2 * programCount + programIndex];
    uint8 out_0 = select(out_0_idx == -1, (varying int8) 0x20, shuffle(nib_0, nib_1, out_0_idx - base * 2));
    uint8 out_1 = select(out_1_idx == -1, (varying int8) 0x20, shuffle(nib_0, nib_1, out_1_idx - base * 2));
    uint8 out_2 = select(out_2_idx == -1, (varying int8) 0x20, shuffle(nib_0, nib_1, out_2_idx - base * 2));
    cif (out_off + programIndex < out_end) outbuf[out_off + programIndex] = out_0;
    cif (out_off + programCount + programIndex < out_end) outbuf[out_off + programCount + programIndex] = out_1;
    if (out_off + 2 * programCount + programIndex < out_end) outbuf[out_off + 2 * programCount + programIndex] = out_2;
    out_off = out_end;
  }
}
