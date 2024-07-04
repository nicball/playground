export void render_fixed32_ascii(const uint8* const uniform inbuf, uint8* const uniform outbuf) {
  foreach (i = 0 ... 32) {
    const int8 b = inbuf[i];
    outbuf[i] = select(and(0x20 <= b, b <= 0x7e), b, (varying int8) 0x2e);
  }
}

export void render_fixed32_hex(const uint8* const uniform inbuf, uint8* const uniform outbuf) {
  foreach (j = 0 ... 64) {
    uint8 h = ((varying int8)j) & 1;
    uint8 b = shuffle((int8)inbuf[j], programIndex >> 1);
    int8 nib = (b >> ((1 - h) * 4)) & 0xF;
    bool c = nib < 10;
    outbuf[j] = select(c, nib + 0x30, nib - 10 + 0x61);
  }
}
