export void render_fixed32_ascii(const uint8* const uniform inbuf, uint8* const uniform outbuf) {
  uniform const int nloop = 32 / programCount;
  for (uniform int i = 0; i < nloop; ++i) {
    const int8 b = inbuf[i * programCount + programIndex];
    // streaming_store(&outbuf[i * programCount], select(and(0x20 <= b, b <= 0x7e), b, (varying int8) 0x2e));
    outbuf[i * programCount + programIndex] = select(and(0x20 <= b, b <= 0x7e), b, (varying int8) 0x2e);
  }
}

export void render_fixed32_hex(const uint8* const uniform inbuf, uint8* const uniform outbuf) {
  int8 chunk;
  uniform const int nloop = 64 / programCount;
  for (uniform int i = 0; i < nloop; ++i) {
    uniform int const first_half = 1 - (i & 1);
    if (first_half) {
      chunk = inbuf[i * programCount + programIndex];
    }
    uint8 b;
    if (first_half) {
      b = shuffle(chunk, programIndex >> 1);
    }
    else {
      b = shuffle(chunk, (programIndex >> 1) + (programCount >> 1));
    }
    const int8 nib = (b >> ((1 - (programIndex & 1)) * 4)) & 0xF;
    const bool c = nib < 10;
    // streaming_store(&outbuf[i * programCount], select(c, nib + 0x30, nib - 10 + 0x61));
    outbuf[i * programCount + programIndex] = select(c, nib + 0x30, nib - 10 + 0x61);
  }
}
