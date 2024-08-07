static inline const char* clGetErrorString(cl_int err) {
  switch (err) {
  case -1: return "device not found";
  case -2: return "device not available";
  case -3: return "compiler not available";
  case -4: return "mem object allocation failure";
  case -5: return "out of resources";
  case -6: return "out of host memory";
  case -7: return "profiling info not available";
  case -8: return "mem copy overlap";
  case -9: return "image format mismatch";
  case -10: return "image format not supported";
  case -11: return "build program failure";
  case -12: return "map failure";
  case -13: return "misaligned sub buffer offset";
  case -14: return "exec status error for events in wait list";
  case -15: return "compile program failure";
  case -16: return "linker not available";
  case -17: return "link program failure";
  case -18: return "device partition failed";
  case -19: return "kernel arg info not available";
  case -30: return "invalid value";
  case -31: return "invalid device type";
  case -32: return "invalid platform";
  case -33: return "invalid device";
  case -34: return "invalid context";
  case -35: return "invalid queue properties";
  case -36: return "invalid command queue";
  case -37: return "invalid host ptr";
  case -38: return "invalid mem object";
  case -39: return "invalid image format descriptor";
  case -40: return "invalid image size";
  case -41: return "invalid sampler";
  case -42: return "invalid binary";
  case -43: return "invalid build options";
  case -44: return "invalid program";
  case -45: return "invalid program executable";
  case -46: return "invalid kernel name";
  case -47: return "invalid kernel definition";
  case -48: return "invalid kernel";
  case -49: return "invalid arg index";
  case -50: return "invalid arg value";
  case -51: return "invalid arg size";
  case -52: return "invalid kernel args";
  case -53: return "invalid work dimension";
  case -54: return "invalid work group size";
  case -55: return "invalid work item size";
  case -56: return "invalid global offset";
  case -57: return "invalid event wait list";
  case -58: return "invalid event";
  case -59: return "invalid operation";
  case -60: return "invalid gl object";
  case -61: return "invalid buffer size";
  case -62: return "invalid mip level";
  case -63: return "invalid global work size";
  case -64: return "invalid property";
  case -65: return "invalid image descriptor";
  case -66: return "invalid compiler options";
  case -67: return "invalid linker options";
  case -68: return "invalid device partition count";
  case -69: return "invalid pipe size";
  case -70: return "invalid device queue";
  case -71: return "invalid spec id";
  case -72: return "max size restriction exceeded";
  default: return "unknown error";
  }
}
