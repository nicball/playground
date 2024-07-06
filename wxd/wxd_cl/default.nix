with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "wxd_cl-0.1.0";
  src = ./.;
  nativeBuildInputs = [ opencl-headers clang spirv-llvm-translator ];
  buildInputs = [ ocl-icd ];
  installPhase = ''
    clang -target spir64 -O0 -emit-llvm -c render_kernel.cl -o render_kernel.bc
    llvm-spirv render_kernel.bc -o render_kernel.spv
    objcopy -I binary -O elf64-x86-64 --rename-section .data=.rodata,alloc,load,readonly render_kernel.spv render_kernel.o
    gcc render_kernel.o wxd_cl.c -o $out -lOpenCL -g
  '';
}
