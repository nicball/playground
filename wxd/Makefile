target/wxd: wxd/wxd.c
	gcc wxd/wxd.c -o target/wxd -O3 -g -Wall -pedantic

target/wxd_fixed: wxd_ispc/wxd_fixed.c wxd_ispc/render_fixed.ispc
	ispc -O2 wxd_ispc/render_fixed.ispc --target avx2-i8x32 -o target/render_fixed.o -h target/render_fixed.h -g
	gcc wxd_ispc/wxd_fixed.c target/render_fixed.o -o target/wxd_fixed -O2 -g -Wall -pedantic -Itarget

target/wxd_ispc: wxd_ispc/wxd_ispc.c wxd_ispc/render_xxd.ispc
	ispc -O2 wxd_ispc/render_xxd.ispc --target avx2-i32x8 -o target/render_xxd.o -h target/render_xxd.h -g
	gcc wxd_ispc/wxd_ispc.c target/render_xxd.o -o target/wxd_ispc -O2 -g -Wall -pedantic -Itarget

target/wxd_mmap: bad/wxd_mmap.c
	gcc bad/wxd_mmap.c -o target/wxd_mmap -O3 -g -Wall -pedantic

target/wxd_intrin: wxd_intrin/wxd_intrin.cc wxd_intrin/render.cc wxd_intrin/render.h wxd_intrin/channel.h
	clang++ --std=c++20 -O2 -march=znver3 wxd_intrin/wxd_intrin.cc wxd_intrin/render.cc -Iwxd_intrin -o target/wxd_intrin -g -Wall -pedantic

