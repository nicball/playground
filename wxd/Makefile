target/wxd: wxd.c
	gcc wxd.c -o target/wxd -O3 -g -Wall -pedantic

target/wxd_fixed: wxd_fixed.c render_fixed.h
	ispc -O3 render_fixed.ispc --target avx2-i8x32 -o target/render_fixed.o
	gcc wxd_fixed.c target/render_fixed.o -o target/wxd_fixed -O3 -g -Wall -pedantic

target/wxd_ispc: wxd_ispc.c render_xxd.h
	ispc -O3 render_xxd.ispc --target avx2-i32x8 -o target/render_xxd.o
	gcc wxd_ispc.c target/render_xxd.o -o target/wxd_ispc -O3 -g -Wall -pedantic

target/wxd_mmap: wxd_mmap.c
	gcc wxd_mmap.c -o target/wxd_mmap -O3 -g -Wall -pedantic

render_xxd.h: render_xxd.ispc
	ispc render_xxd.ispc -h render_xxd.h

render_fixed.h: render_fixed.ispc
	ispc render_fixed.ispc -h render_fixed.h
