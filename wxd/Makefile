target/wxd: wxd.c
	gcc wxd.c -o target/wxd -O3 -g -Wall -pedantic

target/wxd_ispc: wxd_ispc.c render_xxd.ispc
	ispc -O3 render_xxd.ispc --target avx2-i32x8 -o target/render_xxd.o
	gcc wxd_ispc.c target/render_xxd.o -o target/wxd_ispc -O3 -g -Wall -pedantic

target/wxd_mmap: wxd_mmap.c
	gcc wxd_mmap.c -o target/wxd_mmap -O3 -g -Wall -pedantic
