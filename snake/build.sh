#!/bin/sh
x86_64-w64-mingw32-gcc -I/home/zsy/tmp/ncurses-precompiled/include/ncursesw -I/home/zsy/tmp/ncurses-precompiled/include -L/home/zsy/tmp/ncurses-precompiled/lib main.c -lncursesw -static
cp a.exe ~/vboxsf/
