
# got this idea from
# https://github.com/Tuplanolla/ld-prehaskell/blob/master/makefile
# https://www.cs.swarthmore.edu/~newhall/unixhelp/howto_makefiles.html

CC=gcc
TMPDIR=/tmp/ghc/
WLD=/home/j/dev/apps/wayland/wayland-install

# the compiler: gcc for C program, define as g++ for C++
CC = gcc

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
SKBCFLAGS  = -g -Wall -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I$(WLD)/include -L$(WLD)/lib -lskb

XKBCFLAGS  = -g -Wall -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I$(WLD)/include -L$(WLD)/lib -lskb-xkbcommon

all: test-libskb test-libxkb

skb: test-libskb
	LD_LIBRARY_PATH=$(WLD)/lib ./test-libskb

xkb: test-libxkb
	LD_LIBRARY_PATH=$(WLD)/lib ./test-libxkb

run: all
	LD_LIBRARY_PATH=$(WLD)/lib ./test-libskb
	LD_LIBRARY_PATH=$(WLD)/lib ./test-libxkb

test-libxkb: test-libxkb.c
	$(CC) $(XKBCFLAGS) $< -o $@

test-libskb: test-libskb.c
	$(CC) $(SKBCFLAGS) $< -o $@

clean:
	$(RM) test-libskb test-libxkb

.PHONY: all clean run
