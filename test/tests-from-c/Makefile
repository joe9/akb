
# got this idea from
# https://github.com/Tuplanolla/ld-prehaskell/blob/master/makefile
# https://www.cs.swarthmore.edu/~newhall/unixhelp/howto_makefiles.html

CC=gcc

# the compiler: gcc for C program, define as g++ for C++
CC = gcc

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
CFLAGS  = -g -Wall

all: test-akb

skb: test-libskb
	./test-libskb

run: all
	./test-akb

test-akb: test-akb.c
	$(CC) $< -o $@

clean:
	$(RM) test-akb

.PHONY: all clean run
