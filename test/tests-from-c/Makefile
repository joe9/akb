
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

all: test-akb read-out-loop fscanf-out-loop

skb: test-libskb
	./test-libskb

run: all
	./test-akb
	./read-out-loop
	./fscanf-out-loop

read-out-loop: read-out-loop.c
	$(CC) $< -o $@

fscanf-out-loop: fscanf-out-loop.c
	$(CC) $< -o $@

test-akb: test-akb.c
	$(CC) $< -o $@

clean:
	$(RM) test-akb read-out-loop fscanf-out-loop

.PHONY: all clean run
