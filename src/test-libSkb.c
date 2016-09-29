
#include <stdio.h>
#include "skb.h"

/* 2 types of build methods: */
/* 1. using gcc: */
/*     TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c -lHSrts-ghc8.0.1 */
/*     gcc -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I. -L. -lSkb test-libSkb.c */

/* 2. using ghc */
/*       TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c */
/*       TMPDIR=/tmp/ghc stack exec ghc -- --make -no-hs-main -optc-O2 test-libSkb.c skb.o -L. -I. -lSkb && ./a.out */


int main(int argc, char *argv[])
{
   int i;
   xkb_context_new();

   i = fibonacci_hs(42);
   printf("Fibonacci: %d\n", i);

   xkb_context_unref();
   return 0;
}
