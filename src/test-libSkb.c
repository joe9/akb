
#include <stdio.h>
#include "skb.h"

/* build command: */
/* TMPDIR=/tmp/ghc stack exec ghc -- --make -no-hs-main -optc-O2 test-libSkb.c -L. -I. -lSkb && ./a.out */
/* or using gcc instead of ghc */
/* gcc -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I. -L. -lSkb test-libSkb.c */


int main(int argc, char *argv[])
{
   int i;
   xkb_context_new();

   i = fibonacci_hs(42);
   printf("Fibonacci: %d\n", i);

   xkb_context_unref();
   return 0;
}
