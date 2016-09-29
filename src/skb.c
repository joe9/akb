
#include <HsFFI.h>

/* 2 types of build methods: */
/* 1. using gcc: */
/*     TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c -lHSrts-ghc8.0.1 */
/*     gcc -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I. -L. -lSkb test-libSkb.c */

/* 2. using ghc */
/*       TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c */
/*       TMPDIR=/tmp/ghc stack exec ghc -- --make -no-hs-main -optc-O2 test-libSkb.c skb.o -L. -I. -lSkb && ./a.out */


extern void __stginit_Skb(void);

void xkb_context_new(void)
{
   static char *argv[] = { "libSkb.so", 0 }, **argv_ = argv;
   static int argc = 1;
   hs_init(&argc, &argv_);
   hs_add_root(__stginit_Skb);
}

void xkb_context_unref(void)
{
   hs_exit();
}
