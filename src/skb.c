
#include <HsFFI.h>

/* https://www.vex.net/~trebla/haskell/so.xhtml */
/* http://stackoverflow.com/questions/27815467/haskell-dynamic-library */
/* http://stackoverflow.com/questions/5131182/how-to-compile-haskell-to-a-static-library */
/* https://mostlycode.wordpress.com/2010/01/03/shared-haskell-so-library-with-ghc-6-10-4-and-cabal/ */

/* could not figure out how to get this to do the below with
 * cabal, hence using make as used here https://github.com/Tuplanolla/ld-prehaskell */

/* 2 types of build methods: */
/* 1. using gcc: */
/*     TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c -lHSrts-ghc8.0.1 */
/*     gcc -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I. -L. -lSkb test-libSkb.c */

/* 2. using ghc */
/*       TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c */
/*       TMPDIR=/tmp/ghc stack exec ghc -- --make -no-hs-main -optc-O2 test-libSkb.c skb.o -L. -I. -lSkb && ./a.out */


extern void __stginit_Skb(void);

/* skb would be happy if this function had a definition of
   void xkb_context_new (void) but that would break compatibility.
   Hence leaving it as-is. */
void
skb_context_new(void)
{
   static char *argv[] = { "libskb.so", 0 }, **argv_ = argv;
   static int argc = 1;
   hs_init(&argc, &argv_);
   hs_add_root(__stginit_Skb);
}

void skb_context_unref(void)
{
   hs_exit();
}
