
#include <stdio.h>
#include <skb.h>

/* https://www.vex.net/~trebla/haskell/so.xhtml */
/* http://stackoverflow.com/questions/27815467/haskell-dynamic-library */
/* http://stackoverflow.com/questions/5131182/how-to-compile-haskell-to-a-static-library */
/* https://mostlycode.wordpress.com/2010/01/03/shared-haskell-so-library-with-ghc-6-10-4-and-cabal/ */

/* 2 types of build methods: */
/* 1. using gcc: */
/*     TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c -lHSrts-ghc8.0.1 */
/*     gcc -I/home/j/var/stack/programs/x86_64-linux/ghc-8.0.1/lib/ghc-8.0.1/include/ -I. -L. -lSkb test-libSkb.c */

/* 2. using ghc */
/*       TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c */
/*       TMPDIR=/tmp/ghc stack exec ghc -- --make -no-hs-main -optc-O2 test-libSkb.c skb.o -L. -I. -lSkb && ./a.out */


int
main (int argc, char *argv[])
{
  skb_context_new ();

  int *statePtr;

  /*    1 is mapped to customDvorak */
  statePtr = skb_state_new (1);
  int keysym = skb_state_key_get_one_sym (statePtr, 10);
  printf ("keysym received: %i\n", keysym);

  keysym = skb_state_key_get_one_sym (statePtr, 20);
  printf ("keysym received: %i\n", keysym);

  keysym = skb_state_key_get_one_sym (statePtr, 32);
  printf ("keysym received: %i\n", keysym);

  skb_state_update_mask (statePtr, 1, 0, 0, 0, 0, 0);
  keysym = skb_state_key_get_one_sym (statePtr, 32);
  printf ("keysym received: %i\n", keysym);

  skb_state_unref (statePtr);

  skb_context_unref ();
  return 0;
}
