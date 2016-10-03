
#include <stdio.h>
#include <xkbcommon/xkbcommon.h>

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
  struct xkb_context *ctx = xkb_context_new (XKB_CONTEXT_NO_FLAGS);
  struct xkb_keymap *keymap = xkb_keymap_new_from_buffer (ctx, NULL, 0, 0, 0);
  struct xkb_state *state = xkb_state_new (keymap);

  int keysym = xkb_state_key_get_one_sym (state, 10);
  printf ("keysym received: %i\n", keysym);

  keysym = xkb_state_key_get_one_sym (state, 20);
  printf ("keysym received: %i\n", keysym);

  keysym = xkb_state_key_get_one_sym (state, 32);
  printf ("keysym received: %i\n", keysym);

  xkb_state_update_mask (state, 1, 0, 0, 0, 0, 0);
  keysym = xkb_state_key_get_one_sym (state, 32);
  printf ("keysym received: %i\n", keysym);

  keysym = xkb_state_key_get_one_sym (state, 32);
  printf ("keysym received: %i\n", keysym);

  uint32_t utfValue = xkb_state_key_get_utf32 (state, 32);
  printf ("utf value received: %i\n", utfValue);

  /*    cleanup */
  xkb_state_unref (state);
  xkb_keymap_unref (keymap);
  xkb_context_unref (ctx);
  return 0;
}
