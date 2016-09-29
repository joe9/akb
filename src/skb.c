
#include <HsFFI.h>

/* build command */
/* TMPDIR=/tmp/ghc stack exec ghc -- -O2 -dynamic -shared -fPIC -o libSkb.so Skb.hs skb.c */

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
