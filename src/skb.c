
#include <HsFFI.h>
extern void __stginit_Skb(void);

static void xkb_context_new(void) __attribute__((constructor));
static void xkb_context_new(void)
{
   static char *argv[] = { "libSkb.so", 0 }, **argv_ = argv;
   static int argc = 1;
   hs_init(&argc, &argv_);
   hs_add_root(__stginit_Skb);
}

static void xkb_context_unref(void) __attribute__((destructor));
static void xkb_context_unref(void)
{
   hs_exit();
}
