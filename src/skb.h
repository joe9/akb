
#include "Skb_stub.h"

extern void xkb_context_new(void) __attribute__((constructor));
extern void xkb_context_unref(void) __attribute__((destructor));
