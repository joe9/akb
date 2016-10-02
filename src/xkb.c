
#include <HsFFI.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libxkbcommon/xkbcommon/xkbcommon.h"
#include "skb.h"

/* https://www.vex.net/~trebla/haskell/so.xhtml */
/* http://stackoverflow.com/questions/27815467/haskell-dynamic-library */
/* http://stackoverflow.com/questions/5131182/how-to-compile-haskell-to-a-static-library */
/* https://mostlycode.wordpress.com/2010/01/03/shared-haskell-so-library-with-ghc-6-10-4-and-cabal/ */

/* could not figure out how to get this to do the below with
 * cabal, hence using make as used here https://github.com/Tuplanolla/ld-prehaskell */

struct xkb_context {
   enum xkb_log_level log_level;
   int log_verbosity;
};


/* below from libxkbcommon/src/utils.h */
/* Compiler Attributes */

#if defined(__GNUC__) && (__GNUC__ >= 4) && !defined(__CYGWIN__)
# define XKB_EXPORT      __attribute__((visibility("default")))
#elif defined(__SUNPRO_C) && (__SUNPRO_C >= 0x550)
# define XKB_EXPORT      __global
#else /* not gcc >= 4 and not Sun Studio >= 8 */
# define XKB_EXPORT
#endif

/**
 * Create a new context.
 */
XKB_EXPORT struct xkb_context *
xkb_context_new(enum xkb_context_flags flags)
{
   struct xkb_context *ctx = calloc(1, sizeof(*ctx));
   ctx->log_level = XKB_LOG_LEVEL_ERROR;
   ctx->log_verbosity = 0;

   skb_context_new();
   /*    might add this if needed */
   /* Environment overwrites defaults. */
   /*    env = secure_getenv("XKB_LOG_LEVEL"); */
   /*    if (env) */
   /*       xkb_context_set_log_level(ctx, log_level(env)); */

   /*    env = secure_getenv("XKB_LOG_VERBOSITY"); */
   /*    if (env) */
   /*       xkb_context_set_log_verbosity(ctx, log_verbosity(env)); */
   return ctx;
}

/**
 * Drop an existing reference on the context, and free it if the refcnt is
 * now 0.
 */
XKB_EXPORT void
xkb_context_unref(struct xkb_context *ctx)
{
   if (!ctx) return;
   skb_context_unref();
   free(ctx);
}

/* Common keyboard description structure */
struct xkb_keymap {
   /* 1 is mapped to customDvorak */
   int initial_state_index;
};
XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_names(struct xkb_context *ctx,
			  const struct xkb_rule_names *rmlvo_in,
			  enum xkb_keymap_compile_flags flags)
{
   struct xkb_keymap *keymap = calloc(1, sizeof(*keymap));
   keymap->initial_state_index = 1;
   return keymap;
}
XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_buffer(struct xkb_context *ctx,
			   const char *buffer, size_t length,
			   enum xkb_keymap_format format,
			   enum xkb_keymap_compile_flags flags)
{
   struct xkb_keymap *keymap = calloc(1, sizeof(*keymap));
   keymap->initial_state_index = 1;
   return keymap;
}
XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_file(struct xkb_context *ctx,
			 FILE *file,
			 enum xkb_keymap_format format,
			 enum xkb_keymap_compile_flags flags)
{
   struct xkb_keymap *keymap = calloc(1, sizeof(*keymap));
   keymap->initial_state_index = 1;
   return keymap;
}
XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_string(struct xkb_context *ctx,
			   const char *string,
			   enum xkb_keymap_format format,
			   enum xkb_keymap_compile_flags flags)
{
   return xkb_keymap_new_from_buffer(ctx, string, strlen(string),
				     format, flags);
}
XKB_EXPORT void
xkb_keymap_unref(struct xkb_keymap *keymap)
{
   if (!keymap) return;
   free(keymap);
}

struct xkb_state {
   void *hs_stable_state_ptr;  /* C representation of a StablePtr */
   /*
     /*    TODO */
   /*
    * We mustn't clear a base modifier if there's another depressed key
    * which affects it, e.g. given this sequence
    * < Left Shift down, Right Shift down, Left Shift Up >
    * the modifier should still be set. This keeps the count.
    */
   /*    int16_t mod_key_count[XKB_MAX_MODS]; */
};

XKB_EXPORT struct xkb_state *
xkb_state_new(struct xkb_keymap *keymap)
{
   struct xkb_state *state = calloc(1, sizeof(*state));
   state->hs_stable_state_ptr = skb_state_new(keymap->initial_state_index);
   return state;
}

XKB_EXPORT void
xkb_state_unref(struct xkb_state *state)
{
   if (!state) return;
   skb_state_unref(state->hs_stable_state_ptr);
   free(state);
}
