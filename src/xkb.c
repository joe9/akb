
#include <HsFFI.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "libxkbcommon/xkbcommon/xkbcommon.h"
#include "skb.h"

/* https://www.vex.net/~trebla/haskell/so.xhtml */
/* http://stackoverflow.com/questions/27815467/haskell-dynamic-library */
/* http://stackoverflow.com/questions/5131182/how-to-compile-haskell-to-a-static-library */
/* https://mostlycode.wordpress.com/2010/01/03/shared-haskell-so-library-with-ghc-6-10-4-and-cabal/ */
/* https://www.haskell.org/onlinereport/haskell2010/haskellch8.html */

/* could not figure out how to get this to do the below with
 * cabal, hence using make as used here https://github.com/Tuplanolla/ld-prehaskell */

struct xkb_context
{
   enum xkb_log_level log_level;
   int log_verbosity;
};


/* below from libxkbcommon/src/utils.h */
/* Compiler Attributes */

#if defined(__GNUC__) && (__GNUC__ >= 4) && !defined(__CYGWIN__)
#define XKB_EXPORT      __attribute__((visibility("default")))
#elif defined(__SUNPRO_C) && (__SUNPRO_C >= 0x550)
#define XKB_EXPORT      __global
#else /* not gcc >= 4 and not Sun Studio >= 8 */
#define XKB_EXPORT
#endif

/**
 * Create a new context.
 */
XKB_EXPORT struct xkb_context *
xkb_context_new (enum xkb_context_flags flags)
{
   /*    printf("xkb_context_new entered\n"); */
   struct xkb_context *ctx = calloc (1, sizeof (*ctx));
   ctx->log_level = XKB_LOG_LEVEL_ERROR;
   ctx->log_verbosity = 0;

   skb_context_new ();
   /*    might add this if needed */
   /* Environment overwrites defaults. */
   /*    env = secure_getenv("XKB_LOG_LEVEL"); */
   /*    if (env) */
   /*       xkb_context_set_log_level(ctx, log_level(env)); */

   /*    env = secure_getenv("XKB_LOG_VERBOSITY"); */
   /*    if (env) */
   /*       xkb_context_set_log_verbosity(ctx, log_verbosity(env)); */
   /*    printf("xkb_context_new exited\n"); */
   return ctx;
}

/**
 * Drop an existing reference on the context, and free it if the refcnt is
 * now 0.
 */
XKB_EXPORT void
xkb_context_unref (struct xkb_context *ctx)
{
   /*    printf("xkb_context_unref\n"); */
   if (!ctx)
      return;
   free (ctx);
   skb_context_unref ();
}

/* Common keyboard description structure */
struct xkb_keymap
{
   /* 1 is mapped to customDvorak */
   uint32_t initial_state_index;
};
XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_names (struct xkb_context *ctx,
			   const struct xkb_rule_names *rmlvo_in,
			   enum xkb_keymap_compile_flags flags)
{
   /*    printf("xkb_keymap_new_from_names entered\n"); */
   struct xkb_keymap *keymap = malloc (sizeof *keymap);
   *keymap = (struct xkb_keymap)
	 {
	    .initial_state_index = 1};
   /*    printf("xkb_keymap_new_from_names exited\n"); */
   return keymap;
}

XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_buffer (struct xkb_context *ctx,
			    const char *buffer, size_t length,
			    enum xkb_keymap_format format,
			    enum xkb_keymap_compile_flags flags)
{
   /*    printf("xkb_keymap_new_from_buffer entered\n"); */
   struct xkb_keymap *keymap = malloc (sizeof *keymap);
   *keymap = (struct xkb_keymap)
	 {
	    .initial_state_index = 1};
   /*    printf("xkb_keymap_new_from_buffer exited\n"); */
   return keymap;
}

XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_file (struct xkb_context *ctx,
			  FILE * file,
			  enum xkb_keymap_format format,
			  enum xkb_keymap_compile_flags flags)
{
   /*    printf("xkb_keymap_new_from_file entered\n"); */
   struct xkb_keymap *keymap = malloc (sizeof *keymap);
   *keymap = (struct xkb_keymap)
	 {
	    .initial_state_index = 1};
   /*    printf("xkb_keymap_new_from_file exited\n"); */
   return keymap;
}

XKB_EXPORT struct xkb_keymap *
xkb_keymap_new_from_string (struct xkb_context *ctx,
			    const char *string,
			    enum xkb_keymap_format format,
			    enum xkb_keymap_compile_flags flags)
{
   /*    printf("xkb_keymap_new_from_string\n"); */
   return xkb_keymap_new_from_buffer (ctx, string, strlen (string),
				      format, flags);
}

XKB_EXPORT void
xkb_keymap_unref (struct xkb_keymap *keymap)
{
   /*    printf("xkb_keymap_unref\n"); */
   if (!keymap)
      return;
   free (keymap);
}

struct xkb_state
{
   void *hs_stable_state_ptr;	/* C representation of a StablePtr */
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
xkb_state_new (struct xkb_keymap *keymap)
{
   /*    printf("xkb_state_new entered\n"); */
   /* use this as It's an assignment on the entire struct, not just one field. */
   struct xkb_state *state = malloc (sizeof *state);
   *state = (struct xkb_state)
	 {
	    .hs_stable_state_ptr = skb_state_new (keymap->initial_state_index)};
   /*    printf("xkb_state_new exited\n"); */
   return state;
}

XKB_EXPORT void
xkb_state_unref (struct xkb_state *state)
{
   /*    printf("xkb_state_unref entered\n"); */
   if (!state)
      return;
   skb_state_unref (state->hs_stable_state_ptr);
   free (state);
}

xkb_keysym_t
xkb_state_key_get_one_sym (struct xkb_state * state, xkb_keycode_t key)
{
   /*    printf("xkb_state_key_get_one_sym\n"); */
   return skb_state_key_get_one_sym (state->hs_stable_state_ptr, key);
}

enum xkb_state_component
xkb_state_update_mask (struct xkb_state *state,
		       xkb_mod_mask_t depressed_mods,
		       xkb_mod_mask_t latched_mods,
		       xkb_mod_mask_t locked_mods,
		       xkb_layout_index_t depressed_layout,
		       xkb_layout_index_t latched_layout,
		       xkb_layout_index_t locked_layout)
{
   /*    printf("xkb_state_update_mask\n"); */
   return skb_state_update_mask (state->hs_stable_state_ptr,
				 depressed_mods,
				 latched_mods,
				 locked_mods,
				 depressed_layout,
				 latched_layout, locked_layout);
}

enum xkb_state_component
xkb_state_update_key (struct xkb_state *state, xkb_keycode_t key,
		      enum xkb_key_direction direction)
{
   /*    printf("xkb_state_update_key\n"); */
   return skb_state_update_key (state->hs_stable_state_ptr, key, direction);
}

xkb_mod_mask_t
xkb_state_serialize_mods (struct xkb_state * state,
			  enum xkb_state_component components)
{
   /*       printf("xkb_state_serialize_mods\n"); */
   return skb_state_serialize_state_component (state->hs_stable_state_ptr,
					       components);
}

xkb_mod_mask_t
xkb_state_serialize_layout (struct xkb_state * state,
			    enum xkb_state_component components)
{
   /*       printf("xkb_state_serialize_layout\n"); */
   return skb_state_serialize_state_component (state->hs_stable_state_ptr,
					       components);
}

uint32_t
xkb_state_key_get_utf32 (struct xkb_state * state, xkb_keycode_t key)
{
/*       printf("xkb_state_key_get_utf32\n"); */
   return skb_state_key_get_utf (state->hs_stable_state_ptr, key);
}

int
xkb_keymap_key_repeats (struct xkb_keymap *keymap, xkb_keycode_t key)
{
   /*       printf("xkb_keymap_key_repeats\n"); */
   return skb_keymap_key_repeats (keymap->initial_state_index, key);
}

xkb_mod_index_t
xkb_keymap_mod_get_index(struct xkb_keymap *keymap, const char *name)
{
   /*       printf("xkb_keymap_mod_get_index\n"); */
   /* http://stackoverflow.com/a/16775684 */
   /* to avoid the warning/error due to the const qualifier above */
   return skb_keymap_mod_get_index(keymap->initial_state_index, (char *)name);
}

xkb_led_index_t
xkb_keymap_led_get_index(struct xkb_keymap *keymap, const char *name){
   /*       printf("xkb_keymap_led_get_index\n"); */
   /* http://stackoverflow.com/a/16775684 */
   /* to avoid the warning/error due to the const qualifier above */
   return skb_keymap_led_get_index(keymap->initial_state_index, (char *)name);
}

/* TODO: add led functionality, keep them off for now */
int
xkb_state_led_index_is_active(struct xkb_state *state, xkb_led_index_t idx){
   /*       printf("xkb_keymap_led_index_is_active\n"); */
   return 0;
}

char *
xkb_keymap_get_as_string(struct xkb_keymap *keymap,
			 enum xkb_keymap_format format){
   /*  printf("xkb_keymap_get_as_string entered\n"); */
   /*    from http://stackoverflow.com/a/1775487 */
   size_t needed = snprintf(NULL, 0, "%i", keymap->initial_state_index);
   char  *buffer = malloc(needed+1);
   sprintf(buffer, "%i", keymap->initial_state_index);
   /*    printf("xkb_keymap_get_as_string exited\n"); */
   return buffer;
}
