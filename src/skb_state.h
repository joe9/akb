
#ifndef _SKB_H_
#define _SKB_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/**
 * @struct xkb_state
 * Opaque keyboard state object.
 *
 * State objects contain the active state of a keyboard (or keyboards), such
 * as the currently effective layout and the active modifiers.  It acts as a
 * simple state machine, wherein key presses and releases are the input, and
 * key symbols (keysyms) are the output.
 */
struct xkb_state ;

struct state_components {
   /* These may be negative, because of -1 group actions. */
   int32_t base_group; /**< depressed */
   int32_t latched_group;
   int32_t locked_group;
   uint32_t group; /**< effective */

   uint32_t  base_mods; /**< depressed */
   uint32_t latched_mods;
   uint32_t locked_mods;
   uint32_t mods; /**< effective */

   uint32_t leds;
};

#endif /* _SKB_H_
