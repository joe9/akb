#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsStablePtr skb_state_new(HsInt32 a1);
extern HsWord32 skb_state_key_get_one_sym(HsStablePtr a1, HsWord32 a2);
extern void skb_state_unref(HsStablePtr a1);
extern HsWord32 skb_state_update_key(HsStablePtr a1, HsWord32 a2, HsWord32 a3);
extern HsWord32 skb_state_update_mask(HsStablePtr a1, HsWord32 a2, HsWord32 a3, HsWord32 a4, HsWord32 a5, HsWord32 a6, HsWord32 a7);
extern HsWord32 skb_keymap_key_repeats(HsWord32 a1, HsWord32 a2);
extern HsWord32 skb_state_serialize_state_component(HsStablePtr a1, HsWord32 a2);
extern HsWord32 skb_state_key_get_utf(HsStablePtr a1, HsWord32 a2);
extern HsWord32 skb_keymap_mod_get_index(HsWord32 a1, HsPtr a2);
extern HsWord32 skb_keymap_led_get_index(HsWord32 a1, HsPtr a2);
#ifdef __cplusplus
}
#endif

