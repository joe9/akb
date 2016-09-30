{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "libxkbcommon/xkbcommon/xkbcommon.h"
module Bindings.Xkbcommon where
import Foreign.Ptr
#strict_import

{- struct xkb_context; -}
--   replacing struct xkb_context with an uint32_t
#opaque_t struct xkb_context
{- struct xkb_keymap; -}
#opaque_t struct xkb_keymap
{- struct xkb_state; -}
#opaque_t struct xkb_state
{- typedef uint32_t xkb_keycode_t; -}
#synonym_t xkb_keycode_t , CUInt
{- typedef uint32_t xkb_keysym_t; -}
#synonym_t xkb_keysym_t , CUInt
{- typedef uint32_t xkb_layout_index_t; -}
#synonym_t xkb_layout_index_t , CUInt
{- typedef uint32_t xkb_layout_mask_t; -}
#synonym_t xkb_layout_mask_t , CUInt
{- typedef uint32_t xkb_level_index_t; -}
#synonym_t xkb_level_index_t , CUInt
{- typedef uint32_t xkb_mod_index_t; -}
#synonym_t xkb_mod_index_t , CUInt
{- typedef uint32_t xkb_mod_mask_t; -}
#synonym_t xkb_mod_mask_t , CUInt
{- typedef uint32_t xkb_led_index_t; -}
#synonym_t xkb_led_index_t , CUInt
{- typedef uint32_t xkb_led_mask_t; -}
#synonym_t xkb_led_mask_t , CUInt
{- struct xkb_rule_names {
    const char * rules;
    const char * model;
    const char * layout;
    const char * variant;
    const char * options;
}; -}
#starttype struct xkb_rule_names
#field rules , CString
#field model , CString
#field layout , CString
#field variant , CString
#field options , CString
#stoptype
#ccall xkb_keysym_get_name , CUInt -> CString -> CSize -> IO CInt
{- enum xkb_keysym_flags {
    XKB_KEYSYM_NO_FLAGS = 0, XKB_KEYSYM_CASE_INSENSITIVE = 1 << 0
}; -}
#integral_t enum xkb_keysym_flags
#num XKB_KEYSYM_NO_FLAGS
#num XKB_KEYSYM_CASE_INSENSITIVE
#ccall xkb_keysym_from_name , CString -> <enum xkb_keysym_flags> -> IO CUInt
#ccall xkb_keysym_to_utf8 , CUInt -> CString -> CSize -> IO CInt
#ccall xkb_keysym_to_utf32 , CUInt -> IO CUInt
{- enum xkb_context_flags {
    XKB_CONTEXT_NO_FLAGS = 0,
    XKB_CONTEXT_NO_DEFAULT_INCLUDES = 1 << 0,
    XKB_CONTEXT_NO_ENVIRONMENT_NAMES = 1 << 1
}; -}
#integral_t enum xkb_context_flags
#num XKB_CONTEXT_NO_FLAGS
#num XKB_CONTEXT_NO_DEFAULT_INCLUDES
#num XKB_CONTEXT_NO_ENVIRONMENT_NAMES
#ccall xkb_context_new , <enum xkb_context_flags> -> IO (Ptr <struct xkb_context>)
#ccall xkb_context_ref , Ptr <struct xkb_context> -> IO (Ptr <struct xkb_context>)
#ccall xkb_context_unref , Ptr <struct xkb_context> -> IO ()
#ccall xkb_context_set_user_data , Ptr <struct xkb_context> -> Ptr () -> IO ()
#ccall xkb_context_get_user_data , Ptr <struct xkb_context> -> IO (Ptr ())
#ccall xkb_context_include_path_append , Ptr <struct xkb_context> -> CString -> IO CInt
#ccall xkb_context_include_path_append_default , Ptr <struct xkb_context> -> IO CInt
#ccall xkb_context_include_path_reset_defaults , Ptr <struct xkb_context> -> IO CInt
#ccall xkb_context_include_path_clear , Ptr <struct xkb_context> -> IO ()
#ccall xkb_context_num_include_paths , Ptr <struct xkb_context> -> IO CUInt
#ccall xkb_context_include_path_get , Ptr <struct xkb_context> -> CUInt -> IO CString
{- enum xkb_log_level {
    XKB_LOG_LEVEL_CRITICAL = 10,
    XKB_LOG_LEVEL_ERROR = 20,
    XKB_LOG_LEVEL_WARNING = 30,
    XKB_LOG_LEVEL_INFO = 40,
    XKB_LOG_LEVEL_DEBUG = 50
}; -}
#integral_t enum xkb_log_level
#num XKB_LOG_LEVEL_CRITICAL
#num XKB_LOG_LEVEL_ERROR
#num XKB_LOG_LEVEL_WARNING
#num XKB_LOG_LEVEL_INFO
#num XKB_LOG_LEVEL_DEBUG
#ccall xkb_context_set_log_level , Ptr <struct xkb_context> -> <enum xkb_log_level> -> IO ()
#ccall xkb_context_get_log_level , Ptr <struct xkb_context> -> IO (<enum xkb_log_level>)
#ccall xkb_context_set_log_verbosity , Ptr <struct xkb_context> -> CInt -> IO ()
#ccall xkb_context_get_log_verbosity , Ptr <struct xkb_context> -> IO CInt
-- #ccall xkb_context_set_log_fn , Ptr <struct xkb_context> -> FunPtr (Ptr <struct xkb_context> -> <enum xkb_log_level> -> CString -> <__builtin_va_list> -> IO ()) -> IO ()
{- enum xkb_keymap_compile_flags {
    XKB_KEYMAP_COMPILE_NO_FLAGS = 0
}; -}
#integral_t enum xkb_keymap_compile_flags
#num XKB_KEYMAP_COMPILE_NO_FLAGS
#ccall xkb_keymap_new_from_names , Ptr <struct xkb_context> -> Ptr <struct xkb_rule_names> -> <enum xkb_keymap_compile_flags> -> IO (Ptr <struct xkb_keymap>)
{- enum xkb_keymap_format {
    XKB_KEYMAP_FORMAT_TEXT_V1 = 1
}; -}
#integral_t enum xkb_keymap_format
#num XKB_KEYMAP_FORMAT_TEXT_V1
-- #ccall xkb_keymap_new_from_file , Ptr <struct xkb_context> -> Ptr <struct _IO_FILE> -> <enum xkb_keymap_format> -> <enum xkb_keymap_compile_flags> -> IO (Ptr <struct xkb_keymap>)
#ccall xkb_keymap_new_from_string , Ptr <struct xkb_context> -> CString -> <enum xkb_keymap_format> -> <enum xkb_keymap_compile_flags> -> IO (Ptr <struct xkb_keymap>)
#ccall xkb_keymap_new_from_buffer , Ptr <struct xkb_context> -> CString -> CSize -> <enum xkb_keymap_format> -> <enum xkb_keymap_compile_flags> -> IO (Ptr <struct xkb_keymap>)
#ccall xkb_keymap_ref , Ptr <struct xkb_keymap> -> IO (Ptr <struct xkb_keymap>)
#ccall xkb_keymap_unref , Ptr <struct xkb_keymap> -> IO ()
#ccall xkb_keymap_get_as_string , Ptr <struct xkb_keymap> -> <enum xkb_keymap_format> -> IO CString
#ccall xkb_keymap_min_keycode , Ptr <struct xkb_keymap> -> IO CUInt
#ccall xkb_keymap_max_keycode , Ptr <struct xkb_keymap> -> IO CUInt
#callback xkb_keymap_key_iter_t , Ptr <struct xkb_keymap> -> CUInt -> Ptr () -> IO ()
#ccall xkb_keymap_key_for_each , Ptr <struct xkb_keymap> -> <xkb_keymap_key_iter_t> -> Ptr () -> IO ()
#ccall xkb_keymap_key_get_name , Ptr <struct xkb_keymap> -> CUInt -> IO CString
#ccall xkb_keymap_key_by_name , Ptr <struct xkb_keymap> -> CString -> IO CUInt
#ccall xkb_keymap_num_mods , Ptr <struct xkb_keymap> -> IO CUInt
#ccall xkb_keymap_mod_get_name , Ptr <struct xkb_keymap> -> CUInt -> IO CString
#ccall xkb_keymap_mod_get_index , Ptr <struct xkb_keymap> -> CString -> IO CUInt
#ccall xkb_keymap_num_layouts , Ptr <struct xkb_keymap> -> IO CUInt
#ccall xkb_keymap_layout_get_name , Ptr <struct xkb_keymap> -> CUInt -> IO CString
#ccall xkb_keymap_layout_get_index , Ptr <struct xkb_keymap> -> CString -> IO CUInt
#ccall xkb_keymap_num_leds , Ptr <struct xkb_keymap> -> IO CUInt
#ccall xkb_keymap_led_get_name , Ptr <struct xkb_keymap> -> CUInt -> IO CString
#ccall xkb_keymap_led_get_index , Ptr <struct xkb_keymap> -> CString -> IO CUInt
#ccall xkb_keymap_num_layouts_for_key , Ptr <struct xkb_keymap> -> CUInt -> IO CUInt
#ccall xkb_keymap_num_levels_for_key , Ptr <struct xkb_keymap> -> CUInt -> CUInt -> IO CUInt
#ccall xkb_keymap_key_get_syms_by_level , Ptr <struct xkb_keymap> -> CUInt -> CUInt -> CUInt -> Ptr (Ptr CUInt) -> IO CInt
#ccall xkb_keymap_key_repeats , Ptr <struct xkb_keymap> -> CUInt -> IO CInt
#ccall xkb_state_new , Ptr <struct xkb_keymap> -> IO (Ptr <struct xkb_state>)
#ccall xkb_state_ref , Ptr <struct xkb_state> -> IO (Ptr <struct xkb_state>)
#ccall xkb_state_unref , Ptr <struct xkb_state> -> IO ()
#ccall xkb_state_get_keymap , Ptr <struct xkb_state> -> IO (Ptr <struct xkb_keymap>)
{- enum xkb_key_direction {
    XKB_KEY_UP, XKB_KEY_DOWN
}; -}
#integral_t enum xkb_key_direction
#num XKB_KEY_UP
#num XKB_KEY_DOWN
{- enum xkb_state_component {
    XKB_STATE_MODS_DEPRESSED = 1 << 0,
    XKB_STATE_MODS_LATCHED = 1 << 1,
    XKB_STATE_MODS_LOCKED = 1 << 2,
    XKB_STATE_MODS_EFFECTIVE = 1 << 3,
    XKB_STATE_LAYOUT_DEPRESSED = 1 << 4,
    XKB_STATE_LAYOUT_LATCHED = 1 << 5,
    XKB_STATE_LAYOUT_LOCKED = 1 << 6,
    XKB_STATE_LAYOUT_EFFECTIVE = 1 << 7,
    XKB_STATE_LEDS = 1 << 8
}; -}
#integral_t enum xkb_state_component
#num XKB_STATE_MODS_DEPRESSED
#num XKB_STATE_MODS_LATCHED
#num XKB_STATE_MODS_LOCKED
#num XKB_STATE_MODS_EFFECTIVE
#num XKB_STATE_LAYOUT_DEPRESSED
#num XKB_STATE_LAYOUT_LATCHED
#num XKB_STATE_LAYOUT_LOCKED
#num XKB_STATE_LAYOUT_EFFECTIVE
#num XKB_STATE_LEDS
#ccall xkb_state_update_key , Ptr <struct xkb_state> -> CUInt -> <enum xkb_key_direction> -> IO (<enum xkb_state_component>)
#ccall xkb_state_update_mask , Ptr <struct xkb_state> -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO (<enum xkb_state_component>)
#ccall xkb_state_key_get_syms , Ptr <struct xkb_state> -> CUInt -> Ptr (Ptr CUInt) -> IO CInt
#ccall xkb_state_key_get_utf8 , Ptr <struct xkb_state> -> CUInt -> CString -> CSize -> IO CInt
#ccall xkb_state_key_get_utf32 , Ptr <struct xkb_state> -> CUInt -> IO CUInt
#ccall xkb_state_key_get_one_sym , Ptr <struct xkb_state> -> CUInt -> IO CUInt
#ccall xkb_state_key_get_layout , Ptr <struct xkb_state> -> CUInt -> IO CUInt
#ccall xkb_state_key_get_level , Ptr <struct xkb_state> -> CUInt -> CUInt -> IO CUInt
{- enum xkb_state_match {
    XKB_STATE_MATCH_ANY = 1 << 0,
    XKB_STATE_MATCH_ALL = 1 << 1,
    XKB_STATE_MATCH_NON_EXCLUSIVE = 1 << 16
}; -}
#integral_t enum xkb_state_match
#num XKB_STATE_MATCH_ANY
#num XKB_STATE_MATCH_ALL
#num XKB_STATE_MATCH_NON_EXCLUSIVE
#ccall xkb_state_serialize_mods , Ptr <struct xkb_state> -> <enum xkb_state_component> -> IO CUInt
#ccall xkb_state_serialize_layout , Ptr <struct xkb_state> -> <enum xkb_state_component> -> IO CUInt
#ccall xkb_state_mod_name_is_active , Ptr <struct xkb_state> -> CString -> <enum xkb_state_component> -> IO CInt
#ccall xkb_state_mod_names_are_active , Ptr <struct xkb_state> -> <enum xkb_state_component> -> <enum xkb_state_match> -> IO CInt
#ccall xkb_state_mod_index_is_active , Ptr <struct xkb_state> -> CUInt -> <enum xkb_state_component> -> IO CInt
#ccall xkb_state_mod_indices_are_active , Ptr <struct xkb_state> -> <enum xkb_state_component> -> <enum xkb_state_match> -> IO CInt
#ccall xkb_state_mod_index_is_consumed , Ptr <struct xkb_state> -> CUInt -> CUInt -> IO CInt
#ccall xkb_state_mod_mask_remove_consumed , Ptr <struct xkb_state> -> CUInt -> CUInt -> IO CUInt
#ccall xkb_state_key_get_consumed_mods , Ptr <struct xkb_state> -> CUInt -> IO CUInt
#ccall xkb_state_layout_name_is_active , Ptr <struct xkb_state> -> CString -> <enum xkb_state_component> -> IO CInt
#ccall xkb_state_layout_index_is_active , Ptr <struct xkb_state> -> CUInt -> <enum xkb_state_component> -> IO CInt
#ccall xkb_state_led_name_is_active , Ptr <struct xkb_state> -> CString -> IO CInt
#ccall xkb_state_led_index_is_active , Ptr <struct xkb_state> -> CUInt -> IO CInt
