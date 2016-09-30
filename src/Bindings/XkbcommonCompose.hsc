{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "../libxkbcommon/xkbcommon/xkbcommon-compose.h"
module Bindings.XkbcommonCompose where
import Foreign.Ptr
#strict_import

{- struct xkb_compose_table; -}
#opaque_t struct xkb_compose_table
{- struct xkb_compose_state; -}
#opaque_t struct xkb_compose_state
{- enum xkb_compose_compile_flags {
    XKB_COMPOSE_COMPILE_NO_FLAGS = 0
}; -}
#integral_t enum xkb_compose_compile_flags
#num XKB_COMPOSE_COMPILE_NO_FLAGS
{- enum xkb_compose_format {
    XKB_COMPOSE_FORMAT_TEXT_V1 = 1
}; -}
#integral_t enum xkb_compose_format
#num XKB_COMPOSE_FORMAT_TEXT_V1
#ccall xkb_compose_table_new_from_locale , Ptr <struct xkb_context> -> CString -> <enum xkb_compose_compile_flags> -> IO (Ptr <struct xkb_compose_table>)
#ccall xkb_compose_table_new_from_file , Ptr <struct xkb_context> -> Ptr <struct _IO_FILE> -> CString -> <enum xkb_compose_format> -> <enum xkb_compose_compile_flags> -> IO (Ptr <struct xkb_compose_table>)
#ccall xkb_compose_table_new_from_buffer , Ptr <struct xkb_context> -> CString -> CSize -> CString -> <enum xkb_compose_format> -> <enum xkb_compose_compile_flags> -> IO (Ptr <struct xkb_compose_table>)
#ccall xkb_compose_table_ref , Ptr <struct xkb_compose_table> -> IO (Ptr <struct xkb_compose_table>)
#ccall xkb_compose_table_unref , Ptr <struct xkb_compose_table> -> IO ()
{- enum xkb_compose_state_flags {
    XKB_COMPOSE_STATE_NO_FLAGS = 0
}; -}
#integral_t enum xkb_compose_state_flags
#num XKB_COMPOSE_STATE_NO_FLAGS
#ccall xkb_compose_state_new , Ptr <struct xkb_compose_table> -> <enum xkb_compose_state_flags> -> IO (Ptr <struct xkb_compose_state>)
#ccall xkb_compose_state_ref , Ptr <struct xkb_compose_state> -> IO (Ptr <struct xkb_compose_state>)
#ccall xkb_compose_state_unref , Ptr <struct xkb_compose_state> -> IO ()
#ccall xkb_compose_state_get_compose_table , Ptr <struct xkb_compose_state> -> IO (Ptr <struct xkb_compose_table>)
{- enum xkb_compose_status {
    XKB_COMPOSE_NOTHING,
    XKB_COMPOSE_COMPOSING,
    XKB_COMPOSE_COMPOSED,
    XKB_COMPOSE_CANCELLED
}; -}
#integral_t enum xkb_compose_status
#num XKB_COMPOSE_NOTHING
#num XKB_COMPOSE_COMPOSING
#num XKB_COMPOSE_COMPOSED
#num XKB_COMPOSE_CANCELLED
{- enum xkb_compose_feed_result {
    XKB_COMPOSE_FEED_IGNORED, XKB_COMPOSE_FEED_ACCEPTED
}; -}
#integral_t enum xkb_compose_feed_result
#num XKB_COMPOSE_FEED_IGNORED
#num XKB_COMPOSE_FEED_ACCEPTED
#ccall xkb_compose_state_feed , Ptr <struct xkb_compose_state> -> CUInt -> IO (<enum xkb_compose_feed_result>)
#ccall xkb_compose_state_reset , Ptr <struct xkb_compose_state> -> IO ()
#ccall xkb_compose_state_get_status , Ptr <struct xkb_compose_state> -> IO (<enum xkb_compose_status>)
#ccall xkb_compose_state_get_utf8 , Ptr <struct xkb_compose_state> -> CString -> CSize -> IO CInt
#ccall xkb_compose_state_get_one_sym , Ptr <struct xkb_compose_state> -> IO CUInt
