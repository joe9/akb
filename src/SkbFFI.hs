{-# LANGUAGE ForeignFunctionInterface #-}

module SkbFFI where

--   http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
import Foreign.C.Types
import Foreign.C.String
import Foreign.StablePtr
import Data.Word
import Data.Default
import Data.IORef
--
import KeySymbolDefinitions
import State
import Keymap.CustomDvorak
import Modifiers
import Skb

foreign export ccall  skb_state_new ::
               CInt -> IO (StablePtr StateIORef)

foreign export ccall skb_state_key_get_one_sym ::
               StablePtr StateIORef -> KeyCode -> IO Word32

foreign export ccall  skb_state_unref ::
               StablePtr StateIORef -> IO ()

foreign export ccall  skb_state_update_key ::
               StablePtr StateIORef -> KeyCode -> Word32 -> IO Word32 -- ^UpdatedStateComponents

foreign export ccall  skb_state_update_mask ::
               StablePtr StateIORef -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Word32 -- ^UpdatedStateComponents

foreign export ccall  skb_keymap_key_repeats ::
               Word32 -> KeyCode -> Word32

foreign export ccall  skb_state_serialize_state_component ::
               StablePtr StateIORef -> Word32 -> IO Word32

foreign export ccall  skb_state_key_get_utf ::
               StablePtr StateIORef -> KeyCode -> IO Word32

foreign export ccall skb_keymap_mod_get_index ::
               Word32 -> CString -> IO Word32

foreign export ccall skb_keymap_led_get_index ::
               Word32 -> CString -> IO Word32
