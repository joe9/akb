{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Akb where

--   http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import           Data.Default
import           Data.IORef
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.StablePtr
import           Protolude         hiding (State, group)

import Keymap.CustomDvorak
import KeySymbolDefinitions
import KeySymbolToUTF
import Modifiers
import State

-- import BitMask
pickInitialState :: CInt -> State
pickInitialState 0 = customDvorak
pickInitialState 1 = customDvorakSticky
pickInitialState _ = def

pickInitialStateS :: ByteString -> State
pickInitialStateS s
  | s == "customDvorak" = customDvorak
  | s == "customDvorakStick" = customDvorakSticky
  | otherwise = def

-- skb_state_key_get_utf ptr keyCode = withState ptr (keyCodeToUTF keyCode)
keyEvent
  :: State
  -> KeyCode
  -> KeyDirection
  -> (Maybe (KeySymbol, Modifiers, Word32, Word8), State)
keyEvent state keycode Pressed =
  (\(kms, state) -> (Just (uncurry addUTF kms), state))
    (onKeyPress keycode state)
keyEvent state keycode Released = (Nothing, onKeyRelease keycode state)

addUTF :: KeySymbol -> Modifiers -> (KeySymbol, Modifiers, Word32, Word8)
addUTF ks mods =
  let (utf32, utf8) = keySymbolToUTF8 ks
  in (ks, mods, utf32, utf8)
