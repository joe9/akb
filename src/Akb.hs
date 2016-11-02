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
pickInitialState :: ByteString -> State
pickInitialState s
  | s == "customDvorak" = customDvorak
  | s == "customDvorakSticky" = customDvorakSticky
  | otherwise = def

keyEvent
  :: State
  -> KeyCode
  -> KeyDirection
  -> (Maybe (KeySymbol, Repeat, Modifiers, Word32, Word8), State)
keyEvent state keycode Pressed =
  (\(ks,r, state) -> (addUTF (ks, r, (sEffectiveModifiers state)), state))
    (onPress keycode state)
keyEvent state keycode Released = (Nothing, onRelease keycode state)

addUTF :: (KeySymbol, Repeat, Modifiers) -> Maybe (KeySymbol, Repeat, Modifiers, Word32, Word8)
addUTF (ks,r,mods) =
  let (utf32, utf8) = keySymbolToUTF8 ks
  in Just (ks, r, mods, utf32, utf8)
