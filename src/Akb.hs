module Akb where

--   http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
import Data.Default
import Data.IORef
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.StablePtr

--
import Keymap.CustomDvorak
import KeySymbolDefinitions
import Modifiers
import State

-- import BitMask
pickInitialState :: CInt -> State
pickInitialState 0 = customDvorak
pickInitialState 1 = customDvorakSticky
pickInitialState _ = def

-- skb_state_key_get_utf ptr keyCode = withState ptr (keyCodeToUTF keyCode)
keyEvent :: State
         -> KeyCode
         -> KeyDirection
         -> (Maybe (KeySymbol, Modifiers), State)
keyEvent state keycode Pressed =
  (\(a, b) -> (Just a, b)) (onKeyPress keycode state)
keyEvent state keycode Released = (Nothing, onKeyRelease keycode state)
