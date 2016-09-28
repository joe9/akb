
module Lib
    ( State(..)
    , Group(..)
    , Modifier(..)
    , ModifierMap(..)
    , group
    , groups
    , keyCodeAndGroupsToKeymap
    , onKey
    , updateModifiers
    , shiftIsLevelTwo
    , KeyCode(..)
    , Level(..)
    , onKeyCode

    -- from KeySymbolDefinitions
    , KeySymbol(..)
    , showKeySymbol
    , noKeySymbol

    -- from Keymap.CustomDvorak
    , customDvorakKeymap
    , customDvorak
    ) where

-- import qualified Data.Vector as V
-- import Data.Default
-- import Data.Maybe
--
import KeySymbolDefinitions
import State
import Keymap.CustomDvorak
