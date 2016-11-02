{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module OnKeyEvent where

import           Data.Default
import qualified Data.Vector  as V
import           Protolude    hiding (State, group)

--
import KeySymbolDefinitions
import Modifiers
import State

stickyWhatToDoWithKeySymbol :: KeySymbol -> Either ModifierMap KeySymbol
stickyWhatToDoWithKeySymbol XKB_KEY_Control_L =
  Left (ModifierMap XKB_KEY_Control_L Control)
stickyWhatToDoWithKeySymbol XKB_KEY_Shift_L =
  Left (ModifierMap XKB_KEY_Shift_L Shift)
stickyWhatToDoWithKeySymbol XKB_KEY_Alt_L =
  Left (ModifierMap XKB_KEY_Alt_L Mod1)
stickyWhatToDoWithKeySymbol XKB_KEY_Meta_L =
  Left (ModifierMap XKB_KEY_Meta_L Mod3)
stickyWhatToDoWithKeySymbol k = Right k

whatToDoWithKeySymbol :: KeySymbol -> Either ModifierMap KeySymbol
whatToDoWithKeySymbol XKB_KEY_Control_L =
  Left (ModifierMap XKB_KEY_Control_L Control)
whatToDoWithKeySymbol XKB_KEY_Shift_L = Left (ModifierMap XKB_KEY_Shift_L Shift)
whatToDoWithKeySymbol XKB_KEY_Alt_L = Left (ModifierMap XKB_KEY_Alt_L Mod1)
whatToDoWithKeySymbol XKB_KEY_Meta_L = Left (ModifierMap XKB_KEY_Meta_L Mod3)
whatToDoWithKeySymbol k = Right k
