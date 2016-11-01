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
  Left
    (ModifierMap
       XKB_KEY_Control_L
       Control
       (stickyPressModifier XKB_KEY_Control_L Control)
       identity)
stickyWhatToDoWithKeySymbol XKB_KEY_Shift_L =
  Left
    (ModifierMap
       XKB_KEY_Shift_L
       Shift
       (stickyPressModifier XKB_KEY_Shift_L Shift)
       identity)
stickyWhatToDoWithKeySymbol XKB_KEY_Alt_L =
  Left
    (ModifierMap
       XKB_KEY_Alt_L
       Mod1
       (stickyPressModifier XKB_KEY_Alt_L Mod1)
       identity)
stickyWhatToDoWithKeySymbol XKB_KEY_Meta_L =
  Left
    (ModifierMap
       XKB_KEY_Meta_L
       Mod3
       (stickyPressModifier XKB_KEY_Meta_L Mod3)
       identity)
stickyWhatToDoWithKeySymbol k = Right k

whatToDoWithKeySymbol :: KeySymbol -> Either ModifierMap KeySymbol
whatToDoWithKeySymbol XKB_KEY_Control_L =
  Left
    (ModifierMap
       XKB_KEY_Control_L
       Control
       (pressModifier XKB_KEY_Control_L Control)
       (releaseModifier XKB_KEY_Control_L Control))
whatToDoWithKeySymbol XKB_KEY_Shift_L =
  Left
    (ModifierMap
       XKB_KEY_Shift_L
       Shift
       (pressModifier XKB_KEY_Shift_L Shift)
       (releaseModifier XKB_KEY_Shift_L Shift))
whatToDoWithKeySymbol XKB_KEY_Alt_L =
  Left
    (ModifierMap
       XKB_KEY_Alt_L
       Mod1
       (pressModifier XKB_KEY_Alt_L Mod1)
       (releaseModifier XKB_KEY_Alt_L Mod1))
whatToDoWithKeySymbol XKB_KEY_Meta_L =
  Left
    (ModifierMap
       XKB_KEY_Meta_L
       Mod3
       (pressModifier XKB_KEY_Meta_L Mod3)
       (releaseModifier XKB_KEY_Meta_L Mod3))
whatToDoWithKeySymbol k = Right k
