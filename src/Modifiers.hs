{-# LANGUAGE DefaultSignatures #-}

module Modifiers where

import Data.Bits
import Data.Word
--
import BitMask
import NamesPatterns

type Modifiers = Word32

-- corresponding to real modifiers of XKB
-- the order of the below keys correspond to the mod_index defined
--   in libxkbcommon/src/compose/parser.c, hence using Enum to
-- represent the modifier index
-- below from wlc/include/wlc/wlc.h
-- /** Mods in interface.keyboard.key function. */
-- enum wlc_modifier_bit {
--    WLC_BIT_MOD_SHIFT = 1<<0,
--    WLC_BIT_MOD_CAPS = 1<<1,
--    WLC_BIT_MOD_CTRL = 1<<2,
--    WLC_BIT_MOD_ALT = 1<<3,
--    WLC_BIT_MOD_MOD2 = 1<<4,
--    WLC_BIT_MOD_MOD3 = 1<<5,
--    WLC_BIT_MOD_LOGO = 1<<6,
--    WLC_BIT_MOD_MOD5 = 1<<7,
-- };
data Modifier
  = Shift
  | Lock
  | Control
  | Mod1 -- Alt
  | Mod2
  | Mod3
  | Mod4
  | Mod5
  deriving (Bounded, Enum, Eq, Show)

instance ToBitMask Modifier

setModifier :: Modifiers -> Modifier -> Modifiers
-- setModifier ms = (.|.) ms . toBitMask
setModifier ms = setBit ms . fromEnum

clearModifier :: Modifiers -> Modifier -> Modifiers
clearModifier ms = clearBit ms . fromEnum

testModifier :: Modifiers -> Modifier -> Bool
testModifier ms = testBit ms . fromEnum

modifierIndex :: String -> Word32
modifierIndex name
  | name == (unName XKB_MOD_NAME_SHIFT) = fromIntegral (fromEnum Shift)
  | name == (unName XKB_MOD_NAME_CAPS) = fromIntegral (fromEnum Lock)
  | name == (unName XKB_MOD_NAME_CTRL) = fromIntegral (fromEnum Control)
  | name == (unName XKB_MOD_NAME_ALT) = fromIntegral (fromEnum Mod1)
  | name == (unName XKB_MOD_NAME_NUM) = fromIntegral (fromEnum Mod2)
  | name == (unName XKB_MOD_NAME_LOGO) = fromIntegral (fromEnum Mod4)
  -- from xkbcommon/xkbcommon.h
  -- #define XKB_MOD_INVALID     (0xffffffff)
  | otherwise = (0xffffffff)

ledIndex :: String -> Word32
ledIndex name
  | name == (unName XKB_LED_NAME_CAPS) = 0
  | name == (unName XKB_LED_NAME_NUM) = 1
  | name == (unName XKB_LED_NAME_SCROLL) = 2
  -- from xkbcommon/xkbcommon.h
  -- #define XKB_LED_INVALID     (0xffffffff)
  | otherwise = (0xffffffff)
