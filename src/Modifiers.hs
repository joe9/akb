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

setShift :: Modifiers -> Modifiers
setShift = flip setBit (fromEnum Shift)

setLock :: Modifiers -> Modifiers
setLock = flip setBit (fromEnum Lock)

setControl :: Modifiers -> Modifiers
setControl = flip setBit (fromEnum Control)

setMod1 :: Modifiers -> Modifiers
setMod1 = flip setBit (fromEnum Mod1)

setMod2 :: Modifiers -> Modifiers
setMod2 = flip setBit (fromEnum Mod2)

setMod3 :: Modifiers -> Modifiers
setMod3 = flip setBit (fromEnum Mod3)

setMod4 :: Modifiers -> Modifiers
setMod4 = flip setBit (fromEnum Mod4)

setMod5 :: Modifiers -> Modifiers
setMod5 = flip setBit (fromEnum Mod5)

clearShift :: Modifiers -> Modifiers
clearShift = flip clearBit (fromEnum Shift)

clearLock :: Modifiers -> Modifiers
clearLock = flip clearBit (fromEnum Lock)

clearControl :: Modifiers -> Modifiers
clearControl = flip clearBit (fromEnum Control)

clearMod1 :: Modifiers -> Modifiers
clearMod1 = flip clearBit (fromEnum Mod1)

clearMod2 :: Modifiers -> Modifiers
clearMod2 = flip clearBit (fromEnum Mod2)

clearMod3 :: Modifiers -> Modifiers
clearMod3 = flip clearBit (fromEnum Mod3)

clearMod4 :: Modifiers -> Modifiers
clearMod4 = flip clearBit (fromEnum Mod4)

clearMod5 :: Modifiers -> Modifiers
clearMod5 = flip clearBit (fromEnum Mod5)
