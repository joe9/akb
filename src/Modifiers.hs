{-# LANGUAGE DefaultSignatures #-}

module Modifiers
  ( Modifiers
  , Modifier(..)
  , toBitMask
  , fromBitMask
  , isInBitMask
  , setModifier
  , clearModifier
  , testModifier
  ) where

import Control.Monad
import Data.Bits
import Data.Word

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

-- got this idea from
-- http://stackoverflow.com/questions/15910363/represent-a-list-of-enums-bitwise-as-an-int
class ToBitMask a where
  toBitMask :: a -> Word32
  -- | Using a DefaultSignatures extension to declare a default signature with
  -- an `Enum` constraint without affecting the constraints of the class itself.
  default toBitMask :: Enum a =>
    a -> Word32
  toBitMask = shiftL 1 . fromEnum

instance ToBitMask Modifier

instance (ToBitMask a) =>
         ToBitMask [a] where
  toBitMask = foldr (.|.) 0 . map toBitMask

setModifier :: Modifiers -> Modifier -> Modifiers
-- setModifier ms = (.|.) ms . toBitMask
setModifier ms = setBit ms . fromEnum

clearModifier :: Modifiers -> Modifier -> Modifiers
clearModifier ms = clearBit ms . fromEnum

testModifier :: Modifiers -> Modifier -> Bool
testModifier ms = testBit ms . fromEnum

-- | Not making this a typeclass, since it already generalizes over all
-- imaginable instances with help of `MonadPlus`.
fromBitMask
  :: (MonadPlus m, Enum a, Bounded a, ToBitMask a)
  => Word32 -> m a
fromBitMask bm = msum $ map asInBM $ enumFrom minBound
  where
    asInBM a =
      if isInBitMask bm a
        then return a
        else mzero

isInBitMask
  :: (ToBitMask a)
  => Word32 -> a -> Bool
isInBitMask bm a =
  let aBM = toBitMask a
  in aBM == aBM .&. bm
