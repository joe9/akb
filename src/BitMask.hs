{-# LANGUAGE DefaultSignatures #-}

module BitMask
  ( ToBitMask
  , toBitMask
  , fromBitMask
  , isInBitMask
  ) where

import Control.Monad
import Data.Bits
import Data.Word

-- got this idea from
-- http://stackoverflow.com/questions/15910363/represent-a-list-of-enums-bitwise-as-an-int
class ToBitMask a where
  toBitMask :: a -> Word32
  -- | Using a DefaultSignatures extension to declare a default signature with
  -- an `Enum` constraint without affecting the constraints of the class itself.
  default toBitMask :: Enum a =>
    a -> Word32
  toBitMask = shiftL 1 . fromEnum

instance (ToBitMask a) =>
         ToBitMask [a] where
  toBitMask = foldr (.|.) 0 . map toBitMask

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
