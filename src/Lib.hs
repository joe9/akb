
{-# LANGUAGE ForeignFunctionInterface  #-}

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
import Foreign.C.Types
import          qualified Data.List.NonEmpty as NonEmpty
--   http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
import qualified Foreign.Ptr as P
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Internal as SVI
import qualified GHC.ForeignPtr as FP
import Data.IORef
--
import KeySymbolDefinitions
import State
import Keymap.CustomDvorak

-- https://wiki.haskell.org/Foreign_Function_Interface

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> CInt

fibonacciv :: Int -> SV.Vector Int
fibonacciv n = SV.fromList [1,2]

fibonacciv_hs :: CInt -> SV.Vector CInt
fibonacciv_hs = SV.map fromIntegral . fibonacciv . fromIntegral

vectorToPtr0 :: SV.Storable a => SV.Vector a -> (P.Ptr a,Int)
vectorToPtr0 vector =
 let (foreignPtr,size) = SV.unsafeToForeignPtr0 vector
 in (SVI.getPtr foreignPtr,size)

-- http://stackoverflow.com/questions/8964362/haskell-stableptr
data StateIORef = StateIORef (IORef State)

skb_state_new :: CInt -> IO (StablePtr StateIORef)
skb_state_new i = do
  let (configIndex,config) = selectValidConfig i
  stateIORef <- newIORef (def {sConfigIndex = configIndex})
  return newStablePtr stateIORef

skb_state_key_get_one_sym :: StablePtr StateIORef -> KeyCode -> IO Word32
skb_state_key_get_one_sym ptr keycode = do
  stateIORef <- deRefStablePtr ptr
  state <- readIORef stateIORef
  (keySym, updatedState) <- onKeyCode (selectConfig (sConfigIndex state)) state keycode
  writeIORef stateIORef updatedState
  case keySym of
    Just k -> return k
    Nothing -> return XK_NoSymbol

skb_state_unref :: StablePtr StateIORef -> IO ()
skb_state_unref = freeStablePtr

selectValidConfig :: CInt -> (CInt,Config)
selectValidConfig i =
  case configs NonEmpty.!! i of
    (Just config) -> (i,config)
    Nothing -> (0,configs NonEmpty.!! 0)

selectConfig :: CInt -> Config
selectConfig i =
  case configs NonEmpty.!! i of
    (Just config) -> config
    Nothing -> configs NonEmpty.!! 0
