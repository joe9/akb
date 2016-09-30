
{-# LANGUAGE ForeignFunctionInterface #-}

module Skb
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
  -- from Lib.hs
  , pickInitialState
  , skb_state_new
  , skb_state_key_get_one_sym
  , skb_state_unref
  ) where

-- import qualified Data.Vector as V
-- import Data.Default
import Foreign.C.Types
import qualified Data.List.NonEmpty as NonEmpty

--   http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
import qualified Foreign.Ptr as P
import Foreign.StablePtr
import Data.Word
import Data.Default
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Internal as SVI
import qualified GHC.ForeignPtr as FP
import Data.IORef
-- import Bindings.Xkbcommon

--
import KeySymbolDefinitions
import State
import Keymap.CustomDvorak

-- https://wiki.haskell.org/Foreign_Function_Interface
fibonacci :: Int -> Int
fibonacci n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> CInt

fibonacciv :: Int -> SV.Vector Int
fibonacciv n = SV.fromList [1, 2]

fibonacciv_hs :: CInt -> SV.Vector CInt
fibonacciv_hs = SV.map fromIntegral . fibonacciv . fromIntegral

-- https://wiki.haskell.org/Foreign_Function_Interface
-- http://stackoverflow.com/questions/8964362/haskell-stableptr
-- http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
-- https://wiki.haskell.org/Calling_Haskell_from_C
data StateIORef =
  StateIORef (IORef State)

skb_state_new :: CInt -> IO (StablePtr StateIORef)
skb_state_new i = do
  stateIORef <- newIORef (pickInitialState i)
  newStablePtr (StateIORef stateIORef)
foreign export ccall skb_state_new :: CInt -> IO (StablePtr StateIORef)

skb_state_key_get_one_sym :: StablePtr StateIORef -> KeyCode -> IO Word32
skb_state_key_get_one_sym ptr keycode = do
  (StateIORef stateIORef) <- deRefStablePtr ptr
  state <- readIORef stateIORef
  let (keySym, updatedState) = onKeyCode keycode state
  writeIORef stateIORef updatedState
  case keySym of
    Just k -> return (unKeySymbol k)
    Nothing -> return (unKeySymbol XK_NoSymbol)
foreign export ccall skb_state_key_get_one_sym :: StablePtr StateIORef -> KeyCode -> IO Word32

skb_state_unref :: StablePtr StateIORef -> IO ()
skb_state_unref = freeStablePtr
foreign export ccall skb_state_unref :: StablePtr StateIORef -> IO ()

pickInitialState :: CInt -> State
pickInitialState 0 = def
pickInitialState _ = customDvorak
