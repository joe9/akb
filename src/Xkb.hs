{-# LANGUAGE ForeignFunctionInterface #-}

module Xkb where

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
import Modifiers
import BitMask
import Keymap.CustomDvorak
import Skb

-- https://wiki.haskell.org/Foreign_Function_Interface
-- http://stackoverflow.com/questions/8964362/haskell-stableptr
-- http://stackoverflow.com/questions/14125057/how-to-poke-a-vector-or-to-get-a-ptr-vector-to-its-data
-- https://wiki.haskell.org/Calling_Haskell_from_C

xkb_state_new :: CInt -> IO (StablePtr StateIORef)
xkb_state_new i = do
  stateIORef <- newIORef (pickInitialState i)
  newStablePtr (StateIORef stateIORef)

foreign export ccall  xkb_state_new ::
               CInt -> IO (StablePtr StateIORef)
