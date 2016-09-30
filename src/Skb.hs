{-# LANGUAGE ForeignFunctionInterface #-}

module Skb where

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

foreign export ccall  skb_state_new ::
               CInt -> IO (StablePtr StateIORef)

skb_state_key_get_one_sym :: StablePtr StateIORef -> KeyCode -> IO Word32
skb_state_key_get_one_sym ptr keycode = do
  withState
    ptr
    (\state ->
       let (keySym, updatedState) = onKeyCode keycode state
       in case keySym of
            Just k -> (unKeySymbol k, updatedState)
            Nothing -> (unKeySymbol XKB_KEY_NoSymbol, updatedState))

withState :: StablePtr StateIORef -> (State -> (a, State)) -> IO a
withState ptr f = do
  (StateIORef stateIORef) <- deRefStablePtr ptr
  state <- readIORef stateIORef
  let (a, updatedState) = f state
  writeIORef stateIORef updatedState
  return a

foreign export ccall  skb_state_key_get_one_sym ::
               StablePtr StateIORef -> KeyCode -> IO Word32

skb_state_unref :: StablePtr StateIORef -> IO ()
skb_state_unref = freeStablePtr

foreign export ccall  skb_state_unref ::
               StablePtr StateIORef -> IO ()

pickInitialState :: CInt -> State
pickInitialState 0 = def
pickInitialState _ = customDvorak

{- enum xkb_key_direction {
    XKB_KEY_UP, XKB_KEY_DOWN
}; -}
type CKeyDirection = Word8

-- skb_state_update_key :: StablePtr StateIORef -> KeyCode -> CKeyDirection -> IO StateComponent
-- the StateComponent is not being used by wlc, so why bother
-- returning it.
skb_state_update_key :: StablePtr StateIORef
                     -> KeyCode
                     -> CKeyDirection
                     -> IO ()
skb_state_update_key ptr keycode dir = do
  withState
    ptr
    (\state ->
       let direction = toEnum (fromIntegral dir)
           (_, updatedState) = updateOnKeyCode keycode direction state
       in ((), updatedState))

-- foreign export ccall skb_state_update_key :: StablePtr StateIORef -> KeyCode -> CKeyDirection -> IO StateComponent
foreign export ccall  skb_state_update_key ::
               StablePtr StateIORef -> KeyCode -> CKeyDirection -> IO ()

skb_state_update_mask :: StablePtr StateIORef
                      -> Word8
                      -> Word8
                      -> Word8
                      -> IO ()
skb_state_update_mask ptr depressed latched locked = do
  withState
    ptr
    (\state ->
       ( ()
       , state
         { sEffectiveModifiers = fromIntegral depressed
         , sLatchedModifiers = fromIntegral latched
         , sLockedModifiers = fromIntegral locked
         }))

-- foreign export ccall skb_state_update_key :: StablePtr StateIORef -> KeyCode -> CKeyDirection -> IO StateComponent
foreign export ccall  skb_state_update_mask ::
               StablePtr StateIORef -> Word8 -> Word8 -> Word8 -> IO ()

skb_keymap_key_repeats :: StablePtr StateIORef
                     -> KeyCode
                     -> IO CInt
skb_keymap_key_repeats ptr keycode = do
  withState
    ptr
    (\state ->
       let bool = doesKeyRepeat keycode state
       in (fromIntegral ( fromEnum bool), state))

foreign export ccall  skb_keymap_key_repeats ::
               StablePtr StateIORef -> KeyCode -> IO CInt

-- TODO serialize mods or change wlc to use is_active functions
