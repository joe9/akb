module State where

import           Data.Bits
import           Data.Default
import           Data.Maybe
import          qualified Data.List.NonEmpty as NonEmpty
import Foreign.C.Types
import Data.Word
import Data.Either
import qualified Data.Vector  as V
--
import KeySymbolDefinitions
import Modifiers

data GroupOnOverflow
  = Clamp
  | Wrap

data Group
  = Group (V.Vector KeySymbol)
  | Groups Int
           GroupOnOverflow
           (V.Vector Group) -- Int for the index of the current active group

data ModifierMap = ModifierMap
  { mKeySymbol :: KeySymbol
  , mModifier  :: Modifier
  , mAction    :: (State -> State)
  }

instance Eq ModifierMap where
  (ModifierMap k t _) == (ModifierMap k1 t1 _) = (k == k1) && (t == t1)

type KeyCode = Word32

type Level = Int

-- <geekosaur> but, I am not sure I'd pass that thing to C, but instead build a Map
--             Haskell-side from a Storable value (say, CInt) to Haskell-side State
--             values, and give C the CInt
-- <joe9> geekosaur: such as : So, having function pointers in the State datatype is not
--        a big deal? I could just create the State data type in C and marshall/fill it
--        with values from haskell?
-- <joe9> geekosaur: oh, ok. That was what I was thinking. Makes perfect sense.
-- <geekosaur> if it's changeable, then you make it a Map CInt (IORef State) instead
-- <joe9> https://github.com/joe9/lookup-keycode/blob/master/src/State.hs geekosaur  is
--        the full code if you are wondering.
-- <joe9> geekosaur: no it is not changeable from C or at runtime. It is just changeable
--        at start up, maybe.
-- <joe9> geekosaur: my question was, C program call init, we give it a CInt and store
--        the mapping (CInt -> State) in an IORef. Now, the C program takes over does
--        something and calls the haskell program with the same CInt again. when the
--        haskell function reads the IORef, it would find the previously stored (CInt ->
--        State) mapping, correct? So, haskell is keeping that IORef somewhere even when
--        the haskell program is not in use (stack,etc).
-- <geekosaur> joe9, why use an IORef there? the idea is that the mapping is static. you
--             could store the State in an IORef if it needs to change, but the mapping
--             from CInt to the (possibly in an IORef) State should be constant otherwise
--             it's useless
-- <joe9> geekosaur: as correctly pointed out by you, the function pointers in the state
--        do not change. But, the modifier values change. I cannot treat the modifiers as
--        constant. https://paste.pound-python.org/show/GZX7xW1ZAtt74s1TfnqZ/ . and, C
--        does not read the state back from me. It just provides me a pointer to the
--        state that I gave it when doing init.
-- <geekosaur> then you do not want the CInt to State mapping itself to be in an IORef
-- <lpaste> Ed_ revised  No title :  No title  at http://lpaste.net/231948
-- <joe9> geekosaur: First call of C, Shift key is pressed, haskell program updates the
--        modifiers and stores it in an IORef.
-- <geekosaur> you want Map CInt (IORef State)  [15:31]
-- <geekosaur> you can't even sensibly look up an IORef (CInt -> State), and it isn't
--             what you think anyway (it's a function, and I can't see why you'd want to
--             change it)  [15:32]
-- <joe9> geekosaur: another question, haskell program gets garbage collected when I use
--        it this way, correct?
-- <geekosaur> it does, yes
-- <geekosaur> this is another reason to pass the CInt, you don't care and the C side of
--             things doesn't care
-- <geekosaur> on the C side, it's an int, it doesn't change, it's not magic in any way
-- <joe9> geekosaur: my worry was that IORef would lose the value between C calls to the
--        haskell program.  [15:33]
-- <geekosaur> the whole point of IORef is not to lose such things
-- <joe9> geekosaur: ok, Thanks. I presume haskell somehow stores the IORef in a stack
--        somewhere when it hands over control to the C program.  [15:34]
-- <geekosaur> well, you I guess need to arrange for the Map to be held somewhere across
--             invocations from C, that might require the unsafePerformIO global hack
-- <joe9> geekosaur: oh, that was what I was worried about. Saving the haskell state
--        between C calls as haskell program is not running a separate process.  [15:35]
-- <geekosaur> it's still better than trying to make C deal, unless you want to
--             constantly marshal/unmarshal and pass back a new marshaled State to C wth
--             each call
-- <joe9> geekosaur: I agree. the IORef idea is a lot better. just want to get my head
--        around the intricacies.  [15:36]
-- <geekosaur> it does become complex when you're driving it from the C end :/

-- TODO group is a state level attribute, not a key level attribute
-- TODO could change this to an unboxed vector for performance
data State = State
  { sKeymap             :: V.Vector Group
  , sOnKey              :: KeySymbol -> Either ModifierMap KeySymbol
  , sLevel              :: State -> Level -- TODO State -> (Level,State)
  , sGroupNumber        :: State -> (Int, State)
  , sEffectiveModifiers :: !Modifiers
  , sLatchedModifiers   :: !Modifiers
  , sLockedModifiers    :: !Modifiers
  }

instance Default State where def = State V.empty onKey shiftIsLevelTwo (\s -> (0, s)) 0 0 0

-- TODO change the return type to [KeySymbol] as a keyCode can
-- generate multiple key symbols
onKeyCode :: KeyCode -> State -> (Maybe KeySymbol, State)
onKeyCode keycode state =
  fromMaybe
    (Nothing, state)
    ((sKeymap state) V.!? (fromIntegral keycode)
     >>= lookupFromGroup (sLevel state state)
     >>= stateChangeOnKey state)

updateOnKeyCode :: KeyCode -> KeyDirection -> State -> (Maybe KeySymbol, State)
updateOnKeyCode keycode direction state =
  fromMaybe
    (Nothing, state)
    ((sKeymap state) V.!? (fromIntegral keycode)
     >>= lookupFromGroup (sLevel state state)
     >>= stateChangeOnKey state)

-- TODO level might modify state if it cosumes Shift modifier
shiftIsLevelTwo :: State -> Level
shiftIsLevelTwo s
  | testModifier allCurrentModifiers Shift = 1 -- zero indexed
  | otherwise = 0 -- zero indexed
  where
    allCurrentModifiers =
      sEffectiveModifiers s .|. sLatchedModifiers s .|. sLockedModifiers s

lookupFromGroup :: Int -> Group -> Maybe KeySymbol
lookupFromGroup i (Group v) = v V.!? i
lookupFromGroup _ _         = Nothing

onKey :: KeySymbol -> Either ModifierMap KeySymbol
onKey XK_Control_L =
  Left (ModifierMap XK_Control_L Control (updateModifiers XK_Control_L Control))
onKey k = Right k

stickyUpdateModifiers :: KeySymbol -> Modifier -> State -> State
stickyUpdateModifiers _ m state
  | testModifier (sLockedModifiers state) m =
    state {sLockedModifiers = setModifier (sLockedModifiers state) m}
  | testModifier (sEffectiveModifiers state) m =
    state {sLatchedModifiers = setModifier (sLatchedModifiers state) m}
  | otherwise =
    state {sEffectiveModifiers = setModifier (sEffectiveModifiers state) m}

updateModifiers :: KeySymbol -> Modifier -> State -> State
updateModifiers _ m state =
  state {sEffectiveModifiers = setModifier (sEffectiveModifiers state) m}

stateChangeOnKey :: State -> KeySymbol -> Maybe (Maybe KeySymbol, State)
stateChangeOnKey s keycode =
  case onKey keycode of
    (Right ks)                 -> Just (Just ks, s)
    (Left (ModifierMap _ _ f)) -> Just (Nothing, f s)

group :: [KeySymbol] -> Group
group = Group . V.fromList

groups :: [[KeySymbol]] -> Group
groups = Groups 0 Clamp . V.fromList . fmap group

keyCodeAndGroupsToKeymap :: [(Int, Group)] -> V.Vector Group
keyCodeAndGroupsToKeymap keycodes =
  let lastKeyCode = 1 + fst (last keycodes)
  in (V.//) (V.replicate lastKeyCode (Group V.empty)) keycodes

-- same as xkb_key_direction
data KeyDirection = Up | Down deriving (Enum)

-- same as xkb_state_component
type StateComponent = Word8

data StateComponentBit =
--     /** Depressed modifiers, i.e. a key is physically holding them. */
    ModifiersDepressed -- (1 << 0)
--     /** Latched modifiers, i.e. will be unset after the next non-modifier
--      *  key press. */
    | ModifiersLatched -- (1 << 1)
--     /** Locked modifiers, i.e. will be unset after the key provoking the
--      *  lock has been pressed again. */
    | ModifiersLocked -- (1 << 2)
--     /** Effective modifiers, i.e. currently active and affect key
--      *  processing (derived from the other state components).
--      *  Use this unless you explictly care how the state came about. */
    | ModifiersEffective -- (1 << 3)
--     /** Depressed layout, i.e. a key is physically holding it. */
    | LayoutDepressed -- (1 << 4)
--     /** Latched layout, i.e. will be unset after the next non-modifier
--      *  key press. */
    | LayoutLatched -- (1 << 5)
--     /** Locked layout, i.e. will be unset after the key provoking the lock
--      *  has been pressed again. */
    | LayoutLocked -- (1 << 6)
--     /** Effective layout, i.e. currently active and affects key processing
--      *  (derived from the other state components).
--      *  Use this unless you explictly care how the state came about. */
    | LayoutEffective -- (1 << 7)
--     /** LEDs (derived from the other state components). */
    | Leds -- (1 << 8)
    deriving (Enum)

-- What does Layout mean in the above data type?
stateToStateComponent :: State -> StateComponent
stateToStateComponent _ = setBit 0 (fromEnum LayoutEffective)

doesKeyRepeat :: KeyCode -> State -> Bool
doesKeyRepeat state keycode = undefined
--   maybe False isRight
--     ((sKeymap state) V.!? (fromIntegral keycode) >>= sOnKey state)
