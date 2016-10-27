module State where

import           Data.Bits
import           Data.Default
import           Data.Maybe
import qualified Data.Vector  as V
import           Data.Word
import           Text.Groom

--
import BitMask
import KeySymbolDefinitions
import KeySymbolToUTF
import Modifiers

-- /** Specifies the direction of the key (press / release). */
-- enum xkb_key_direction {
--     XKB_KEY_UP,   /**< The key was released. */
--     XKB_KEY_DOWN  /**< The key was pressed. */
-- };
data KeyDirection
  = Released -- Up = input_event.value=0
  | Pressed -- Down = input_event.value=1
  | Repeat -- Repeat = input_event.value=2
  deriving (Enum, Eq, Show)

--     /** Depressed modifiers, i.e. a key is physically holding them. */
--     /** Latched modifiers, i.e. will be unset after the next non-modifier
--      *  key press. */
--     /** Locked modifiers, i.e. will be unset after the key provoking the
--      *  lock has been pressed again. */
--     /** Effective modifiers, i.e. currently active and affect key
--      *  processing (derived from the other state components).
--      *  Use this unless you explictly care how the state came about. */
--     /** Depressed layout, i.e. a key is physically holding it. */
--     /** Latched layout, i.e. will be unset after the next non-modifier
--      *  key press. */
--     /** Locked layout, i.e. will be unset after the key provoking the lock
--      *  has been pressed again. */
--     /** Effective layout, i.e. currently active and affects key processing
--      *  (derived from the other state components).
--      *  Use this unless you explictly care how the state came about. */
--     /** LEDs (derived from the other state components). */
-- Layout == Group
-- stateToStateComponent :: State -> UpdatedStateComponents
-- stateToStateComponent _ = setBit 0 (fromEnum GroupEffective)
data GroupOnOverflow
  = Clamp
  | Wrap

data Group
  = Group (V.Vector KeySymbol)
  | Groups GroupOnOverflow
           (V.Vector Group)

data ModifierMap = ModifierMap
  { mKeySymbol   :: KeySymbol
  , mModifier    :: Modifier
  , mWhenPressed :: State -> State
  }

instance Eq ModifierMap where
  (ModifierMap k t _) == (ModifierMap k1 t1 _) = (k == k1) && (t == t1)

type KeyCode = Word16

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
-- TODO could change this to an unboxed vector for performance
data State = State
  { sKeymap :: V.Vector Group
  , sOnKeyEvent :: KeySymbol -> Either ModifierMap KeySymbol
  , sCalculateLevel :: Modifiers -> Level -- if there is a level change
    -- if there is a level change
    -- that changes state, for example,
    -- consuming the shift modifier
  , sConsumeModifiersUsedForCalculatingLevel :: State -> State
    --  effective group = f(locked group + latched group + base group)
    --  f = sNormalizeGroup
    -- The function f ensures that the effective group
    -- is within range. The precise function is specified for the
    -- keyboard and can be retrieved through the keyboard
    -- description. It may wrap around, clamp down, or default. Few
    -- applications will actually examine the effective group, and far
    -- fewer still will examine the locked, latched, and base groups.
  , sNormalizeGroup :: Word32 -> Word32
    -- effective is used for lookups
  , sEffectiveGroup :: !Word32 -- group index is mostly 0 -- (first group)
  , sDepressedGroup :: !Word32 -- keys that are physically or logically down
  , sLatchedGroup :: !Word32
  , sLockedGroup :: !Word32
    -- lookup & effective = modifiers used for lookup
    --   , sLookupModifiers :: !Modifiers
    -- The effective modifiers are the bitwise union of the
    --   locked, latched, and the base modifiers.
    -- effective is used for lookups
  , sEffectiveModifiers :: !Modifiers
  , sDepressedModifiers :: !Modifiers -- modifiers that are physically or logically down
  , sLatchedModifiers :: !Modifiers
  , sLockedModifiers :: !Modifiers
  }

instance Eq State where
  (State _ _ _ _ _ eg dg lag locg em dm lam lom) == (State _ _ _ _ _ egb dgb lagb locgb emb dmb lamb lomb) =
    eg == egb &&
    dg == dgb &&
    lag == lagb &&
    locg == locgb && em == emb && dm == dmb && lam == lamb && lom == lomb

instance Default State where
  def =
    State
      V.empty
      onKey
      shiftIsLevelTwoCalculateLevel
      shiftIsLevelTwoConsumeModifiers
      id
      0
      0
      0
      0
      0
      0
      0
      0

-- TODO change the return type to [KeySymbol] as a keyCode can
-- generate multiple key symbols
lookupKeyCode :: KeyCode -> State -> KeySymbol
lookupKeyCode = onKeyCodeEvent (\_ k -> k) XKB_KEY_NoSymbol

onKeyPress :: KeyCode -> State -> ((KeySymbol, Modifiers), State)
onKeyPress keycode state =
  ((\(k, s) -> ((k, sEffectiveModifiers s), s)) . f keycode) updatedEffectives
  where
    f k s = onKeyCodeEvent stateChangeOnPress (XKB_KEY_NoSymbol, s) k s
    updatedEffectives = updateEffectives state

onKeyRelease :: KeyCode -> State -> State
onKeyRelease keycode state =
  let updatedEffectivesState = updateEffectives state
  in (f keycode) updatedEffectivesState
  where
    f k s = onKeyCodeEvent stateChangeOnRelease s k s

onKeyCodeEvent :: (State -> KeySymbol -> a) -> a -> KeyCode -> State -> a
onKeyCodeEvent f defaultValue keycode state =
  maybe
    defaultValue
    (f state)
    (sKeymap state V.!? fromIntegral keycode >>=
     -- The lookup group is the same as the effective group
     lookupGroup (fromIntegral (sEffectiveGroup state)) >>=
     lookupFromGroup (sCalculateLevel state (sEffectiveModifiers state)))

keyCodeToUTF :: KeyCode -> State -> (Word32, State)
keyCodeToUTF keyCode state =
  onKeyCodeEvent (\_ ks -> (keySymbolToUTF ks, state)) (0, state) keyCode state

findIfKeyRepeats :: KeyCode -> State -> Bool
findIfKeyRepeats = onKeyCodeEvent doesKeyRepeat True

doesKeyRepeat :: State -> KeySymbol -> Bool
doesKeyRepeat s keysymbol =
  case sOnKeyEvent s keysymbol of
    Right _ -> True
    -- assuming all modifiers do not repeat
    Left _  -> False

calculateLevel :: State -> Level
calculateLevel state = sCalculateLevel state (sEffectiveModifiers state)

shiftIsLevelTwoCalculateLevel :: Modifiers -> Level
shiftIsLevelTwoCalculateLevel effectiveModifiers
  | testModifier effectiveModifiers Shift = 1 -- zero indexed
  | otherwise = 0 -- zero indexed

shiftIsLevelTwoConsumeModifiers :: State -> State
shiftIsLevelTwoConsumeModifiers state
  | sCalculateLevel state (sEffectiveModifiers state) > 0 =
    state
    {sDepressedModifiers = clearModifier (sDepressedModifiers state) Shift}
  | otherwise = state

-- nested groups are not allowed
lookupGroup :: Int -> Group -> Maybe Group
lookupGroup _ grp@(Group _) = Just grp
lookupGroup rawGroupIndex (Groups wrapType grps) =
  case grps V.!? groupIndex of
    jg@(Just _) -> jg
    Nothing ->
      case wrapType of
        Clamp -> Just (V.last grps)
        Wrap  -> grps V.!? mod (V.length grps) (groupIndex + 1)
  where
    groupIndex =
      if rawGroupIndex < 0
        then 0
        else rawGroupIndex

-- keySymbolsVectorFromGroup :: Group -> Maybe (V.Vector KeySymbol)
-- keySymbolsVectorFromGroup (Group ks) = Just ks
-- keySymbolsVectorFromGroup _  = Nothing
lookupFromGroup :: Level -> Group -> Maybe KeySymbol
lookupFromGroup level (Group v) =
  case v V.!? level of
    jk@(Just _) -> jk
    Nothing     -> v V.!? 0 -- if there is only 1 level
lookupFromGroup _ _ = Nothing

onKey :: KeySymbol -> Either ModifierMap KeySymbol
onKey XKB_KEY_Control_L =
  Left
    (ModifierMap
       XKB_KEY_Control_L
       Control
       (pressModifier XKB_KEY_Control_L Control))
onKey k = Right k

-- Pressing Esc when having any locked modifiers releases all
clearStickyPresses :: State -> State
clearStickyPresses state =
  state {sLockedModifiers = 0, sLatchedModifiers = 0, sDepressedModifiers = 0}

stickyPressModifier :: KeySymbol -> Modifier -> State -> State
stickyPressModifier _ m state
  | testModifier (sLockedModifiers state) m
   -- if the key is already locked, unlock it
   =
    state
    { sLockedModifiers = clearModifier (sLockedModifiers state) m
    , sDepressedModifiers = setModifier (sDepressedModifiers state) m
    }
  | testModifier (sLatchedModifiers state) m
   -- if the key was previously latched, lock it and remove the latch
   -- but better to ensure that Depressed has it
   =
    state
    { sLockedModifiers = setModifier (sLockedModifiers state) m
    , sLatchedModifiers = clearModifier (sLatchedModifiers state) m
    , sDepressedModifiers = setModifier (sDepressedModifiers state) m
    }
  -- ensure that Depressed and Latched have it
  | otherwise =
    state
    { sLatchedModifiers = setModifier (sLatchedModifiers state) m
    , sDepressedModifiers = setModifier (sDepressedModifiers state) m
    }

pressModifier :: KeySymbol -> Modifier -> State -> State
pressModifier _ = updateDepresseds

releaseModifier :: KeySymbol -> Modifier -> State -> State
releaseModifier _ m state =
  state {sDepressedModifiers = clearModifier (sDepressedModifiers state) m}

stateChangeOnPress :: State -> KeySymbol -> (KeySymbol, State)
stateChangeOnPress state keysymbol =
  case sOnKeyEvent state keysymbol of
    Left (ModifierMap keysym modifier onPressFunction)
    -- not consuming modifiers when a modifier is the result, bug or feature?
    -- updateDepresseds does the same thing as the onPressFunction,
    --   remove it?
     ->
      ( keysym
      , (updateEffectives . updateDepresseds modifier . onPressFunction) state)
    Right keysym -> (keysym, state)

stateChangeOnRelease :: State -> KeySymbol -> State
stateChangeOnRelease state keysymbol =
  case sOnKeyEvent state keysymbol of
    Left (ModifierMap ks m _) -> (updateEffectives . releaseModifier ks m) state
    Right _ -> (updateEffectives . resetLatchesToDepresseds) state

-- if there is no latch set for a depressed modifier, do nothing -- when non-sticky
-- when there is a latch set and it is a depressed modifier, do nothing
-- when there is a latch set and it is not a depressed modifier, remove latch
resetLatchesToDepresseds :: State -> State
resetLatchesToDepresseds state =
  state
  { sLatchedModifiers = sDepressedModifiers state .&. sLatchedModifiers state
  , sLatchedGroup =
      if sLatchedGroup state == sDepressedGroup state
        then sLatchedGroup state
        else 0
  }

updateEffectives :: State -> State
updateEffectives state =
  state
  { sEffectiveModifiers =
      sDepressedModifiers state .|. sLatchedModifiers state .|.
      sLockedModifiers state
  , sEffectiveGroup =
      sNormalizeGroup
        state
        (sDepressedGroup state + sLatchedGroup state + sLockedGroup state)
  }

updateDepresseds :: Modifier -> State -> State
updateDepresseds modifier state =
  state {sDepressedModifiers = setModifier (sDepressedModifiers state) modifier}

group :: [KeySymbol] -> Group
group = Group . V.fromList

groups :: [[KeySymbol]] -> Group
groups = Groups Clamp . V.fromList . fmap group

keyCodeAndGroupsToKeymap :: [(KeyCode, Group)] -> V.Vector Group
keyCodeAndGroupsToKeymap keycodes =
  let lastKeyCode = 1 + fst (last keycodes)
  in (V.//)
       (V.replicate (fromIntegral lastKeyCode) (Group V.empty))
       (fmap (\(f, s) -> ((fromIntegral :: KeyCode -> Int) f, s)) keycodes)

-- might have to add this as a field to State and use lens to avoid
-- the deep nesting code
-- convenience data type to show State data
data ShowState = ShowState
  { ssEffectiveGroup     :: !Word32 -- ^ group index is mostly 0 -- (first group)
  , ssDepressedGroup     :: !Word32 -- ^keys that are physically or logically down
  , ssLatchedGroup       :: !Word32
  , ssLockedGroup        :: !Word32
  , ssEffectiveModifiers :: ![Modifier]
  , ssDepressedModifiers :: ![Modifier] -- ^modifiers that are physically or logically down
  , ssLatchedModifiers   :: ![Modifier]
  , ssLockedModifiers    :: ![Modifier]
  } deriving (Show)

stateToShowState :: State -> ShowState
stateToShowState state =
  ShowState
    (sEffectiveGroup state)
    (sDepressedGroup state)
    (sLatchedGroup state)
    (sLockedGroup state)
    (fromBitMask (sEffectiveModifiers state))
    (fromBitMask (sDepressedModifiers state))
    (fromBitMask (sLatchedModifiers state))
    (fromBitMask (sLockedModifiers state))

instance Show State where
  show = groom . stateToShowState
