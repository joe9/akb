module State
  ( State(..)
  , Group(..)
  , KeySymbol(..)
  , Modifier(..)
  , ModifierMap(..)
  , group
  , groups
  , keyCodeAndGroupsToKeymap
  , onKey
  , updateModifiers
  , shiftIsLevelTwo
  , KeyCode
  , Level
  , onKeyCode
  , stickyUpdateModifiers
  ) where

import           Data.Bits
import           Data.Default
import           Data.Maybe
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

type KeyCode = Int

type Level = Int

data State = State
  { sKeymap             :: V.Vector Group
  , sOnKey              :: KeySymbol -> Either ModifierMap KeySymbol
  , sLevel              :: State -> Level -- TODO State -> (Level,State)
  , sGroupNumber        :: State -> (Int, State)
  , sEffectiveModifiers :: Modifiers
  , sLatchedModifiers   :: Modifiers
  , sLockedModifiers    :: Modifiers
  }

instance Default State where
  def = State V.empty onKey shiftIsLevelTwo (\s -> (0, s)) 0 0 0

-- TODO change the return type to [KeySymbol] as a keyCode can
-- generate multiple key symbols
onKeyCode :: KeyCode -> State -> (Maybe KeySymbol, State)
onKeyCode keycode state =
  fromMaybe
    (Nothing, state)
    ((sKeymap state) V.!? keycode >>= lookupFromGroup (sLevel state state) >>=
     stateChangeOnKey state)

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
