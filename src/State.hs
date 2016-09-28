
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
    , KeyCode(..)
    , Level(..)
    , onKeyCode
    ) where

import qualified Data.Vector as V
import Data.Default
import Data.Maybe
--
import KeySymbolDefinitions

data Group = Group (V.Vector KeySymbol)
  | Groups Int (V.Vector Group) -- Int for the index of the current active group

data Modifier = LeftControl | RightControl | LeftAlt | RightAlt | LeftShift | RightShift deriving (Eq, Show)

data ModifierMap = ModifierMap {mKeySymbol :: KeySymbol
                         , mModifier :: Modifier
                         , mAction :: (State -> State)}

instance Eq ModifierMap where
  (==) (ModifierMap k t _) (ModifierMap k1 t1 _) = (k == k1) && (t == t1)

type KeyCode = Int
type Level = Int

data State = State { sKeymap :: V.Vector Group
                   , sOnKey :: KeySymbol -> Either ModifierMap KeySymbol
                   , sLevel :: State -> Level
                   , sEffectiveModifiers :: [Modifier]
                   , sLatchedModifiers :: [Modifier]
                   , sLockedModifiers :: [Modifier]
                   }

instance Default State where
  def = State V.empty onKey shiftIsLevelTwo [] [] []

onKeyCode :: KeyCode -> State -> (Maybe KeySymbol, State)
onKeyCode keycode state =
  fromMaybe (Nothing,state)
     ((sKeymap state) V.!? keycode >>= lookupFromGroup (sLevel state state) >>= stateChangeOnKey state)

-- TODO level might modify state if it cosumes Shift modifier
shiftIsLevelTwo :: State -> Level
shiftIsLevelTwo s
  | elem LeftShift allCurrentModifiers || elem RightShift allCurrentModifiers
  = 1 -- zero indexed
  | otherwise = 0 -- zero indexed
  where allCurrentModifiers = sEffectiveModifiers s ++ sLatchedModifiers s ++ sLockedModifiers s

lookupFromGroup :: Int -> Group -> Maybe KeySymbol
lookupFromGroup i (Group v) = v V.!? i

onKey :: KeySymbol -> Either ModifierMap KeySymbol
onKey XK_Control_L = Left (ModifierMap XK_Control_L LeftControl (updateModifiers XK_Control_L LeftControl))
onKey k  = Right k

stickyUpdateModifiers :: KeySymbol -> Modifier -> State -> State
stickyUpdateModifiers _ m state
  | elem m (sLockedModifiers state) = state
  | elem m (sLatchedModifiers state) = state {sLockedModifiers = m : sLockedModifiers state}
  | elem m (sEffectiveModifiers state) = state {sLatchedModifiers = m : sLatchedModifiers state}
  | otherwise = state {sEffectiveModifiers = m : sEffectiveModifiers state}

updateModifiers :: KeySymbol -> Modifier -> State -> State
updateModifiers _ m state
  | elem m (sEffectiveModifiers state) = state
  | otherwise = state {sEffectiveModifiers = m : sEffectiveModifiers state}

stateChangeOnKey :: State -> KeySymbol -> Maybe (Maybe KeySymbol, State)
stateChangeOnKey s keycode =
  case onKey keycode of
    (Right ks) -> Just (Just ks,s)
    (Left (ModifierMap _ _ f)) -> Just (Nothing,f s)

group :: [KeySymbol] -> Group
group = Group . V.fromList

groups :: [[KeySymbol]] -> Group
groups = Groups 0 . V.fromList . fmap group

keyCodeAndGroupsToKeymap :: [(Int,Group)] -> V.Vector Group
keyCodeAndGroupsToKeymap keycodes =
  let lastKeyCode = 1 + fst (last keycodes)
  in (V.//) (V.replicate lastKeyCode (Group V.empty)) keycodes
