
module State
    ( State(..)
    , KeySymbol(..)
    , group
    , groups
    , keycodeAndGroupsToKeymap
    ) where

import qualified Data.Vector as V

data Modifier = LeftControl | RightControl | LeftAlt | RightAlt | LeftShift | RightShift deriving (Eq, Show)

data KeySymbol = Value Int | Modifier Modifier (State -> State) | NoSymbol

data State = State { sKeymap :: V.Vector Group
                   , sOnKeyCode :: State -> (State, Maybe KeySymbol)
                   }

data Group = Group (V.Vector KeySymbol)
  | Groups Int (V.Vector Group) -- Int for the index of the current active group

group :: [KeySymbol] -> Group
group = Group . V.fromList

groups :: [[KeySymbol]] -> Group
groups = Groups 0 . V.fromList . fmap group

keycodeAndGroupsToKeymap :: [(Int,Group)] -> V.Vector Group
keyCodeAndGroupsToKeymap keycodes =
  let lastKeyCode = fst (last keycodes)
  in (V.//) (V.replicate lastKeyCode (Group V.empty)) keycodes
