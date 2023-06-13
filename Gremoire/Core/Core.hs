module Core.Core
  ( module Core.Ability
  , module Core.Card
  , module Core.CardState
  , module Core.Cut
  , module Core.Fields
  , module Core.GameState
  , module Core.Guard
  , module Core.History
  , module Core.Resolve
  , module Core.Targeting
  , module Core.Trigger
  , module Core.Types
  , fromList
  ) where

import Core.Ability
import Core.Card
import Core.CardState
import Core.Cut
import Core.Fields
import Core.GameState
import Core.Guard
import Core.History
import Core.Resolve
import Core.Targeting
import Core.Trigger
import Core.Types

import qualified Data.Map as Map (Map, fromList)

fromList :: Ord k => [(k, a)] -> Map.Map k a
fromList = Map.fromList
