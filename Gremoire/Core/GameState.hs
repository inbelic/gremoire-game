module Core.GameState
  ( Game(..)
  , GameState(..)
  , peek
  , isStackEmpty
  , noAbilitiesTriggering
  ) where

import Core.Card (view)
import Core.History

import Internal.Game.Types
import Internal.Game.Views (CompiledWindows)

peek :: Game -> CompiledWindows -> GameState
peek (Game stck hist crds) = uncurry (GameState stck hist) . view crds

isStackEmpty :: GameState -> Bool
isStackEmpty (GameState stck _ _ _) = null stck

noAbilitiesTriggering :: GameState -> Bool
noAbilitiesTriggering (GameState _ hist _ _)
  = null . current $ hist
