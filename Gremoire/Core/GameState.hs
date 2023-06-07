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
import Core.Views (CompiledWindows)

peek :: Game -> CompiledWindows -> GameState
peek (Game stck hist crds) = GameState stck hist . view crds

isStackEmpty :: GameState -> Bool
isStackEmpty (GameState stck _ _) = null stck

noAbilitiesTriggering :: GameState -> Bool
noAbilitiesTriggering (GameState _ hist _)
  = null . current $ hist
