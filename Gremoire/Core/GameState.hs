module Core.GameState
  ( Game(..)
  , GameState(..)
  , peek
  , isStackEmpty
  , noAbilitiesTriggering
  , isActive
  ) where

import Core.Card (view)
import Core.Fields
import Core.CardState
import Core.History

import Internal.Bytes
import Internal.Game.Types
import Internal.Game.Views (CompiledWindows)

peek :: Game -> CompiledWindows -> GameState
peek (Game stck hist crds) = uncurry (GameState stck hist) . view crds

isStackEmpty :: GameState -> Bool
isStackEmpty (GameState stck _ _ _) = null stck

noAbilitiesTriggering :: GameState -> Bool
noAbilitiesTriggering (GameState _ hist _ _)
  = null . current $ hist

-- A player (owner) is active when the flag is equal to them and it is the
-- Seige phase or when the flag is not equal to them and it is the Retaliate
-- phase
isActive :: Owner -> GameState -> Bool
isActive owner gs
  | flag == owner && p == Seige     = True
  | flag /= owner && p == Retaliate = True
  | otherwise                       = False
  where
    flag = (+ 1) . u8ToEnum . check ruleCardID AttackFlag (U8 0) $ getCS gs
    p = u8ToEnum . check ruleCardID Phase (U8 0) $ getCS gs
