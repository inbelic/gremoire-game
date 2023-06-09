module Core.Base.Trigger where

import Core.CardState
import Core.GameState
import Core.Fields
import Core.History

import Internal.Bytes
import Internal.Game.Types

-- Module to define various common helpers for triggers


-- Wrap a trigger function on the gamestate (just for ergonomics)
checkTrg :: (GameState -> Bool) -> Trigger
checkTrg f = Trigger $ const f

-- Determine if the game has just entered a given phase
enteredPhase :: Phase -> Trigger
enteredPhase p = checkTrg $ any f . current . getHistory
  where
    f (Event _ tcID (Set Phase cp))
      | ruleCardID == tcID = cp == enumToU8 p
      | otherwise = False
    f _ = False

-- Determine if the game is currently in the given Phase
inPhase :: Phase -> Trigger
inPhase p = checkTrg $ assertEq ruleCardID Phase (enumToU8 p) . getCS

-- Determine if the current card has just entered a zone
selfEntered :: Zone -> Trigger
selfEntered zn = Trigger $ \cID -> any (f cID) . current . getHistory
  where
    f cID (Event _ tcID (Set Zone czn))
      | tcID == cID && enumToU8 zn == czn = True
      | otherwise = False
    f _ _ = False
