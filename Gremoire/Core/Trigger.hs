module Core.Trigger
  ( Trigger(..)
  , both, oneOf
  , checkTrg
  , selfEntered
  , changedField
  ) where

import Core.CardState
import Core.Cut
import Core.GameState
import Core.Fields
import Core.History

import Internal.Bytes
import Internal.Boolean
import Internal.Game.Types

-- Module to define various common helpers for triggers

-- Wrap a trigger function on the gamestate (just for ergonomics)
checkTrg :: (GameState -> Bool) -> Trigger
checkTrg f = Trigger $ const f

-- Determine if the current card has just entered a zone
selfEntered :: Zone -> Trigger
selfEntered zn = Trigger $ \cID -> any (f cID) . current . getHistory
  where
    f cID (Event _ tcID (Set Zone czn))
      | tcID == cID && enumToU8 zn == czn = True
      | otherwise = False
    f _ _ = False

changedField :: Field -> Trigger
changedField fld = Trigger $ \cID -> any (f cID) . current . getHistory
  where
    f cID (Event _ tcID (Set setFld _))
      | tcID == cID && fld == setFld = True
      | otherwise                    = False
    f cID (Event _ tcID (Shift shiftedFld _ _))
      | tcID == cID && fld == shiftedFld = True
      | otherwise                        = False
    f cID (Event _ tcID (Alter alteredField))
      | tcID == cID && fld == alteredField = True
      | otherwise                          = False
    f _ _ = False
