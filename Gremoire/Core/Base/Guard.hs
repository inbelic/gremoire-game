module Core.Base.Guard where

import Core.CardState
import Core.GameState
import Core.Fields

import Internal.Bytes
import Internal.Game.Types

-- Module to define various common helpers for guards

alwaysOk :: Guard
alwaysOk = Guard $ \_ _ _ -> True

inZone :: Zone -> Guard
inZone zn = Guard $
  \_ tcID (GameState _ _ cs _) -> assertEq tcID Zone (enumToU8 zn) cs

checkGrd :: (GameState -> Bool) -> Guard
checkGrd f = Guard $ \_ _ -> f

isRulesCard :: Guard
isRulesCard = Guard $ \_ tcID _ -> tcID == ruleCardID
