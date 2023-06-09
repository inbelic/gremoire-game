module Core.Base.Resolve where

import Core.Card
import Core.CardState
import Core.GameState
import Core.Fields
import Core.Cut
import Internal.Game.Types

import Internal.Bytes
import Internal.Misc (maximum')

-- Module to define various common helpers for resolves

moveZone :: Zone -> Resolve
moveZone zn = Resolve $ \_ tcID gs ->
  let owner = check tcID Owner (U8 0) . getCS $ gs
      ownersZone
        = extract Position
        . refine Owner Eq (enumToU8 owner)
        . refine Zone Eq (enumToU8 zn)
        . getCS $ gs
      posn = (+ 1) . maximum' . map snd $ ownersZone
   in set Zone (enumToU8 zn) <> set Position posn


-- it is common that instead of targeting the original card, the owner will
-- return the id of the rules card to change some flag in the state so we
-- can wrap that change in state in this function
orChangeRuleCard :: Change -> Resolve -> Resolve
orChangeRuleCard chng (Resolve r) = Resolve rslv
  where
    rslv cID tcID gs
      | tcID == ruleCardID = chng
      | otherwise = r cID tcID gs
