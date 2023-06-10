module Core.Logic.Battle where

-- This module looks to implement the logic for determining which unit will
-- damage which unit and how much damage the heros will receive respectively

import Core.CardState
import Core.GameState
import Core.Fields
import Core.Cut

import Internal.Bytes
import Internal.Game.Types

computeStrike :: CardID -> CardState -> Maybe CardID
computeStrike cID cs = determineStrike cID allys ops
  where
    owner = check cID Owner (U8 0) cs
    allys = map getPower
          . reverse
          . orderBy Position
          . refine Owner Eq owner
          . refine Zone Eq (enumToU8 Battlefield)
          $ cs
    ops = map getToughness
        . reverse
        . orderBy Position
        . refine Owner Neq owner
        . refine Zone Eq (enumToU8 Battlefield)
        $ cs

getPower :: (CardID, FieldMap) -> (CardID, Power)
getPower (cID, fm) = (cID, check' Power (U8 0) fm)

getToughness :: (CardID, FieldMap) -> (CardID, Toughness)
getToughness (cID, fm) = (cID, check' Toughness (U8 0) fm)

determineStrike :: CardID -> [(CardID, Power)]
              -> [(CardID, Toughness)] -> Maybe CardID
determineStrike cID [] [] = Nothing
determineStrike cID [] _  = Nothing
determineStrike cID _ []  = Nothing
determineStrike cID ((aCID, pwr) : allyRest) ((oCID, tgh) : opRest)
  | cID == aCID = Just oCID
  | otherwise =
    case compare pwr tgh of
      LT -> determineStrike cID allyRest $ (oCID, tgh - pwr) : opRest
      _ -> determineStrike cID allyRest opRest
