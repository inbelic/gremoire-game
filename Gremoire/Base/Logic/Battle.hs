module Base.Logic.Battle where

-- This module looks to implement the logic for determining which unit will
-- damage which unit and how much damage the heros will receive respectively,
-- as well as, some of the logic into determining which units will be nominated

import Core.Core


----------------
  -- Nominations
----------------
-- A valid nomination is a card that is in the Owners Barrack zone and is not
-- already nominate. We use a bit of a hack here to target the owner if
-- they are done nominating
validNomins :: TargetID -> Owner -> Targeting
validNomins tID owner = Targeting $
  \_ gs ->
    let nomins = within
               . refine Nominated Eq (enumToU8 False)
               . refine Owner Eq owner
               . refine Zone Eq (enumToU8 Barrack)
               . getCS $ gs
     in case nomins of
          [] -> [(tID, Given (CardID owner))]
          xs -> [(tID, ) . Inquire . (:) (CardID owner) $ xs]

-- Here we check that all players are done nominating
playersDoneNoms :: [Owner] -> Trigger
playersDoneNoms owners = Trigger $
  \_ gs -> null . refine Nominated Eq (enumToU8 False)
            . subset (map CardID owners) $ getCS gs

----------------
  -- Strikes
----------------
-- Have a unit strike another strike by reducing its toughness
doStrike :: Resolve
doStrike = Resolve $
  \cID tcID gs -> shift Toughness True . check cID Power (U8 0) $ getCS gs

-- Determine the current units strike target
detStrikeTarget :: TargetID -> Targeting
detStrikeTarget tID = Targeting $
  \cID gs -> case computeStrike cID $ getCS gs of
               Nothing -> []
               (Just tcID) -> [(tID, Given tcID)]


-- Recusively compute the which units will strike which units
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

-- Helpers
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
