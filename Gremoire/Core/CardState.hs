module Core.CardState
  ( check
  , assertEq
  , subset
  , within
  , extract
  , getZone
  , getHero
  ) where

-- This module provides a helpful public 'library' of tool functions to insepct
-- a cardstate that has been evaluated

import Core.Fields
import Core.Cut

import Internal.Bytes
import Internal.Game.Types (CardID, CardState)
import qualified Data.Map as Map (Map, lookup, keys, foldrWithKey, filterWithKey)

-- Checks the integer value stored in the cards field
-- will return the provided default in that the card or field
-- value is not given
check :: CardID -> Field -> U8 -> CardState -> U8
check cID fld def cs
  = case (=<<) (Map.lookup fld) $ Map.lookup cID cs of
      Nothing -> def
      (Just x) -> x

-- Assert that the field value of cardid is equal to the given value
assertEq :: CardID -> Field -> U8 -> CardState -> Bool
assertEq cID fld val cs
  = case (=<<) (Map.lookup fld) $ Map.lookup cID cs of
      Nothing -> False
      (Just x) -> x == val

subset :: [CardID] -> CardState -> CardState
subset cIDs = Map.filterWithKey (\cID _ -> cID `elem` cIDs)

-- Returns which cards are still in the cardstate. Used after some sort of
-- selection or filtration
within :: CardState -> [CardID]
within = Map.keys

extract :: Field -> CardState -> [(CardID, U8)]
extract fld = Map.foldrWithKey f []
  where
    f cID fm = case Map.lookup fld fm of
                 Nothing -> id
                 (Just x) -> ((cID, x) :)

-- Return all the cards that are in the zone
getZone :: Zone -> CardState -> [CardID]
getZone zone = within . refine Zone Eq (enumToU8 zone)

-- Return the hero card of the respective hero (error on 0)
getHero :: Owner -> CardState -> CardID
getHero o
  | u8 o == 0 = undefined
  | otherwise = head . getZone Throne
