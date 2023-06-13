module Core.CardState
  ( CardState(..)
  , check
  , check'
  , assertEq
  , subset
  , within
  , extract
  , orderBy
  ) where

-- This module provides a helpful public 'library' of tool functions to insepct
-- a cardstate that has been evaluated

import Core.Fields
import Core.Cut

import Internal.Bytes
import Internal.Game.Types (CardID, CardState)
import qualified Data.Map as Map (Map, lookup, keys, foldrWithKey, filterWithKey)
import Data.List (insertBy)

-- Checks the integer value stored in the cards field
-- will return the provided default in that the card or field
-- value is not given
check :: CardID -> Field -> U8 -> CardState -> U8
check cID fld def cs
  = case check' fld def <$> Map.lookup cID cs of
      Nothing -> def
      (Just x) -> x

check' :: Field -> U8 -> FieldMap -> U8
check' fld def fm
  = case Map.lookup fld fm of
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

-- Get all the field values of the cardstate and their associated id
extract :: Field -> CardState -> [(CardID, U8)]
extract fld = Map.foldrWithKey f []
  where
    f cID fm = case Map.lookup fld fm of
                 Nothing -> id
                 (Just x) -> ((cID, x) :)

-- We will output the list version of the cardstate that is ordered by the
-- given field and filters out all cards that do not have the given field
orderBy :: Field -> CardState -> [(CardID, FieldMap)]
orderBy fld = map snd . Map.foldrWithKey (orderedInsert fld) []
  where
    orderedInsert :: Field -> CardID -> FieldMap -> [(U8, (CardID, FieldMap))]
                  -> [(U8, (CardID, FieldMap))]
    orderedInsert fld cID fm
      = case Map.lookup fld fm of
          Nothing -> id
          (Just val) -> insertBy byField (val, (cID, fm))

    byField :: (U8, (CardID, FieldMap)) -> (U8, (CardID, FieldMap)) -> Ordering
    byField x y = compare (fst x) (fst y)
