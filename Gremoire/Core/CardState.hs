module Core.CardState
  ( check
  ) where

-- This module provides a helpful public 'library' of tool functions to insepct
-- a cardstate that has been evaluated

import Core.Fields
import Core.Cut

import Internal.Bytes
import Internal.Game.Types (CardID, CardState)
import qualified Data.Map as Map (Map, lookup)

-- Checks the integer value stored in the cards field
-- will return the provided default in that the card or field
-- value is not given
check :: CardID -> Field -> U8 -> CardState -> U8
check cID fld def cs
  = case (=<<) (Map.lookup fld) $ Map.lookup cID cs of
      Nothing -> def
      (Just x) -> x
