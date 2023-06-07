module Core.Views
  ( Window(..)
  , CompiledWindows(..)
  , Mask(..)
  , Masks
  , CompiledMasks(..)
  , filterMasks
  , Comp(..)
  -- Cut re-definitions
  , Constraint(..)
  , Filtration(..)
  , Selection(..)
  , Cut(..)
  -- Cut re-exports
  , trim
  , cut
  ) where

import Core.Fields
import Internal.Bytes (U8)
import Internal.Game.Cut
import qualified Data.Map as Map (filterWithKey)

-- When we want to evaluate the fields of a card when viewing the cards we will
-- use a Window to show which Field we want to asses and the Filtration will
-- filter to a subset of cards that we want to evaluate (view).
--
-- Examples:
--
-- (Cost, Filtration [Constraint Zone Eq (fromEnum Hand)])
--  -> We want to evaluate the Cost field of all cards that are in the hand zone
--
-- (Toughness, Filtration [ Constraint Zone Eq (fromEnum Battlefield)
--                        , Constraint Cost LsEq 5
--                        ]) -> We want to evaluate the Toughness field of
-- all cards that are in the battlefield zone and have a toughness
-- less than or equal to 5
data Window = Window Field (Filtration Field U8)
  -- Field: Which Field of the card we want to evaluate
  -- Filters: Subset of cards that we want to evaluate


-- Compiled windows denote a set of Windows that are in order of their
-- dependencies, so if we consider the ith of n filters, then
-- i cannot have any dependencies of any filters from 0 to i - 1
--
-- We use a newtype so that we don't accidently use uncompiled filters
-- somewhere and enter an infinite loop of dependencies
newtype CompiledWindows = CWindows
  { getWindows :: [Window]
  }

-- All fields will default to not being shown and a mask will denote a
-- rule to whether we should show it our not. We slightly change our
-- intrepretation of a Filter in this instance. Hence, we can read the
-- following as, show Field if the other Fields of the card satisfy
-- the Filters. So the list of filters within a SINGLE mask are a series
-- of AND statements, whereas, the series of masks can are OR's so if any
-- Mask evaluates to True for a field then it will be shown
--
-- Examples:
--  Mask Zone [] -> Always show Zone
--
--  Mask SetID [ Filter Revealed (fromEnum True) Eq
--             , Filter Zone (fromEnum Hand) Eq
--             ]
--    -> Show the SetID if the Revealed field flag is set to True AND
--  in the Hand zone
--
--  [ Mask SetID [ Filter Revealed (fromEnum True) Eq
--               , Filter Zone (fromEnum Hand) Eq
--               ]
--  , Mask SetID [ Filter Zone (fromEnum Barracks) ]
--  ]
--    -> Show the SetID if (the Revealed field flag is set to True AND
--  in the Hand zone) OR if in the Barrack zone
data Mask = Mask Field (Selection Field U8)
  deriving Show
type Masks = [Mask]

-- We will compile the masks to ensure the same ordering of output of
-- fields among all parsing parties. The first element of the tuple
-- will be the masks for system owned cards, the middle for cards when
-- they are displayed to their owner and the third will be for cards
-- displayed to their opponent
newtype CompiledMasks = CMasks
  { getMasks :: (Masks, Masks, Masks)
  }

-- Apply the masks over the given FieldMap and return the resulting FieldMap
-- with the Fields that satisfy their respective filters
filterMasks :: Masks -> FieldMap -> FieldMap
filterMasks masks fm = Map.filterWithKey (filterMasks' masks fm) fm
  where
    filterMasks' :: Masks -> FieldMap -> Field -> U8 -> Bool
    filterMasks' masks fm fld _ = any (checkMask fm fld) masks

    checkMask :: FieldMap -> Field -> Mask -> Bool
    checkMask fm fld (Mask curFld selection)
      | fld /= curFld = False
      | otherwise = select' selection fm
