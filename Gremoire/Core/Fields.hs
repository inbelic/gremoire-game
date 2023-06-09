module Core.Fields where

import Internal.Bytes (U8)

import qualified Data.Map as Map (Map, fromList, lookup, filter, keys)

data Field
  -- Boolean flags
  = Revealed | ActiveFlag | AttackFlag | Nominated
  -- Enums
  | Zone | Phase
  -- U8s
  | SetID | CardNum | Owner | Position | Power | Toughness
  deriving (Eq, Ord, Show, Enum, Bounded)

type FieldMap = Map.Map Field U8

-- Various Boolean flags for a card
type Revealed = Bool
type ActiveFlag = Bool
type Nominated = Bool

-- Various Enum values for a card
data Zone = Hand | TopDeck | MidDeck | BotDeck
          | Stack | Throne | Barrack | Battlefield
          | Cemetery
  deriving (Eq, Ord, Enum, Show)

data Phase = Morning | Seige | Retaliate | Nominate
           | Formation | Skirmish | Night
  deriving (Eq, Ord, Enum, Show)

-- Various Integer values for a card
type SetID = U8
type CardNum = U8
type Owner = U8
type Position = U8
type Power = U8
type Toughness = U8

type NumEnums = Int
data FieldType
  = FlagType
  | EnumType NumEnums
  | IntType
  deriving Eq

-- Map the Fields to their corresponding type for the sake of documentation
-- and testing
fieldTypeMap :: Map.Map Field FieldType
fieldTypeMap = Map.fromList
  [ (Revealed, FlagType)  -- Boolean flags as either set (True) or not (False)
  , (ActiveFlag, FlagType)
  , (AttackFlag, FlagType)
  , (Nominated, FlagType)
  ----------------------------------------
  , (Zone, EnumType 9)    -- Enums in the range [0, NumEnums - 1]
  , (Phase, EnumType 7)
  ----------------------------------------
  , (SetID, IntType)      -- Integers are in the set [0, 255]
  , (CardNum, IntType)
  , (Owner, IntType)
  , (Position, IntType)
  , (Power, IntType)
  , (Toughness, IntType)
  ]

fieldToType :: Field -> FieldType
fieldToType field
  = case Map.lookup field fieldTypeMap of
      Nothing -> undefined  -- All Fields MUST have their type specified
      (Just x) -> x

-- Subsect the Fields into lists of their respective types
flagFields :: [Field]
flagFields = Map.keys . Map.filter f $ fieldTypeMap
  where
    f FlagType = True
    f _ = False

enumFields :: [Field]
enumFields = Map.keys . Map.filter f $ fieldTypeMap
  where
    f (EnumType _) = True
    f _ = False

intFields :: [Field]
intFields = Map.keys . Map.filter f $ fieldTypeMap
  where
    f IntType = True
    f _ = False
