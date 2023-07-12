module Internal.Game.Load
  ( LoadInfo(..)
  , basicLoadInfo
  , loadGame
  ) where

import Base.Set

import Core.Card
import Core.Fields
import Core.History (begin)
import Internal.Bytes
import Internal.Game.Types
import Internal.Game.Views

import qualified Data.ByteString as B (ByteString)
import qualified Data.Map as Map (fromList, keys)

data LoadInfo = LoadInfo
  { compiledWindows   :: CompiledWindows
  , compiledMasks     :: CompiledMasks
  }

loadGame :: B.ByteString -> IO (LoadInfo, Game)
loadGame _ = return (basicLoadInfo, Game [] begin cards)
  where
    cards = Map.fromList [ (ruleCardID, ruleCard)
                         , (CardID (U8 1), playerRuleCard $ U8 1)
                         , (CardID (U8 2), playerRuleCard $ U8 2)
                         , (CardID (U8 3), dummyHero $ U8 1)
                         , (CardID (U8 4), dummyHero $ U8 2)
                         , (CardID (U8 5), dummyUnit $ U8 1)
                         , (CardID (U8 6), dummyUnit $ U8 2)
                         ]

basicLoadInfo :: LoadInfo
basicLoadInfo = LoadInfo basicFilterSet basicMaskSet

-- An empty filter set means we will take all cards
-- So the basic filter set will assume there are no dependencies and compute
-- everything
basicFilterSet :: CompiledWindows
basicFilterSet = CWindows . map (\fld -> Window fld $ Filtration [])
                          . Map.keys $ fieldTypeMap

basicMaskSet :: CompiledMasks
basicMaskSet = CMasks
  ( [always Owner, always CardNum, always SetID, always Phase]
  , [always Owner, always Zone, always CardNum, always SetID, always Position,
    always Power, always Toughness]
  , [always Owner, always Zone] ++ whenVisible CardNum ++ whenVisible SetID
  ++ whenVisible Position ++ whenVisible Power ++ whenVisible Toughness
  )

always :: Field -> Mask
always fld = Mask fld $ Selection []

-- Visible is equivalent to being in any of the following zones:
-- Throne, Stack, Barrack, Battlefield, Cemetery or Revealed
whenVisible :: Field -> [Mask]
whenVisible fld
  = (:) (Mask fld $ Selection [Constraint Revealed Eq 1])
  $ map (whenInZone fld) [Stack, Throne, Barrack, Battlefield, Cemetery]

whenInZone :: Field -> Zone -> Mask
whenInZone fld zn = Mask fld $ Selection [Constraint Zone Eq $ enumToU8 zn]
