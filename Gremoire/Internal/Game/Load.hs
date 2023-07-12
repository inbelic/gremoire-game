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
  ( [Mask Owner $ Selection [Constraint Owner Tr (U8 0)]]
  , []
  , []
  )
