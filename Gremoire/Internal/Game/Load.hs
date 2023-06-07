module Internal.Game.Load
  ( LoadInfo(..)
  , basicLoadInfo
  , loadGame
  ) where

import Core.Fields
import Core.History (begin)
import Core.Rules (ruleCardID, baseRules)
import Core.Views
import Internal.Bytes (U8(..))
import Internal.Game.Types

import qualified Data.ByteString as B (ByteString)
import qualified Data.Map as Map (fromList)

data LoadInfo = LoadInfo
  { compiledWindows   :: CompiledWindows
  , compiledMasks     :: CompiledMasks
  }

loadGame :: B.ByteString -> IO (LoadInfo, Game)
loadGame _ = return (basicLoadInfo, Game [] begin cards)
  where
    cards = Map.fromList [(ruleCardID, baseRules)]

basicLoadInfo :: LoadInfo
basicLoadInfo = LoadInfo basicFilterSet basicMaskSet

-- An empty filter set means we will take all cards
basicFilterSet :: CompiledWindows
basicFilterSet = CWindows [ Window Owner $ Filtration []
                          , Window Phase $ Filtration []
                          ]

basicMaskSet :: CompiledMasks
basicMaskSet = CMasks
  ( [Mask Owner $ Selection [Constraint Owner Tr (U8 0)]]
  , []
  , []
  )
