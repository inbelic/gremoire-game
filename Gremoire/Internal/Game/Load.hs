module Internal.Game.Load
  ( LoadInfo(..)
  , basicLoadInfo
  , loadGame
  ) where

import Core.Card
import Core.Fields
import Core.History (begin)
import Core.Logic.Rules
import Internal.Bytes
import Internal.Game.Types
import Internal.Game.Views

import qualified Data.ByteString as B (ByteString)
import qualified Data.Map as Map (fromList)

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
                         , (CardID (U8 7), dummyUnit $ U8 1)
                         , (CardID (U8 8), dummyUnit $ U8 2)
                         , (CardID (U8 9), dummyUnit $ U8 1)
                         , (CardID (U8 10), dummyUnit $ U8 2)
                         ]

basicLoadInfo :: LoadInfo
basicLoadInfo = LoadInfo basicFilterSet basicMaskSet

-- An empty filter set means we will take all cards
basicFilterSet :: CompiledWindows
basicFilterSet = CWindows [ Window Revealed $ Filtration []
                          , Window ActiveFlag $ Filtration []
                          , Window AttackFlag $ Filtration []
                          , Window Zone $ Filtration []
                          , Window Phase $ Filtration []
                          , Window Owner $ Filtration []
                          , Window Position $ Filtration []
                          ]

basicMaskSet :: CompiledMasks
basicMaskSet = CMasks
  ( [Mask Owner $ Selection [Constraint Owner Tr (U8 0)]]
  , []
  , []
  )


-- Temporary dummy cards for testing purposes
dummyHero :: Owner -> Card
dummyHero owner
  = mint
  . discardAlteration (set Owner owner)
  . discardAlteration (set Zone (enumToU8 Throne))
  $ create

dummyUnit :: Owner -> Card
dummyUnit owner
  = mint
  . discardAlteration (set Owner owner)
  . discardAlteration (set Zone (enumToU8 TopDeck)) -- Should be middeck but is topdeck for testing purposes
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [resolveUnit]
