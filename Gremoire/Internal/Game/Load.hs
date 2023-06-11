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
                         , (CardID (U8 7), dummyUnit $ U8 1)
                         , (CardID (U8 8), dummyUnit $ U8 2)
                         , (CardID (U8 9), dummyUnit $ U8 1)
                         , (CardID (U8 10), dummyUnit $ U8 2)
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


-- Temporary dummy cards for testing purposes
dummyHero :: Owner -> Card
dummyHero owner
  = mint
  . discardAlteration (set Power (U8 0))
  . discardAlteration (set Toughness (U8 5))
  . discardAlteration (set Owner owner)
  . discardAlteration (set Zone (enumToU8 Throne))
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [retreatHero, conscriptHero]

dummyUnit :: Owner -> Card
dummyUnit owner
  = mint
  . discardAlteration (set Power (U8 1))
  . discardAlteration (set Toughness (U8 2))
  . discardAlteration (set Owner owner)
  . discardAlteration (set Nominated (U8 0))
  . discardAlteration (set Zone (enumToU8 MidDeck))
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [assertAlive, skirmishUnit, conscriptUnit, resolveUnit]
