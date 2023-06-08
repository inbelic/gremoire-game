module Core.Rules where

import Core.Card
import Core.CardState (check)
import Core.GameState
import Core.Fields

import Internal.Bytes
import Internal.Game.Types

import qualified Data.Map as Map (Map, fromList, lookup, filter, keys)

ruleCardID :: CardID
ruleCardID = CardID . U8 $ 1

baseRules :: Card
baseRules =
  discardAlteration (set Phase (enumToU8 Morning))
  . discardAlteration (set Owner (U8 0))
  . foldr (discardAlteration . equip) create
  $ abltys
  where
    abltys = [incrementPhase]

incrementPhase :: Ability
incrementPhase = Ability Nothing OnTrigger trg grd rslvs rulesCard
  where
    trg = Trigger $ \_ gs -> isStackEmpty gs && noAbilitiesTriggering gs
    grd = Guard $ \_ _ gs -> isStackEmpty gs && noAbilitiesTriggering gs
    rslv = Resolve $ \_ _ (GameState _ _ cs _) ->
            case u8ToEnum . check ruleCardID Phase (enumToU8 Night) $ cs of
              Night -> set Phase $ enumToU8 Morning
              phase -> set Phase . enumToU8 . succ $ phase

    rslvs = Map.fromList [(TargetID 0, rslv)]

    rulesCard = Targeting $ \_ _ -> [(TargetID 0, Inquire [ruleCardID])]
