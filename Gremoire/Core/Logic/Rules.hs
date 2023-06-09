module Core.Logic.Rules where

import Core.Card
import Core.CardState (check, assertEq)
import Core.GameState
import Core.Fields

import Core.Base.Guard
import Core.Base.Resolve
import Core.Base.Targeting
import Core.Base.Trigger

import Internal.Boolean
import Internal.Bytes
import Internal.Game.Types

import qualified Data.Map as Map (Map, fromList, lookup, filter, keys)


-------------------------------------------------
  -- RULE CARD Rules
-------------------------------------------------
ruleCard :: Card
ruleCard
  = discardAlteration (set Phase (enumToU8 Night))
  . discardAlteration (set Owner (U8 0))
  . discardAlteration (set AttackFlag (U8 0))
  . discardAlteration (set ActiveFlag (U8 0))
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [setActive, morningPhase, incrementPhase]

playerRuleCard :: Owner -> Card
playerRuleCard owner
  = discardAlteration (set Owner owner)
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [play owner]

incrementPhase :: Ability
incrementPhase = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (checkTrg isStackEmpty)
        . both (checkTrg noAbilitiesTriggering)
        $ checkTrg (assertEq ruleCardID ActiveFlag (enumToU8 False) . getCS)
    grd = both (checkGrd isStackEmpty) (checkGrd noAbilitiesTriggering)
    rslv = Resolve $ \_ _ gs ->
            case u8ToEnum . check ruleCardID Phase (enumToU8 Night)
                 . getCS $ gs of
              Night -> set Phase $ enumToU8 Morning
              phase -> set Phase . enumToU8 . succ $ phase

    rslvs = Map.fromList [(TargetID 0, rslv)]
    trgts = rulesCard (TargetID 0)

-- The morning phase will simply have both players draw a card
morningPhase :: Ability
morningPhase = Ability Nothing OnResolve trg grd rslvs trgts
  where
    trg = enteredPhase Morning

    -- Check that the targeted card is still in the deck
    grd = oneOf (inZone TopDeck)
        . oneOf (inZone MidDeck)
        $ inZone BotDeck

    rslvs = Map.fromList [(TargetID 0, moveZone Hand)]

    trgts = bothDraw $ TargetID 0

setActive :: Ability
setActive = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = oneOf (enteredPhase Seige)
        $ enteredPhase Retaliate

    grd = alwaysOk

    trgts = targetRuleCard (TargetID 0)
    rslvs = Map.fromList [(TargetID 0, Resolve $ \_ _ _ -> setActiveFlag)]

    setActiveFlag = set ActiveFlag (enumToU8 True)

-------------------------------------------------
  -- PLAYER CARD Rules
-------------------------------------------------
-- In the play phases, we will check for the active player (if any) in the
-- triggers when there are no other abilities or trigger on the stack and
-- resolving. The owner will return either the rulescard if they wish to
-- not play anymore cards or a card in their hand. Then we move the card
-- from the Hand onto the Stack
play :: Owner -> Ability
play owner = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (checkTrg $ assertEq ruleCardID ActiveFlag (enumToU8 True) . getCS)
        . both (checkTrg isStackEmpty)
        . both (checkTrg noAbilitiesTriggering)
        . both (checkTrg $ isActive owner)
        $ inPlayPhase

    inPlayPhase = oneOf (inPhase Seige) (inPhase Retaliate)

    grd = oneOf isRulesCard (inZone Hand)
    trgts = validPlays (TargetID 0) owner
    rslvs = Map.fromList [(TargetID 0, orChangeRuleCard unsetActiveFlag
                                        $ moveZone Stack)]

    unsetActiveFlag = set ActiveFlag (enumToU8 False)


-------------------------------------------------
  -- UNIT CARD Rules
-------------------------------------------------
resolveUnit :: Ability
resolveUnit = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = selfEntered Stack
    grd = inZone Stack

    rslvs = Map.fromList [(TargetID 0, moveZone Barrack)]
    trgts = targetSelf (TargetID 0)
