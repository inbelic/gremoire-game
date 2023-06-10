module Core.Logic.Rules where

import Core.Card
import Core.CardState
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
  . discardAlteration (set Nominated (U8 0))
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [ nightPhase players
               , unsetNominate players
               , setNominate players
               , setActive
               , morningPhase, incrementPhase]

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

-- When we enter a play phase we will set the ActiveFlag to denote that a player
-- is completing some actions and so we will not proceed the phases
setActive :: Ability
setActive = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = oneOf (enteredPhase Seige) (enteredPhase Retaliate)

    grd = alwaysOk

    trgts = targetRuleCard (TargetID 0)
    rslvs = Map.fromList [(TargetID 0, justChange setActiveFlag)]

-- When we enter the nominate phase we will set the ActiveFlag to true
-- and the Nominated flag of both players to be False.
-- To stop the phase increment until the active flag is unset in unsetNominate
--
-- Both heros are both automatically nominated into the skirmish
setNominate :: [Owner] -> Ability
setNominate owners = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = enteredPhase Nominate

    grd = alwaysOk

    trgts = targetRuleCard (TargetID 0)
          <> Targeting (\_ _ -> map ((TargetID 1, ) . Given . CardID) owners)
          <> Targeting (\_ gs ->
            let heros = map (`getHero` getCS gs) owners
             in map ((TargetID 2, ) . Given) heros)

    rslvs = Map.fromList [ (TargetID 0, justChange setActiveFlag)
                         , (TargetID 1, justChange $ set Nominated (enumToU8 False))
                         , (TargetID 2, justChange $ set Nominated (enumToU8 True))
                         ]

-- We can end the nominate phase when both players have denoted they are done
-- nominating by setting their player rule card Nominate flag to True
unsetNominate :: [Owner] -> Ability
unsetNominate owners = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (checkTrg $ assertEq ruleCardID ActiveFlag (enumToU8 True) . getCS)
        . both (inPhase Nominate)
        $ playersDoneNoms owners

    grd = alwaysOk
    
    trgts = targetRuleCard (TargetID 0)
    rslvs = Map.fromList [(TargetID 0, justChange unsetActiveFlag)]

-- In the night phase we move back all the units from the battlefield
-- to their barracks and then record the new attacker
nightPhase :: [Owner] -> Ability
nightPhase owners = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = enteredPhase Night

    grd = alwaysOk

    trgts = allZone (TargetID 0) Battlefield
    rslvs = Map.fromList [ (TargetID 0, moveZone Barrack) ]

-------------------------------------------------
  -- PLAYER CARD Rules
-------------------------------------------------
players :: [Owner]
players = [U8 1, U8 2]

playerRuleCard :: Owner -> Card
playerRuleCard owner
  = discardAlteration (set Owner owner)
  . discardAlteration (set Nominated $ U8 0)
  . foldr (discardAlteration . equip) create
  $ abltys
    where
      abltys = [nominatePhase owner, play owner]

-- In the play phases, we will check for the active player (if any) in the
-- triggers when there are no other abilities or trigger on the stack and
-- resolving. The owner will return either the rulescard if they wish to
-- not play anymore cards or a card in their hand. Then we move the card
-- from the Hand onto the Stack
play :: Owner -> Ability
play owner = Ability Nothing OnResolve trg grd rslvs trgts
  where
    trg = both (checkTrg $ assertEq ruleCardID ActiveFlag (enumToU8 True) . getCS)
        . both (checkTrg $ isActive owner)
        . both (checkTrg isStackEmpty)
        . both (checkTrg noAbilitiesTriggering)
        $ inPlayPhase

    inPlayPhase = oneOf (inPhase Seige) (inPhase Retaliate)

    grd = oneOf isRulesCard (inZone Hand)
    trgts = validPlays (TargetID 0) owner
    rslvs = Map.fromList [(TargetID 0, orChangeRuleCard unsetActiveFlag
                                        $ moveZone Stack)]


-- In the nominate phase, the players will mark which of their units will be
-- used in the skirmish phase
nominatePhase :: Owner -> Ability
nominatePhase owner = Ability Nothing OnResolve trg grd rslvs trgts
  where
    trg = both (checkTrg $ assertEq ruleCardID ActiveFlag (enumToU8 True) . getCS)
        . both (checkTrg $ assertEq (CardID owner) Nominated (enumToU8 False) . getCS)
        . both (checkTrg isStackEmpty)
        . both (checkTrg noAbilitiesTriggering)
        $ inPhase Nominate

    grd = oneOf (isPlayerCard owner) (inZone Barrack)
    trgts = validNomins (TargetID 0) owner
    rslvs = Map.fromList [(TargetID 0, justChange . set Nominated $ enumToU8 True)]

-------------------------------------------------
  -- HERO CARD Rules
-------------------------------------------------
-- All heros are nominated during
conscriptHero :: Ability
conscriptHero = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (enteredPhase Formation)
        . both (Trigger nominated)
        $ Trigger (\cID -> assertEq cID Zone (enumToU8 Throne) . getCS)

    grd = both (inZone Throne) $ Guard (const nominated)

    nominated :: CardID -> GameState -> Bool
    nominated cID = assertEq cID Nominated (enumToU8 True) . getCS

    rslvs = Map.fromList [(TargetID 0, justChange unsetNominated
                                        <> moveZone Battlefield)]

    unsetNominated :: Change
    unsetNominated = set Nominated (enumToU8 False)

    trgts = targetSelf (TargetID 0)

-- After a skirmish the hero is returned to the barraks where they are then
-- retreated to the throne
retreatHero :: Ability
retreatHero = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = selfEntered Barrack
    grd = inZone Barrack

    rslvs = Map.fromList [(TargetID 0, moveZone Throne)]
    trgts = targetSelf (TargetID 0)


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

-- All units that have been nominated will have this trigger at the same time
-- so then the order of the triggers will determine which unit is in which
-- position
conscriptUnit :: Ability
conscriptUnit = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (enteredPhase Formation)
        . both (Trigger nominated)
        $ Trigger (\cID -> assertEq cID Zone (enumToU8 Barrack) . getCS)

    grd = both (inZone Barrack)
        $ Guard (const nominated)

    nominated :: CardID -> GameState -> Bool
    nominated cID = assertEq cID Nominated (enumToU8 True) . getCS

    rslvs = Map.fromList [(TargetID 0, justChange unsetNominated
                                        <> moveZone Battlefield)]

    unsetNominated :: Change
    unsetNominated = set Nominated (enumToU8 False)

    trgts = targetSelf (TargetID 0)

-- In the skirmish phase, each unit will compute which opponent they need to
-- strike (TODO: currently unoptimized such that the same thing is computed many
-- times)
skirmishUnit :: Ability
skirmishUnit = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (enteredPhase Skirmish)
        $ Trigger (\cID -> assertEq cID Zone (enumToU8 Battlefield) . getCS)

    grd = alwaysOk

    trgts = detStrikeTarget $ TargetID 0
    rslvs = Map.fromList [(TargetID 0, doStrike)]

assertAlive :: Ability
assertAlive = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (changedField Toughness)
        $ Trigger (\cID -> assertEq cID Toughness (U8 0) . getCS)

    grd = Guard (\_ tcID -> assertEq tcID Toughness (U8 0) . getCS)

    rslvs = Map.fromList [(TargetID 0, moveZone Cemetery)]
    trgts = targetSelf $ TargetID 0
