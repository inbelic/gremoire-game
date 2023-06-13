module Base.Set where

import Core.Core

import Base.Logic.Battle

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

    rslvs = fromList [(TargetID 0, rslv)]
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

    rslvs = fromList [(TargetID 0, moveZone Hand)]

    trgts = bothDraw $ TargetID 0

-- When we enter a play phase we will set the ActiveFlag to denote that a player
-- is completing some actions and so we will not proceed the phases
setActive :: Ability
setActive = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = oneOf (enteredPhase Seige) (enteredPhase Retaliate)

    grd = alwaysOk

    trgts = targetRuleCard (TargetID 0)
    rslvs = fromList [(TargetID 0, justChange setActiveFlag)]

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

    rslvs = fromList [ (TargetID 0, justChange setActiveFlag)
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
    rslvs = fromList [(TargetID 0, justChange unsetActiveFlag)]

-- In the night phase we move back all the units from the battlefield
-- to their barracks and then record the new attacker
nightPhase :: [Owner] -> Ability
nightPhase owners = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = enteredPhase Night

    grd = alwaysOk

    trgts = allZone (TargetID 0) Battlefield
    rslvs = fromList [ (TargetID 0, moveZone Barrack) ]

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
    rslvs = fromList [(TargetID 0, orChangeRuleCard unsetActiveFlag
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
    rslvs = fromList [(TargetID 0, justChange . set Nominated $ enumToU8 True)]

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

    rslvs = fromList [(TargetID 0, justChange unsetNominated
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

    rslvs = fromList [(TargetID 0, moveZone Throne)]
    trgts = targetSelf (TargetID 0)


-------------------------------------------------
  -- UNIT CARD Rules
-------------------------------------------------
resolveUnit :: Ability
resolveUnit = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = selfEntered Stack
    grd = inZone Stack

    rslvs = fromList [(TargetID 0, moveZone Barrack)]
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

    rslvs = fromList [(TargetID 0, justChange unsetNominated
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
    rslvs = fromList [(TargetID 0, doStrike)]

assertAlive :: Ability
assertAlive = Ability Nothing OnTrigger trg grd rslvs trgts
  where
    trg = both (changedField Toughness)
        $ Trigger (\cID -> assertEq cID Toughness (U8 0) . getCS)

    grd = Guard (\_ tcID -> assertEq tcID Toughness (U8 0) . getCS)

    rslvs = fromList [(TargetID 0, moveZone Cemetery)]
    trgts = targetSelf $ TargetID 0


-----------------------------------------------
  -- Helpers
---------------------------------------------

-- Return all the cards that are in the zone
getZone :: Zone -> CardState -> [CardID]
getZone zone = within . refine Zone Eq (enumToU8 zone)

-- Return the hero card of the respective hero (error on 0)
getHero :: Owner -> CardState -> CardID
getHero owner
  | u8 owner == 0 = undefined
  | otherwise = head . getZone Throne . refine Owner Eq owner

isPlayerCard :: Owner -> Guard
isPlayerCard owner = Guard $ \_ tcID _ -> cardID tcID == owner

-- A player (owner) is active when the flag is equal to them and it is the
-- Seige phase or when the flag is not equal to them and it is the Retaliate
-- phase
isActive :: Owner -> GameState -> Bool
isActive owner gs
  | flag == owner && p == Seige     = True
  | flag /= owner && p == Retaliate = True
  | otherwise                       = False
  where
    flag = (+ 1) . u8ToEnum . check ruleCardID AttackFlag (U8 0) $ getCS gs
    p = u8ToEnum . check ruleCardID Phase (U8 0) $ getCS gs


setActiveFlag :: Change
setActiveFlag = set ActiveFlag (enumToU8 True)

unsetActiveFlag :: Change
unsetActiveFlag = set ActiveFlag (enumToU8 False)

toDraw :: TargetID -> Owner -> Targeting
toDraw tID owner = Targeting $
  \cID gs -> let cs = getCS gs
                 owned = refine Owner Eq owner cs
                 topDeck = within $ refine Zone Eq (enumToU8 TopDeck) owned
                 midDeck = within $ refine Zone Eq (enumToU8 MidDeck) owned
                 botDeck = within $ refine Zone Eq (enumToU8 BotDeck) owned
              in case (topDeck, midDeck, botDeck) of
                   ([], [], []) -> [] -- No card to draw
                   ([], [], x:xs) -> [(tID, Given x)]
                   ([], xs, _) -> [(tID, Random xs)]
                   (x:xs, _, _) -> [(tID, Given x)]

bothDraw :: TargetID -> Targeting
bothDraw tID = toDraw tID (U8 1) <> toDraw tID (U8 2)

-- A valid play is a card that is in the Owners Hand zone
validPlays :: TargetID -> Owner -> Targeting
validPlays tID owner = Targeting $
  \_ gs ->
    let hand = within
             . refine Owner Eq owner
             . refine Zone Eq (enumToU8 Hand)
             . getCS $ gs
     in case hand of
          [] -> [(tID, Given ruleCardID)]
          xs -> [(tID, ) . Inquire . (:) ruleCardID $ xs]

allZone :: TargetID -> Zone -> Targeting
allZone tID zn = Targeting $
  \_ gs -> map ((tID, ) . Given)
         . within . refine Zone Eq (enumToU8 zn)
         $ getCS gs

-- Determine if the game has just entered a given phase
enteredPhase :: Phase -> Trigger
enteredPhase p = checkTrg $ any f . current . getHistory
  where
    f (Event _ tcID (Set Phase cp))
      | ruleCardID == tcID = cp == enumToU8 p
      | otherwise = False
    f _ = False

-- Determine if the game is currently in the given Phase
inPhase :: Phase -> Trigger
inPhase p = checkTrg $ assertEq ruleCardID Phase (enumToU8 p) . getCS
