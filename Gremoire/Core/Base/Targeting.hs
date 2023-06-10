module Core.Base.Targeting where

import Core.CardState
import Core.GameState
import Core.Fields
import Core.Cut

import Core.Logic.Battle

import Internal.Bytes
import Internal.Game.Types

-- Module to define various common helpers for targeting

instance Semigroup Targeting where
  (<>) (Targeting t1) (Targeting t2)
    = Targeting $ \cID gs -> t1 cID gs ++ t2 cID gs


targetSelf :: TargetID -> Targeting
targetSelf tID = Targeting $ \cID _ -> [(tID, Given cID)]

targetRuleCard :: TargetID -> Targeting
targetRuleCard tID = Targeting $ \_ _ -> [(tID, Given ruleCardID)]


toDraw :: TargetID -> Owner -> Targeting
toDraw tID owner = Targeting $
  \cID gs -> let cs = getCS gs
                 owned = refine Owner Eq owner cs
                 topDeck = within $ refine Zone Eq (enumToU8 TopDeck) owned
                 midDeck = within $ refine Zone Eq (enumToU8 MidDeck) owned
                 botDeck = within $ refine Zone Eq (enumToU8 BotDeck) owned
              in case (topDeck, midDeck, botDeck) of
                   ([], [], []) -> [] -- No card to draw
                   ([], [], xs) -> [(tID, Given $ head xs)]
                   ([], xs, _) -> [(tID, Random xs)]
                   (xs, _, _) -> [(tID, Given $ head xs)]

bothDraw :: TargetID -> Targeting
bothDraw tID = toDraw tID (U8 1) <> toDraw tID (U8 2)

rulesCard :: TargetID -> Targeting
rulesCard tID = Targeting $ \_ _ -> [(tID, Given ruleCardID)]

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

-- A valid nomination is a card that is in the Owners Barrack zone and is not
-- already nominate. We use a bit of a hack here to target the owner if
-- they are done nominating
validNomins :: TargetID -> Owner -> Targeting
validNomins tID owner = Targeting $
  \_ gs ->
    let nomins = within
               . refine Nominated Eq (enumToU8 False)
               . refine Owner Eq owner
               . refine Zone Eq (enumToU8 Barrack)
               . getCS $ gs
     in case nomins of
          [] -> [(tID, Given (CardID owner))]
          xs -> [(tID, ) . Inquire . (:) (CardID owner) $ xs]

allZone :: TargetID -> Zone -> Targeting
allZone tID zn = Targeting $
  \_ gs -> map ((tID, ) . Given)
         . within . refine Zone Eq (enumToU8 zn)
         $ getCS gs

detStrikeTarget :: TargetID -> Targeting
detStrikeTarget tID = Targeting $
  \cID gs -> case computeStrike cID $ getCS gs of
               Nothing -> []
               (Just tcID) -> [(tID, Given tcID)]
