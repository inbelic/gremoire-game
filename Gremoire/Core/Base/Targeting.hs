module Core.Base.Targeting where

import Core.CardState
import Core.GameState
import Core.Fields
import Core.Cut

import Internal.Bytes
import Internal.Game.Types

instance Semigroup Targeting where
  (<>) (Targeting t1) (Targeting t2)
    = Targeting $ \cID gs -> t1 cID gs ++ t2 cID gs

-- Module to define various common helpers for targeting
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
          [] -> [(tID, Given . CardID $ 0)]
          xs -> [(tID, ) . Inquire . (:) (CardID 0) $ xs]

targetSelf :: TargetID -> Targeting
targetSelf tID = Targeting $ \cID _ -> [(tID, Given cID)]

targetRuleCard :: TargetID -> Targeting
targetRuleCard tID = Targeting $ \_ _ -> [(tID, Given ruleCardID)]
