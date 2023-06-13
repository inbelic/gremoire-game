module Core.Targeting
  ( Targeting(..)
  , Target(..)
  , targetSelf
  , targetRuleCard
  , rulesCard
  ) where

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

rulesCard :: TargetID -> Targeting
rulesCard tID = Targeting $ \_ _ -> [(tID, Given ruleCardID)]
