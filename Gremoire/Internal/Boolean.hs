module Internal.Boolean where

import Internal.Game.Types

-- Unfortunately (and, or) is taken
class Boolean a where
    both :: a -> a -> a
    oneOf :: a -> a -> a

instance Boolean Bool where
    both = (&&)
    oneOf = (||)

instance Boolean Guard where
  both (Guard grd1) (Guard grd2)
    = Guard $ \cID tcID gs -> grd1 cID tcID gs
                           && grd2 cID tcID gs
  oneOf (Guard grd1) (Guard grd2)
    = Guard $ \cID tcID gs -> grd1 cID tcID gs
                           || grd2 cID tcID gs

instance Boolean Trigger where
  both (Trigger trg1) (Trigger trg2)
    = Trigger $ \cID gs -> trg1 cID gs
                           && trg2 cID gs
  oneOf (Trigger trg1) (Trigger trg2)
    = Trigger $ \cID gs -> trg1 cID gs
                           || trg2 cID gs
