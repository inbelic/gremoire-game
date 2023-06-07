module Internal.Game.Cut
  ( Comp(..)
  , mkFilt
  , Contrastable(contrast)
  , Constraint(..)
  , Constraints
  , Selection(..)
  , Filtration(..)
  , Cut(..)
  , Cuts
  , trim
  , cut
  -- Internal testing exports
  , select
  , filtrate
  -- Internal Load export
  , select'
  ) where

import qualified Data.Map as Map (Map, filter, lookup)

-- Comparison related things
-- Enumeration of possible comparisons
data Comp = Eq | Neq | Ls | Gr | LsEq | GrEq | Tr | Fl
  deriving (Eq, Enum, Bounded, Show)

-- Function to convert the enumeration to the function
mkFilt :: (Ord a) => Comp -> a -> a -> Bool
mkFilt Eq    = (==)
mkFilt Neq   = (/=)
mkFilt Ls    = (<)
mkFilt Gr    = (>)
mkFilt LsEq  = (<=)
mkFilt GrEq  = (>=)
-- Catch alls
mkFilt Tr    = \_ _ -> True
mkFilt Fl    = \_ _ -> False


-- We have a constraint on the values in the mapping of integers
data Constraint k a = Constraint k Comp a
  deriving (Eq, Show)
type Constraints k a = [Constraint k a]

-- We begin by assuming all items are not included in the output mapping and
-- the items that make any constraint evaluate to True are added. Hence we
-- 'select' which cards are in the output mapping
newtype Selection k a = Selection
  { getSelection :: Constraints k a
  }
  deriving Show

select :: (Ord a, Ord k) => Selection k a -> Map.Map v (Map.Map k a)
                         -> Map.Map v (Map.Map k a)
select selection = Map.filter (select' selection)

select' :: (Ord a, Ord k) => Selection k a -> Map.Map k a -> Bool
select' (Selection selection) = (`anySatisfied` selection)


-- We begin by assuming all items will be included in the output mapping and the
-- items that make all constraints evaluate to True are removed. Hence, we
-- 'filter' out the cards that are in the output mapping
newtype Filtration k a = Filtration
  { getFiltration :: Constraints k a
  }
  deriving Show

filtrate :: (Ord a, Ord k) => Filtration k a -> Map.Map v (Map.Map k a)
                           -> Map.Map v (Map.Map k a)
filtrate (Filtration filtration)
  = Map.filter (`allSatisfied` filtration)


-- Then we can describe a cut through the feasible plane of the items as
-- either a Selection of what to keep or a Filtration of what to keep. We
-- will encapsulate this two ways to describe a subset as a Cut.
data Cut k a
  = Select (Selection k a)
  | Filtrate (Filtration k a)
  deriving Show
type Cuts k a = [Cut k a]

-- A trim will just do a single cut through the cards with the given Cut
trim :: (Ord a, Ord k) => Cut k a -> Map.Map v (Map.Map k a)
                       -> Map.Map v (Map.Map k a)
trim (Select selection) = select selection
trim (Filtrate filtration) = filtrate filtration

-- A cut is a sequence of trims
cut :: (Ord a, Ord k) => Cuts k a -> Map.Map v (Map.Map k a)
                      -> Map.Map v (Map.Map k a)
cut = flip (foldr trim)

-- Private Functions and Instance definitions
class Contrastable a where
  contrast :: a -> a -- Axiom: a === contrast . contrast $ a

-- Mapping from each comparison to the corresponding comparison that will
-- always be the opposite. Looking to use when we want to find the complimentry
-- set of a set of contraints
instance Contrastable Comp where
  contrast Eq = Neq
  contrast Neq = Eq
  contrast Ls = GrEq
  contrast GrEq = Ls
  contrast Gr = LsEq
  contrast LsEq = Gr
  contrast Tr = Fl
  contrast Fl = Tr

instance Contrastable (Constraint k a) where
  contrast (Constraint k cmp x) = Constraint k (contrast cmp) x

instance Contrastable (Cut k a) where
  contrast (Select selection)
    = Filtrate . Filtration . map contrast . getSelection $ selection
  contrast (Filtrate filtrate)
    = Select . Selection . map contrast . getFiltration $ filtrate

instance Contrastable (Cuts k a) where
  contrast = map contrast


-- Other helpers
anySatisfied :: (Ord a, Ord k) => Map.Map k a -> Constraints k a -> Bool
anySatisfied map = any (isSatisfied False map)

allSatisfied :: (Ord a, Ord k) => Map.Map k a -> Constraints k a -> Bool
allSatisfied map = all (isSatisfied True map)

isSatisfied :: (Ord a, Ord k) => Bool -> Map.Map k a -> Constraint k a -> Bool
isSatisfied def map (Constraint key cmp x)
  = case Map.lookup key map of
      Nothing -> def
      (Just val) -> mkFilt cmp val x
