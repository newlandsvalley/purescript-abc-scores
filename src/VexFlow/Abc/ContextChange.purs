module VexFlow.Abc.ContextChange where

-- | Experimental
-- | a context change from an inline header
-- | restricted to just those headers that actually make a difference

import Data.Abc (ModifiedKeySignature, MeterSignature, NoteDuration)
import Prelude (class Semigroup, class Monoid, mempty)


data ContextChange =
    Meter MeterSignature
  | Key ModifiedKeySignature
  | UnitNote NoteDuration
  | NoChange

-- | we'll support only one context change per bar
-- | (at the beginning of the bar) which covers almost all cases
instance contextChangeSemigroup :: Semigroup ContextChange where
  append NoChange x = x
  append x NoChange = x
  append x _ = x

instance contextChangeMonoid :: Monoid ContextChange where
  mempty = NoChange
