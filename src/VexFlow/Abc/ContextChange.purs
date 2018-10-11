module VexFlow.Abc.ContextChange where

-- | Experimental
-- | a context change from an inline header
-- | restricted to just those headers that actually make a difference
-- | We'll assume, for simplification, that these can only be aplied at
-- | the start of a bar, which is surely true for just about every
-- | practical case

import Data.Abc (ModifiedKeySignature, MeterSignature, NoteDuration)

data ContextChange =
    MeterChange MeterSignature
  | KeyChange ModifiedKeySignature
  | UnitNoteChange NoteDuration
