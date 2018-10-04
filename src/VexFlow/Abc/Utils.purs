module VexFlow.Abc.Utils (beatsPerBeam) where

import Data.Abc (MeterSignature)
import Data.Tuple (Tuple(..))

-- | set the defaullt grouping of notes that are beamed together
-- | according to the meter signature
beatsPerBeam :: MeterSignature -> Int
beatsPerBeam (Tuple n d) =
  case n of
    3 -> 1
    4 -> 2
    6 -> 3
    9 -> 3
    12 -> 3
    _ -> 1
