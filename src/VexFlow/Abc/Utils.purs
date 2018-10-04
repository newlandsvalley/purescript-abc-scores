module VexFlow.Abc.Utils (beatsPerBeam, dotCount, noteTicks) where

import Prelude (($), (*))
import Data.Int (round)
import Data.Rational (fromInt, toNumber)
import Data.Abc (MeterSignature, NoteDuration)
import Data.Tuple (Tuple(..))
import VexFlow.Types (AbcContext)

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

-- | the degree to which a note is dotted
-- | not complete - no account yet of double-dotted
dotCount :: AbcContext -> NoteDuration -> Int
dotCount ctx d =
  case noteTicks ctx d of
    96 ->
      1
    48 ->
      1
    24 ->
      1
    12 ->
      1
    6 ->
      1
    3 ->
      1
    _ ->
      0

-- | note duration in ticks - 1 beat split into 128 possible unit ticks
noteTicks :: AbcContext -> NoteDuration -> Int
noteTicks ctx d =
  round $ toNumber $
     ctx.unitNoteLength * d * (fromInt 128)
