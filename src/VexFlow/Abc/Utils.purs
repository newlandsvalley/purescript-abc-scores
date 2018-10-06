module VexFlow.Abc.Utils (beatsPerBeam, dotCount, normaliseBroken, noteDotCount, noteTicks) where

import Prelude (($), (*), (+), (-))
import Data.Int (round)
import Data.Rational (fromInt, toNumber)
import Data.Tuple (Tuple(..))
import Data.Abc (AbcNote, Broken(..), MeterSignature, NoteDuration)
import Data.Abc.Metadata (dotFactor)
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

noteDotCount :: AbcContext -> AbcNote -> Int
noteDotCount ctx abcNote =
  dotCount ctx abcNote.duration

-- | note duration in ticks - 1 beat split into 128 possible unit ticks
noteTicks :: AbcContext -> NoteDuration -> Int
noteTicks ctx d =
  round $ toNumber $
     ctx.unitNoteLength * d * (fromInt 128)

-- | apply the specified broken rhythm to each note in the note pair (presented individually)
-- | and return the broken note pair presented conventionally
normaliseBroken :: Broken -> AbcNote -> AbcNote -> (Tuple AbcNote AbcNote )
normaliseBroken broken n1 n2 =
  let
    down i =
      (fromInt 1) - (dotFactor i)

    up i =
      (fromInt 1) + (dotFactor i)
  in
    case broken of
      LeftArrow i ->
        let
          left =
            n1 { duration = n1.duration * (down i) }

          right =
            n2 { duration = n2.duration * (up i) }
        in
          (Tuple left right )

      RightArrow i ->
        let
          left =
            n1 { duration = n1.duration * (up i) }

          right =
            n2 { duration = n2.duration * (down i) }
        in
          (Tuple left right )
