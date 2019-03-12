module VexFlow.Abc.Beat (beatDuration, exactBeatNumber) where

-- | We need to work out where the beat lies before we can
-- | work out explicit beaming

import Prelude ((/), ($))
import Data.Rational
import Data.Maybe (Maybe(..))
import VexFlow.Types (BeatMarker, TimeSignature)
import Data.Abc (NoteDuration)


-- | work out the beat duration from the time signature
-- | e.g.
-- | 1/4 for 2/4, 3/4, 4/4
-- | 3/8 for 6/8, 9/8, 12/8 etc.
beatDuration :: TimeSignature -> NoteDuration
beatDuration ts =
  case ts of
    { numerator: 3, denominator: 2 } ->
      (1 % 4)
    { numerator: 6, denominator: 8 } ->
      (3 % 8)
    { numerator: 9, denominator: 8 } ->
      (3 % 8)
    { numerator: 12, denominator: 8 } ->
      (3 % 8)
    _ ->
     (1 % ts.denominator)

-- | Return the beat number if we're at an exact beat multiple
-- | phraseDur is the sum of the duration of the notes so far seen
-- | beatDur is the duration of a beat (within the time signature):
exactBeatNumber :: NoteDuration -> NoteDuration -> Int -> Maybe BeatMarker
exactBeatNumber phraseDur beatDur noteIndex =
  let
    beats = phraseDur / beatDur
  in
    case (denominator beats) of
      1 ->
        Just $ { beatNumber: numerator beats
               , noteIndex : noteIndex
               }
      _ ->
        Nothing
