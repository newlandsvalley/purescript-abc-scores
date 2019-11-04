module VexFlow.Abc.Beat (beatDuration, exactBeatNumber) where

-- | We need to work out where the beat lies before we can
-- | work out explicit beaming
-- | Beat Numbers are of use in determining the span of notes that exist
-- | within a beat. e.g. in common time, they will be labelled 1,2,3 and 4
-- | (in fact what is recorded for the first beat is 0 as it starts and 1 as
-- | it ends, but the end beat mark is of value).

import Prelude ((/), ($), (==))
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
      (1 % 2)
    { numerator: 6, denominator: 8 } ->
      (3 % 8)
    { numerator: 9, denominator: 8 } ->
      (3 % 8)
    { numerator: 12, denominator: 8 } ->
      (3 % 8)
    _ ->
     (1 % ts.denominator)

-- | Return the beat number if we're at an exact beat multiple after beat 0
-- | (which is always implicitly present_).
-- | phraseDur is the sum of the duration of the notes so far seen
-- | beatDur is the duration of a beat (within the time signature):
exactBeatNumber :: NoteDuration -> NoteDuration -> Int -> Maybe BeatMarker
exactBeatNumber phraseDur beatDur noteIndex =
  let
    beats = phraseDur / beatDur
  in
    if (noteIndex == 0) then
      Nothing
    else
      case (denominator beats) of
        1 ->
          Just $ { beatNumber: numerator beats
                 , noteIndex : noteIndex
                 }
        _ ->
          Nothing
