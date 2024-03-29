module VexFlow.Abc.Beam (calculateBeams) where

-- work out the beam groups from the time signature

import Data.Abc (TimeSignature)
import Data.Abc.Meter (commonTime)
import Data.Array (concat, filter, groupBy, mapWithIndex, slice)
import Data.Array.NonEmpty (NonEmptyArray, length, head, last)
import Data.Foldable (foldl, elem)
import Data.Map (Map, empty, insert, lookup, toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.String.Utils (endsWith)
import Data.Tuple (snd)
import Prelude (class Eq, class Show, map, ($), (&&), (+), (-), (/=), (<>), (==), (>), (>=), (||))
import VexFlow.Types (BeamSpec, BeatMarker, BeatNumber, NoteSpec)

-- | How beamable is a given rest or note
data Beamability
  = Beamable -- Any note that is sufficently small to beam
  | Unbeamable -- a rest or else a note too large to beam
  | StartOnly -- a note prefaced by a 'y' typesetting space

derive instance eqBeamability :: Eq Beamability

instance showBeamability :: Show Beamability where
  show Beamable = "Beamable"
  show Unbeamable = "Unbeamable"
  show StartOnly = "StartOnly"

-- | a note or rest defined by its note index and associated with its beamability
type BeamableNote =
  { noteIndex :: Int
  , beamability :: Beamability
  }

-- | a beam range is the index of the first note in the beam plus the
-- | index of (one greater than) the last note in the beam
type BeamRange =
  { start :: Int
  , end :: Int
  }

-- | the various beams that are discovered in each beat in the bar
type BeamMap = Map BeatNumber (Array BeamRange)

-- | just an accumulator for the fold
type BeamAcc =
  { beatMarker :: BeatMarker
  , beams :: BeamMap
  }

quarterNoteTicks :: Int
quarterNoteTicks = 32

-- | Calculate the beams for the bar in question
calculateBeams
  :: TimeSignature
  -> Array NoteSpec
  -> Array BeatMarker
  -> Array Int
  -> Array BeamSpec
calculateBeams timeSignature noteSpecs beatMarkers typesettingSpaces =
  map (\r -> [ r.start, r.end ]) $
    calculateStandardBeams timeSignature noteSpecs beatMarkers typesettingSpaces

-- | The algorithm we use is to identify beat markers for successive
-- | beats and to recognize places where there are at least a couple
-- | of successive notes within a beat which can be beamed
calculateStandardBeams
  :: TimeSignature
  -> Array NoteSpec
  -> Array BeatMarker
  -> Array Int
  -> Array BeamRange
calculateStandardBeams timeSignature noteSpecs beatMarkers typesettingSpaces =
  let
    initialBM = { beatNumber: 0, noteIndex: 0 } -- implict beat 0
    result = foldl (beamFunc noteSpecs typesettingSpaces)
      { beatMarker: initialBM, beams: empty }
      beatMarkers
  in
    if (commonTime == timeSignature) then
      optimiseCommonTimeBeaming result.beams typesettingSpaces
    else
      concat $ map snd $ toUnfoldable result.beams

-- | the heart of the algorithm.
-- | find the beamability of each note, split the bar into beats and fold
-- | over each beat.  Beaming will not normally be used across beats (unless
-- | a tuplet might do so, being accommodated solely in the first beat of the two)
beamFunc :: Array NoteSpec -> Array Int -> BeamAcc -> BeatMarker -> BeamAcc
beamFunc noteSpecs typesettingSpaces acc beatMarker =
  let
    notesInBeat =
      slice acc.beatMarker.noteIndex beatMarker.noteIndex noteSpecs

    beamables :: Array BeamableNote
    beamables =
      mapWithIndex (beamableNote typesettingSpaces acc.beatMarker.noteIndex) notesInBeat

    beamRanges :: Array BeamRange
    beamRanges = getBeamRanges beamables
  in
    { beatMarker: beatMarker
    , beams: insert beatMarker.beatNumber beamRanges acc.beams
    }

-- calculate the beamability of a note or rest
beamableNote :: Array Int -> Int -> Int -> NoteSpec -> BeamableNote
beamableNote typesettingSpaces offset idx noteSpec =
  let
    noteIndex = offset + idx
    beamability =
      if
        ( noteSpec.noteTicks >= quarterNoteTicks
            || (endsWith "r" noteSpec.vexNote.duration)
        ) then
        Unbeamable
      else if (elem noteIndex typesettingSpaces) then
        StartOnly
      else
        Beamable
  in
    { noteIndex
    , beamability
    }

-- | group the beamable notes together and reject any groups containing
-- | only 1 member (you can't beam a singleton)
groupBeamableNotes :: Array BeamableNote -> Array (NonEmptyArray BeamableNote)
groupBeamableNotes bns =
  let
    f :: BeamableNote -> BeamableNote -> Boolean
    f a b =
      (a.beamability /= Unbeamable) && (b.beamability == Beamable)
  in
    filter (\g -> length g > 1) $ groupBy f bns

-- | get the beam ranges from the beamable groups
getBeamRanges :: Array BeamableNote -> Array BeamRange
getBeamRanges bns =
  map createBeamRange $ groupBeamableNotes bns

  where
  createBeamRange :: NonEmptyArray BeamableNote -> BeamRange
  createBeamRange bg =
    let
      start = head bg
      end = last bg
    in
      { start: start.noteIndex, end: end.noteIndex + 1 }

-- | Optimisations in common time which, wherever possible, allow
-- | the beams for the first two beats in the bar to be coalesced
-- | and ditto the last two beats in the bar
optimiseCommonTimeBeaming :: BeamMap -> Array Int -> Array BeamRange
optimiseCommonTimeBeaming bm typesettingSpaces =
  coalesce (lookupRanges 1 bm) (lookupRanges 2 bm) typesettingSpaces
    <>
      coalesce (lookupRanges 3 bm) (lookupRanges 4 bm) typesettingSpaces

-- | Coalesce the beaming for two adjacent beats in the 4/4 bar.
-- | The heuristic is that we coalesce only if each individual beam range is
-- | a singleton with only 2 notes with no separating typesetting space.
-- | (i.e. range 2 doesn't specify a separation)
coalesce :: Array BeamRange -> Array BeamRange -> Array Int -> Array BeamRange
coalesce [ r1 ] [ r2 ] typesettingSpaces =
  if (elem r2.start typesettingSpaces) || (anUncoalesceableRange r1 r2) then
    [ r1 ] <> [ r2 ]
  else
    -- this is the only permitted coalesce
    [ { start: r1.start, end: r2.end } ]
coalesce x y _ = x <> y

lookupRanges :: Int -> BeamMap -> Array BeamRange
lookupRanges idx bm =
  fromMaybe [] $ lookup idx bm

-- take a pair of beam ranges that we might intend to coalesce
-- and return uncoalexceable if either one fails to have 2 notes in the beam
anUncoalesceableRange :: BeamRange -> BeamRange -> Boolean
anUncoalesceableRange r1 r2 =
  r1.end - r1.start /= 2 || r2.end - r2.start /= 2
