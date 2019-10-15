module VexFlow.Abc.Beam (calculateBeams) where

-- work out the beam groups from the time signature

import Prelude (class Eq, class Show, ($), (==), (+), (>), (<), (<>), (&&), (/=), map, not)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Map (Map, empty, insert, lookup, toUnfoldable)
import Data.Tuple (snd)
import Data.String.Utils (endsWith)
import Data.Array.NonEmpty (NonEmptyArray, length, head, last)
import Data.Array (concat, filter, groupBy, mapWithIndex, slice)
import Data.Array (length) as A
import VexFlow.Types (BeamSpec, BeatMarker, BeatNumber, NoteSpec, TimeSignature)

-- | How beamable is a given rest or note
data Beamability =
    Beamable           -- Any note that is sufficently small to beam
  | Unbeamable         -- a rest or else a note too large to beam
  | StartOnly          -- a note prefaced by a 'y' typesetting space

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
calculateBeams ::
     TimeSignature
  -> Array NoteSpec
  -> Array BeatMarker
  -> Array BeamSpec
calculateBeams timeSignature noteSpecs beatMarkers =
  map (\r -> [r.start, r.end]) $
      fullOrPartialBarBeams timeSignature noteSpecs beatMarkers

-- | decide if this is a full bar or a lead-in bar and find the beam ranges
fullOrPartialBarBeams ::
  TimeSignature -> Array NoteSpec -> Array BeatMarker -> Array BeamRange
fullOrPartialBarBeams timeSignature noteSpecs beatMarkers =
   if (A.length beatMarkers > 1) then
     calculateStandardBeams timeSignature noteSpecs beatMarkers
   else
     calculateLeadinBeams noteSpecs

-- | The algorithm we use is to identify beat markers for successive
-- | beats and to recognize places where there are at least a couple
-- | of successive notes within a beat which can be beamed
calculateStandardBeams ::
  TimeSignature -> Array NoteSpec -> Array BeatMarker -> Array BeamRange
calculateStandardBeams timeSignature noteSpecs beatMarkers =
  let
    initialBM = { beatNumber : 0, noteIndex: 0 }
    result = foldl (beamFunc noteSpecs)
               { beatMarker: initialBM, beams: empty }
               beatMarkers
  in
    if (commonTime == timeSignature) then
      optimiseCommonTimeBeaming result.beams
    else
      concat $ map snd $ toUnfoldable result.beams

-- | the heart of the algorithm.
-- | find the beamability of each note, split the bar into beats and fold
-- | over each beat.  Beaming will not normalle be used across beats (unless
-- | a tuplet might do so, being accommodated solely in the first beat of the two)
beamFunc :: Array NoteSpec -> BeamAcc -> BeatMarker -> BeamAcc
beamFunc noteSpecs acc beatMarker =
  let
    notesInBeat =
      slice acc.beatMarker.noteIndex beatMarker.noteIndex noteSpecs
    beamables :: Array BeamableNote
    beamables =
      mapWithIndex (beamableNote acc.beatMarker.noteIndex) notesInBeat
    beamRanges :: Array BeamRange
    beamRanges = getBeamRanges beamables
  in
     { beatMarker : beatMarker
     , beams : insert beatMarker.beatNumber beamRanges acc.beams
     }

-- calculate the beamability of a note or rest
-- (JMW!!! we need later to accommodate typesetting spaces)
beamableNote ::  Int -> Int -> NoteSpec -> BeamableNote
beamableNote offset idx noteSpec =
  let
    beamability =
      if (noteSpec.noteTicks < quarterNoteTicks
          && (not $ endsWith "r" noteSpec.vexNote.duration)) then
        Beamable
      else
        Unbeamable
  in
    { noteIndex : (offset + idx)
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
        { start : start.noteIndex, end : end.noteIndex + 1}

-- | Optimisations in common time which, wherever possible, allow
-- | the beams for the first two beats in the bar to be coalesced
-- | and ditto the last two beats in the bar
optimiseCommonTimeBeaming :: BeamMap -> Array BeamRange
optimiseCommonTimeBeaming bm =
  coalesce (lookupRanges 1 bm) (lookupRanges 2 bm)
  <>
  coalesce (lookupRanges 3 bm) (lookupRanges 4 bm)

-- | coalesce the beaming for two adjacent beats in the bar
-- | the heuristic is that we coalesce only if there is a single
-- | beam range defined
coalesce :: Array BeamRange -> Array BeamRange -> Array BeamRange
coalesce [ r1 ] [ r2 ] = [{ start: r1.start, end: r2.end }]
coalesce x y = x <> y

commonTime :: TimeSignature
commonTime =
  { numerator : 4
  , denominator : 4
  }

-- | calculate the beaming for any short lead-in bar (i.e. a bar with no first
-- | #beat with more than 1 note making it potentially beamable).
-- | we implement this by artificially setting up the assumption it has just
-- | one beat abd then calling the beamFunc that we use in full bar folds
calculateLeadinBeams :: Array NoteSpec -> Array BeamRange
calculateLeadinBeams ns =
  let
    len = A.length ns
  in
    if (1 < len) then
      let
        initialBM = { beatNumber : 0, noteIndex: 0 }
        finalBM = { beatNumber : 1, noteIndex: len }
        acc = { beatMarker: initialBM, beams: empty }
        beamAcc = beamFunc ns acc finalBM
      in
        lookupRanges 1 beamAcc.beams
    else
      -- only a single note lead-in hence unbeamable
      []

lookupRanges :: Int -> BeamMap -> Array BeamRange
lookupRanges idx bm =
  fromMaybe [] $ lookup idx bm
