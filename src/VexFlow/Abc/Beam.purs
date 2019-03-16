module VexFlow.Abc.Beam (calculateBeams) where

-- work out the beam groups from the time signature

import Prelude (($), (==), (-), (>), (<), (<>), (&&), (<=), (>=), map, not)
import Data.Array (any, length, slice, snoc)
import Data.Foldable (foldl)
-- import Data.Set (fromFoldable, toUnfoldable, union) as Set
import Data.Map (Map, empty, insert, lookup, toUnfoldable)
import Data.Tuple (snd)
import Data.String.Utils (endsWith)
import Data.Maybe (Maybe(..))
import VexFlow.Types (BeamSpec, BeatMarker, BeatNumber, NoteSpec,
         TimeSignature, VexTuplet)

type BeamRange =
  { start :: Int
  , end :: Int
  }

quarterNoteTicks :: Int
quarterNoteTicks = 32

-- | Calculate the beams, which come from standard beats or from tuplets
calculateBeams ::
     TimeSignature
  -> Array NoteSpec
  -> Array BeatMarker
  -> Array VexTuplet
  -> Array BeamSpec
calculateBeams timeSignature noteSpecs beatMarkers tuplets =
  if (length beatMarkers > 1) then
    -- normal bars
    map (\r -> [r.start, r.end]) $
      merge
        (calculateStandardBeams timeSignature noteSpecs beatMarkers)
        (calculateTupletBeams noteSpecs tuplets)
  else
    -- short lead-in bar
    calculateLeadinBeam noteSpecs

type BeamMap = Map BeatNumber BeamRange

type BeamAcc =
   { beatMarker :: BeatMarker
   , beams :: BeamMap
   }

beamFunc :: Array NoteSpec -> BeamAcc -> BeatMarker -> BeamAcc
beamFunc noteSpecs acc beatMarker =
  -- beat marker must be just one beat apart and there must be more than 1 note
  -- available to be beamed and also each note must be shorter than a quarter note
  if ((beatMarker.beatNumber - acc.beatMarker.beatNumber == 1)
      && (beatMarker.noteIndex - acc.beatMarker.noteIndex > 1)
      && (allBeamableNotes
           $ slice acc.beatMarker.noteIndex beatMarker.noteIndex noteSpecs)) then
    let
      newBeam :: BeamRange
      newBeam = {start: acc.beatMarker.noteIndex, end: beatMarker.noteIndex}
    in
      { beatMarker: beatMarker
      , beams: insert beatMarker.beatNumber newBeam acc.beams
      }
  else
     { beatMarker : beatMarker
     , beams : acc.beams
     }

-- check that each note in a potential beam is in fact 'beamable'
allBeamableNotes :: Array NoteSpec -> Boolean
allBeamableNotes noteSpecs =
  let
    f :: Boolean -> NoteSpec -> Boolean
    f acc ns =
      acc && isBeamableNote ns
  in
    foldl f true noteSpecs

-- a note is 'beamable' if in fact it really is a note (not a rest) and
-- if it's smaller than a quarter note
isBeamableNote :: NoteSpec -> Boolean
isBeamableNote noteSpec =
  noteSpec.noteTicks < quarterNoteTicks
    && (not $ endsWith "r" noteSpec.vexNote.duration)

-- | The algorithm we use is to identify beat markers for successive
-- | beats and to allow beaming only in those instances where there are at least
-- | a couple of notes to beam.
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
    -- toUnfoldable from Map natually produces output sorted by key
      map snd $ toUnfoldable result.beams

-- | calculate the tuplet beams
calculateTupletBeams :: Array NoteSpec -> Array VexTuplet -> Array BeamRange
calculateTupletBeams noteSpecs vts =
    -- map (\vt -> [vt.startPos, vt.endPos]) vts
  let
    f :: Array BeamRange -> VexTuplet -> Array BeamRange
    f bs vt =
      if allBeamableNotes $ slice vt.startPos vt.endPos noteSpecs then
        snoc bs { start: vt.startPos, end: vt.endPos }
      else
        bs
  in
    foldl f [] vts

{-}
-- | merge the two, eliminating repeats
-- | (this does not cater for subsumption)
merge1 :: Array BeamRange -> Array BeamRange -> Array BeamRange
merge1 standardBeams tupletBeams =
  let
    standardBeamsSet = Set.fromFoldable standardBeams
    tupletBeamsSet = Set.fromFoldable tupletBeams
  in
    Set.toUnfoldable $ Set.union standardBeamsSet tupletBeamsSet
-}


-- | merge the tuplet beam ranges into the standard set
-- | ensuring that if the standard set subsumes an incoming tuplet range
-- | then the incomer is ignored.
merge :: Array BeamRange -> Array BeamRange -> Array BeamRange
merge standardBeams tupletBeams =
  let
    -- merge the new tuplet beam if it is not subsumed by what's there already
    mergeFunc sBeams tupletBeam =
      if (subsumes sBeams tupletBeam) then
        sBeams
      else
        snoc sBeams tupletBeam
  in
    foldl mergeFunc standardBeams tupletBeams

-- | Optimisations in common time which, wherever possible, allow
-- | the beams for the first two beats in the bar to be coalesced
-- | and also the last two beats in the bar
optimiseCommonTimeBeaming :: BeamMap -> Array BeamRange
optimiseCommonTimeBeaming bm =
  coalesce (lookup 1 bm) (lookup 2 bm)
  <>
  coalesce (lookup 3 bm) (lookup 4 bm)
  -- to swith off the optimisation, replace with this:
  -- (it may be too expensive to compute)
  -- map snd $ toUnfoldable bm

-- | coalesce the beaming for two adjacent beats in the bar
coalesce :: Maybe BeamRange -> Maybe BeamRange -> Array BeamRange
coalesce (Just r1) (Just r2) = [{ start: r1.start, end: r2.end }]
coalesce (Just r) _ = [r]
coalesce _ (Just r) = [r]
coalesce _ _ = []

-- | return true if the set of beam ranges subsumes the range of the incoming one
subsumes :: Array BeamRange -> BeamRange -> Boolean
subsumes as new =
  let
    -- does big envelop little ?
    envelops :: BeamRange -> BeamRange -> Boolean
    envelops little big =
      big.start <= little.start && big.end >= little.end
  in
    any (envelops new) as

-- | calculate the beaming for any short lead-in bar
-- |  (i.e. a bar with no first beat)
calculateLeadinBeam :: Array NoteSpec -> Array BeamSpec
calculateLeadinBeam ns =
  let
    len = length ns
  in
    if (1 < len && (allBeamableNotes ns)) then
      [[0, len]]
    else
      []

commonTime :: TimeSignature
commonTime =
  { numerator : 4
  , denominator : 4
  }
