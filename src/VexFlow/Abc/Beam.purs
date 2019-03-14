module VexFlow.Abc.Beam (calculateBeams) where

-- work out the beam groups from the time signature

import Prelude (($), (==), (-), (>), (<), (&&),  map, not)
import Data.Array (slice, snoc)
import Data.Foldable (foldl)
import Data.Set (fromFoldable, toUnfoldable, union) as Set
import Data.Map (Map, empty, insert, toUnfoldable)
import Data.Tuple (snd)
import Data.String.Utils (endsWith)
import VexFlow.Types (BeamSpec, BeatMarker, BeatNumber, NoteSpec,
         TimeSignature, VexTuplet)

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
  merge
    (calculateStandardBeams timeSignature noteSpecs beatMarkers)
    (calculateTupletBeams noteSpecs tuplets)

type BeamMap = Map BeatNumber BeamSpec

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
      newBeam :: Array Int
      newBeam = [acc.beatMarker.noteIndex, beatMarker.noteIndex]
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
  TimeSignature -> Array NoteSpec -> Array BeatMarker -> Array BeamSpec
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
calculateTupletBeams :: Array NoteSpec -> Array VexTuplet -> Array BeamSpec
calculateTupletBeams noteSpecs vts =
    -- map (\vt -> [vt.startPos, vt.endPos]) vts
  let
    f :: Array BeamSpec -> VexTuplet -> Array BeamSpec
    f bs vt =
      if allBeamableNotes $ slice vt.startPos vt.endPos noteSpecs then
        snoc bs [vt.startPos, vt.endPos]
      else
        bs
  in
    foldl f [] vts

-- | merge the two, eliminating repeats
merge :: Array BeamSpec -> Array BeamSpec -> Array BeamSpec
merge standardBeams tupletBeams =
  let
    standardBeamsSet = Set.fromFoldable standardBeams
    tupletBeamsSet = Set.fromFoldable tupletBeams
  in
    Set.toUnfoldable $ Set.union standardBeamsSet tupletBeamsSet

-- | this is a placeholder where we could possibly determine an efficient
-- | optimisation strategy for coalescing the beaming in common time
optimiseCommonTimeBeaming :: BeamMap -> Array BeamSpec
optimiseCommonTimeBeaming bm =
  map snd $ toUnfoldable bm

commonTime :: TimeSignature
commonTime =
  { numerator : 4
  , denominator : 4
  }
