module VexFlow.Abc.Beam (calculateBeams) where

-- work out the beam groups from the time signature

import Prelude (($), (==), (-), (>), (<), (&&), not)
import Data.Array (slice, snoc)
import Data.Foldable (foldl)
import Data.Set (fromFoldable, toUnfoldable, union) as Set
import Data.String.Utils (endsWith)
import VexFlow.Types (BeamSpec, BeatMarker, NoteSpec, VexTuplet)

quarterNoteTicks :: Int
quarterNoteTicks = 32


-- | Calculate the beams, which come from standard beats or from tuplets
calculateBeams :: Array NoteSpec -> Array BeatMarker -> Array VexTuplet -> Array BeamSpec
calculateBeams noteSpecs beatMarkers tuplets =
  merge
    (calculateStandardBeams noteSpecs beatMarkers)
    (calculateTupletBeams noteSpecs tuplets)

type BeamAcc =
   { beatMarker :: BeatMarker
   , beams :: Array BeamSpec
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
      , beams: snoc acc.beams newBeam
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
calculateStandardBeams :: Array NoteSpec -> Array BeatMarker -> Array BeamSpec
calculateStandardBeams noteSpecs beatMarkers =
  let
    initialBM = { beatNumber : 0, noteIndex: 0 }
    result = foldl (beamFunc noteSpecs)
               { beatMarker: initialBM, beams: []}
               beatMarkers
  in
    result.beams

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
