module VexFlow.Abc.Beam (calculateBeams, defaultBeamGroups) where

-- work out the beam groups from the time signature

import Prelude (($), (==), (-), (>), (<), (&&), map)
import Data.Array (length, slice, snoc)
import Data.Foldable (foldl)
import Data.Set (fromFoldable, toUnfoldable, union) as Set
import VexFlow.Types (BeamSpec, BeamGroups, BeatMarker, MusicSpec(..)
      , NoteSpec, TimeSignature, VexTuplet)

quarterNoteTicks :: Int
quarterNoteTicks = 32

-- | set the default grouping of notes that are beamed together
-- | according to the meter signature and MusicSpec contents
-- | with 4/4 rhythms beamed differently according to the
-- | note index of the bar midpoint
defaultBeamGroups :: TimeSignature -> MusicSpec -> BeamGroups
defaultBeamGroups timeSignature (MusicSpec spec) =
  -- special-case 3/2 time signatures.  Here, the beat occupies a half note
  -- which doesn't attract much in the way of beaming unless you split it
  -- into qquarter notes
  if (timeSignature.numerator == 3) && (timeSignature.denominator == 2) then
    [ { noteCount: 1, noteKind: 4 }]
  -- otherwise the note kind is taken directly from the signature denominator
  else
    let
      noteKind =
        timeSignature.denominator
      noteCounts =
        case timeSignature.numerator of
          3 -> [1]
          4 ->
            case spec.midBarNoteIndex of
              [4] ->
                if (length spec.noteSpecs == 8) then
                  -- | AAAA AAAA |
                  [2]
                else
                -- for example | AAAA A2AA |
                  [2,1,1]
              [x] ->
              -- for example |  A2AA AAAA |
                if (length spec.noteSpecs - x == 4) then
                  [1,1,2]
                else
                  [1]
              _ ->
                [1]
          6 -> [3]
          9 -> [3]
          12 -> [3]
          _ -> [1]
    in
      map (\noteCount -> { noteCount, noteKind }) noteCounts

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
-- i.e. no note is longer than a quarter note
allBeamableNotes :: Array NoteSpec -> Boolean
allBeamableNotes noteSpecs =
  let
    f :: Boolean -> NoteSpec -> Boolean
    f acc ns =
      acc && isBeamableNote ns
  in
    foldl f true noteSpecs

isBeamableNote :: NoteSpec -> Boolean
isBeamableNote noteSpec =
  noteSpec.noteTicks < quarterNoteTicks

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
