module VexFlow.Abc.Beam (defaultBeamGroups) where

-- work out the beam groups
-- this is not finished - at the moment we only use a default
-- singleton beam group deternined almost completely by the time signature

import Prelude (($), (==))
import Data.Array (length, singleton)
import VexFlow.Types (BeamGroup, MusicSpec(..), TimeSignature)

defaultBeamGroups :: TimeSignature -> MusicSpec -> Array BeamGroup
defaultBeamGroups timeSignature musicSpec =
  singleton $ defaultBeamGroup timeSignature musicSpec

-- | set the default grouping of notes that are beamed together
-- | according to the meter signature and MusicSpec contents
defaultBeamGroup :: TimeSignature -> MusicSpec -> BeamGroup
defaultBeamGroup timeSignature (MusicSpec spec) =
  let
    noteKind =
      timeSignature.denominator
    noteCount =
      case timeSignature.numerator of
        3 -> 1
        4 ->
          case spec.midBarNoteIndex of
            [4] ->
              if (length spec.noteSpecs == 8) then
              -- 8 notes evenly paced
                2
              else
                1
            _ ->
              1
        6 -> 3
        9 -> 3
        12 -> 3
        _ -> 1
  in
    { noteCount, noteKind }
