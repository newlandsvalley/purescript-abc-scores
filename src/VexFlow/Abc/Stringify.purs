module VexFlow.Abc.Stringify (keySignature, musics) where

-- | Convert between Abc types and VexFlow which are Strings
import Data.Abc

import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (normaliseModalKey)
import Data.Either (Either(..))
import Data.String.Common (toLower)
import Data.Traversable (sequence)
import Prelude ((<>), ($), join, map, show)
import VexFlow.Abc.Utils (dotCount, noteTicks)
import VexFlow.Types (AbcContext, NoteSpec)


pitch :: PitchClass -> Accidental -> Int -> String
pitch pc acc oct =
  toLower (show  pc) <> accidental acc <> "/" <> show oct

accidental :: Accidental -> String
accidental Sharp = "#"
accidental Flat =  "b"
accidental DoubleSharp = "##"
accidental DoubleFlat = "bb"
accidental Natural = "n"
accidental Implicit = ""

-- | a key signature as a VexFlow String
-- | with 'strange' modes normalised to Major
keySignature :: KeySignature -> String
keySignature ks =
  let
    newks = case ks.mode of
      Major -> ks
      Minor -> ks
      _ -> normaliseModalKey ks
    modeStr = case newks.mode of
      Minor -> "m"
      _ -> ""
  in
    show newks.pitchClass <> (keySignatureAccidental newks.accidental) <> modeStr


-- | translate an array of ABC music (from a bar) to VexFlow notespecs
musics :: AbcContext -> Array Music -> Either String (Array NoteSpec)
musics context abcMusics =
  map join $ sequence $ map (music context) abcMusics

-- | translate any ABC music item that produces score to a VexFlow note
-- | producing an empty array if it doesn't do so
music :: AbcContext -> Music -> Either String (Array NoteSpec)
music context m =
  case m of
    Note abcNote ->
      let
        eRes = note context abcNote
      in
        map (\n -> [n]) eRes
    Rest dur ->
      let
        eRes = rest context dur
      in
        map (\n -> [n]) eRes
    _ ->
       Right []

-- | translate an array of ABC notes to VexFlow notes
notes :: AbcContext -> Array AbcNote -> Either String (Array NoteSpec)
notes context abcNotes =
  sequence $ map (note context) abcNotes

-- | translate an ABC note to a VexFlow note
-- | failing if the duration cannot be tarnslated
note :: AbcContext -> AbcNote -> Either String NoteSpec
note context abcNote =
  let
    edur = noteDur context abcNote.duration
    key = pitch abcNote.pitchClass abcNote.accidental abcNote.octave
  in
    case edur of
      Right dur ->
        let
          vexNote =
            { clef : "treble"
            , keys : [key]
            , duration : dur
            }
        in Right
          { vexNote : vexNote
          , accidentals : [accidental abcNote.accidental]
          , dots : [dotCount context abcNote.duration]
          }
      Left x -> Left x

-- | translate an ABC rest to a VexFlow note
-- | which we'll position on the B stave line
-- | failing if the duration cannot be translated
rest :: AbcContext -> AbcRest -> Either String NoteSpec
rest context abcRest =
  let
    edur = noteDur context abcRest.duration
    key = pitch B Implicit 4
  in
    case edur of
      Right dur ->
        let
          vexNote =
            { clef : "treble"
            , keys : [key]
            , duration : (dur <> "r")
            }
        in Right
          { vexNote : vexNote
          , accidentals : []
          , dots : [dotCount context abcRest.duration]
          }
      Left x -> Left x

-- | translate a note or rest duration, wrapping in a Result which indicates an
-- | unsupported duration.  This rounds values of 'short enough' note durations
-- | to the nearest supported value
noteDur :: AbcContext -> NoteDuration -> Either String String
noteDur ctx d =
  case noteTicks ctx d of
    128 ->
      Right "w"
    96 ->
      Right "hd"
    64 ->
      Right "h"
    48 ->
      Right "qd"
    32 ->
      Right "q"
    24 ->
      Right "8d"
    16 ->
      Right "8"
    12 ->
      Right "16d"
    8 ->
      Right "16"
    6 ->
      Right "32d"
    4 ->
      Right "32"
    3 ->
      Right "64d"
    2 ->
      Right "64"
    _ ->
      Left "too long or too dotted"
