module VexFlow.Abc.Stringify (keySignature, note, notes) where

-- | Convert between Abc types and VexFlow which are Strings
import Prelude ((<>), ($), (*), map, show)
import Data.Either (Either(..))
import Data.Int (round)
import Data.String.Common (toLower)
import Data.Rational (fromInt, toNumber)
import Data.Traversable (sequence)
import Data.Abc
import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (normaliseModalKey)
import VexFlow.Types (AbcContext, VexNote)


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

-- | translate an array of ABC notes to VexFlow notes
notes :: AbcContext -> Array AbcNote -> Either String (Array VexNote)
notes context abcNotes =
  sequence $ map (note context) abcNotes

-- | translate an ABC note to a VexFlow note
-- | failing if the duration cannot be tarnslated
note :: AbcContext -> AbcNote -> Either String VexNote
note context abcNote =
  let
    edur = noteDur context abcNote.duration
    key = pitch abcNote.pitchClass abcNote.accidental abcNote.octave
  in
    case edur of
      Right dur -> Right
        { clef : "treble"
        , keys : [key]
        , duration : dur
        }
      Left x -> Left x

noteDur :: AbcContext -> NoteDuration -> Either String String
noteDur ctx d =
  duration ctx d


-- | translate a note or rest duration, wrapping in a Result which indicates an
-- | unsupported duration.  This rounds values of 'short enough' note durations
-- | to the nearest supported value
duration :: AbcContext -> NoteDuration -> Either String String
duration ctx d =
  let
    durn =
      round $ toNumber $
         ctx.unitNoteLength * d * (fromInt 128)
  in
    case durn of
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
