module VexFlow.Abc.Translate (keySignature, musics) where

-- | Translate between Abc types and VexFlow types which are
-- | (at base) Strings
import Data.Abc

import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (normaliseModalKey)
import Data.Array (length)
import Data.Either (Either(..))
import Data.List.NonEmpty (head, toUnfoldable) as Nel
import Data.Rational (numerator, denominator)
import Data.String.Common (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude ((<>), ($), (*), (+), join, map, show)
import VexFlow.Abc.Utils (dotCount, normaliseBroken, noteDotCount, noteTicks)
import VexFlow.Types (AbcContext, NoteSpec, TupletSpec, MusicSpec, VexTuplet)

-- | generate a VexFlow indication of pitch
pitch :: PitchClass -> Accidental -> Int -> String
pitch pc acc oct =
  toLower (show  pc) <> accidental acc <> "/" <> show oct

-- | return the VexFlow pitch of a note
notePitch :: AbcNote -> String
notePitch abcNote =
  pitch abcNote.pitchClass abcNote.accidental abcNote.octave

accidental :: Accidental -> String
accidental Sharp = "#"
accidental Flat =  "b"
accidental DoubleSharp = "##"
accidental DoubleFlat = "bb"
accidental Natural = "n"
accidental Implicit = ""

-- | return the VexFlow string representation of a note's accidental
noteAccidental :: AbcNote -> String
noteAccidental abcNote = accidental abcNote.accidental

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


-- | translate an array of ABC music (from a bar) to a VexFlow music spec
musics :: AbcContext -> Array Music -> Either String MusicSpec
musics context abcMusics =
  let
    -- mSpec = map join $ sequence $ map (music context) abcMusics
    emSpecs :: Either String (Array MusicSpec)
    emSpecs = sequence $ map (music context) abcMusics
  in
    case emSpecs of
      Right mSpecs ->
        let
          -- join all the noteSpecs together
          fullNoteSpecs :: Array NoteSpec
          fullNoteSpecs = join $ map (\ms -> ms.noteSpecs) mSpecs
            -- join all the tuolet Specs together
          fullTuplets :: Array VexTuplet
          fullTuplets = join $ map (\ms -> ms.tuplets) mSpecs
        in
          Right
            { noteSpecs : fullNoteSpecs
            , tuplets : fullTuplets }
      Left err ->
        Left err

-- | translate any ABC music item that produces score to a VexFlow music spec
-- | producing an empty array if it doesn't do so
-- | unfiniahed - need to provide a start offset for the Tuplet case
music :: AbcContext -> Music -> Either String MusicSpec
music context m =
  case m of
    Note abcNote ->
      buildMusicSpecFromN $ note context abcNote

    Rest dur ->
      buildMusicSpecFromN $ rest context dur

    Chord abcChord ->
      buildMusicSpecFromN $ chord context abcChord

    BrokenRhythmPair abcNote1 broken abcNote2 ->
      buildMusicSpecFromNs $ brokenRhythm context abcNote1 broken abcNote2

    Tuplet signature rOrNs ->
      let
        eRes = tuplet context 0 signature (Nel.toUnfoldable rOrNs)
      in
        map (\tupletSpec ->
              { noteSpecs : tupletSpec.noteSpecs
              , tuplets : [tupletSpec.vexTuplet]
              }
            ) eRes

    _ ->
      buildMusicSpecFromNs (Right [])

-- | translate an array of ABC notes to VexFlow notes
notes :: AbcContext -> Array AbcNote -> Either String (Array NoteSpec)
notes context abcNotes =
  sequence $ map (note context) abcNotes

-- | translate an ABC note to a VexFlow note
-- | failing if the duration cannot be tarnslated
note :: AbcContext -> AbcNote -> Either String NoteSpec
note context abcNote =
  let
    edur = noteDur context abcNote
    key = notePitch abcNote
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
          , dots : [noteDotCount context abcNote]
          }
      Left x -> Left x

-- | translate an ABC rest to a VexFlow note
-- | which we'll position on the B stave line
-- | failing if the duration cannot be translated
rest :: AbcContext -> AbcRest -> Either String NoteSpec
rest context abcRest =
  let
    edur = duration context abcRest.duration
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


-- | translate an ABC chord to a VexFlow note
-- | failing if the durations cannot be translated
-- | n.b. in VexFlow, all notes in a chord must have the same duration
-- | this is a mismatch with ABC.  We just take the first note as representative
chord :: AbcContext -> AbcChord -> Either String NoteSpec
chord context abcChord =
  let
    edur :: Either String String
    edur = chordalNoteDur context abcChord.duration (Nel.head abcChord.notes)
    keys :: Array String
    keys = map notePitch (Nel.toUnfoldable abcChord.notes)
    dotCounts :: Array Int
    dotCounts = map (noteDotCount context) (Nel.toUnfoldable abcChord.notes)
    accidentals :: Array String
    accidentals = map noteAccidental (Nel.toUnfoldable abcChord.notes)
  in
    case edur of
      Right dur ->
        let
          vexNote =
            { clef : "treble"
            , keys : keys
            , duration : dur
            }
        in Right
          { vexNote : vexNote
          , accidentals : accidentals
          , dots : dotCounts
          }
      Left x -> Left x

-- | translate an ABC broken note pair to a VexFlow note pair
-- | failing if either duration cannot be translated
-- | not finished - n1 can alter the context for n2
brokenRhythm :: AbcContext -> AbcNote -> Broken -> AbcNote -> Either String (Array NoteSpec)
brokenRhythm context abcNote1 broken abcNote2 =
  let
    (Tuple n1 n2) = normaliseBroken broken abcNote1 abcNote2
    enote1 = note context n1
    enote2 = note context n2
  in
    case (Tuple enote1 enote2) of
      (Tuple (Right note1) (Right note2)) ->
        Right [note1, note2]
      (Tuple (Left e1) _) ->
        Left e1
      (Tuple _ (Left e2) ) ->
        Left e2

-- | translate an ABC tuplet pair to a VexFlow tuplet spec
-- | failing if any note duration cannot be translated
-- | not finished - n1 can alter the context for n2
tuplet :: AbcContext -> Int -> TupletSignature -> Array RestOrNote -> Either String TupletSpec
tuplet context startOffset signature rns =
  let
    enoteSpecs = sequence $ map (restOrNote context) rns
    vexTuplet =
      { p : signature.p
      , q : signature.q
      , startPos : startOffset
      , endPos : startOffset + (length rns)
      }
  in
    case enoteSpecs of
      Right noteSpecs ->
        Right
          { vexTuplet : vexTuplet
          , noteSpecs : noteSpecs
          }
      Left x ->
        Left x


restOrNote :: AbcContext -> RestOrNote -> Either String NoteSpec
restOrNote context rOrn =
  case rOrn of
    Left abcRest ->
      rest context abcRest
    Right abcNote ->
      note context abcNote

-- | translate a note duration within a chord
-- | (in ABC, a chord has a duration over and above the individual
-- | note durations and these are multiplicative)
chordalNoteDur :: AbcContext -> NoteDuration -> AbcNote -> Either String String
chordalNoteDur ctx chordDur abcNote  =
  duration ctx (abcNote.duration * chordDur)

-- | translate a note duration
noteDur :: AbcContext -> AbcNote -> Either String String
noteDur ctx abcNote =
  duration ctx abcNote.duration


-- | translate a duration (from a note or rest), wrapping in a Result which indicates an
-- | unsupported duration.  This rounds values of 'short enough' note durations
-- | to the nearest supported value
duration :: AbcContext -> NoteDuration -> Either String String
duration ctx d =
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
      Left ("too long or too dotted duration: "
          <> (show $ numerator d)
          <> "/"
          <> (show $ denominator d))

buildMusicSpecFromNs :: Either String (Array NoteSpec) -> Either String MusicSpec
buildMusicSpecFromNs ens =
    map (\ns -> { noteSpecs : ns, tuplets : [] }) ens

buildMusicSpecFromN :: Either String NoteSpec -> Either String MusicSpec
buildMusicSpecFromN ens =
    map (\ns -> { noteSpecs : [ns], tuplets : [] }) ens
