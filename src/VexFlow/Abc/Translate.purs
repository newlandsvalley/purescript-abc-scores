module VexFlow.Abc.Translate
  ( keySignature
  , headerChange
  , notePitch
  , music) where

-- | Translate between low-level leaves of the Abc data structure  and VexFlow types
-- | At the leaves, we don't need to thread context through the translation

import Data.Abc

import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (normaliseModalKey)
import Data.Array (length, mapWithIndex)
import Data.Either (Either(..))
import Data.List.NonEmpty (head, toUnfoldable) as Nel
import Data.Rational ((%), numerator, denominator)
import Data.String.Common (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Prelude ((<>), ($), (*), (+), (-), (==), map, mempty, show)
import VexFlow.Abc.Utils (dotCount, normaliseBroken, noteDotCount, noteTicks)
import VexFlow.Types (AbcContext, NoteSpec, TupletSpec, MusicSpec(..))
import VexFlow.Abc.TickableContext (NoteCount, TickableContext, getTickableContext)
import VexFlow.Abc.ContextChange (ContextChange(..))

-- | generate a VexFlow indication of pitch
pitch :: PitchClass -> Accidental -> Int -> String
pitch pc acc oct =
  toLower (show  pc) <> accidental acc <> "/" <> show oct

-- | return the VexFlow pitch of a note
notePitch :: AbcNote -> String
notePitch abcNote =
  -- VexFlow's notation of octave is one higher thab our ABC's
  pitch abcNote.pitchClass abcNote.accidental (abcNote.octave - 1)

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

-- | translate any ABC music item that produces score to a VexFlow music spec
-- | producing an empty array if it doesn't do so
-- | NoteCount is the count of 'tickable' items (notes or otherwise)
-- | that precede this music item in the bar
music :: AbcContext -> NoteCount -> Int -> NoteDuration -> Music -> Either String MusicSpec
music context tickablePosition noteIndex phraseDuration m =
  let
    -- find the number and size of 'tickable' items in this music item
    tickableContext :: TickableContext
    tickableContext = getTickableContext m
    -- find the fraction  of the bar that has already been processed
    barFraction =
      phraseDuration * context.unitNoteLength
    midBarNoteIndex =
      if (barFraction == (1 % 2))
        then [noteIndex]
      else
        []
  in
    case m of
      Note gn ->
        buildMusicSpecFromN tickableContext noteIndex midBarNoteIndex gn.abcNote.tied
          $ graceableNote context 0 gn

      Rest dur ->
        -- rests are never tied
        buildMusicSpecFromN tickableContext noteIndex midBarNoteIndex false $ rest context dur

      Chord abcChord ->
        -- we don't support tied chords at the moment
        buildMusicSpecFromN tickableContext noteIndex midBarNoteIndex false $ chord context abcChord

      BrokenRhythmPair gn1 broken gn2 ->
        buildMusicSpecFromNs tickableContext midBarNoteIndex $ brokenRhythm context gn1 broken gn2

      Tuplet signature rOrNs ->
        let
          eRes = tuplet context tickablePosition signature (Nel.toUnfoldable rOrNs)
        in
          -- we don't support tied tuplets at the moment
          map (\tupletSpec ->
             MusicSpec
                { noteSpecs : tupletSpec.noteSpecs
                , tuplets : [tupletSpec.vexTuplet]
                , ties : []
                , tickableContext : tickableContext
                , contextChanges : mempty
                , midBarNoteIndex : midBarNoteIndex
                }
              ) eRes

      Inline header ->
        buildMusicSpecFromContextChange $ headerChange context header

      _ ->
        buildMusicSpecFromNs tickableContext [] (Right [])


graceableNote :: AbcContext -> Int -> GraceableNote -> Either String NoteSpec
graceableNote context noteIndex gn =
  let
    graceNotes :: Array AbcNote
    graceNotes = maybe [] (\grace -> Nel.toUnfoldable grace.notes) gn.maybeGrace
    graceKeys :: Array String
    graceKeys = map notePitch graceNotes
  in
    note context noteIndex graceKeys gn.abcNote

-- | Translate an ABC note to a VexFlow note
-- | failing if the duration cannot be translated
-- | noteIndex is the index of the note within any bigger structure such as
-- | a tuplet or broken rhythm pair and is 0 for a note on its own
note :: AbcContext -> Int -> Array String -> AbcNote -> Either String NoteSpec
note context noteIndex graceKeys abcNote  =
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
          , graceKeys : graceKeys
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
          , graceKeys : []
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
          , graceKeys : []
          }
      Left x -> Left x

-- | translate an ABC broken note pair to a VexFlow note pair
-- | failing if either duration cannot be translated
-- | not finished - n1 can alter the context for n2
brokenRhythm :: AbcContext -> GraceableNote -> Broken -> GraceableNote -> Either String (Array NoteSpec)
brokenRhythm context gn1 broken gn2 =
  let
    (Tuple n1 n2) = normaliseBroken broken gn1 gn2
    enote1 = graceableNote context 0 n1
    enote2 =graceableNote context 1 n2
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
    enoteSpecs = sequence $ mapWithIndex (restOrNote context) rns
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

restOrNote :: AbcContext -> Int -> RestOrNote -> Either String NoteSpec
restOrNote context noteIndex rOrn =
  case rOrn of
    Left abcRest ->
      rest context abcRest
    Right gn ->
      graceableNote context noteIndex gn

-- | cater for an inline header (within a stave)
-- |   we need to cater for changes in key signature, meter or unit note length
-- | which all alter the translation context.  All other headers may be ignored

headerChange :: AbcContext -> Header -> Array ContextChange
headerChange ctx h =
  case h of
    Key mks ->
      [KeyChange mks]

    UnitNoteLength dur ->
      [UnitNoteChange dur]

    Meter maybeSignature ->
      case maybeSignature of
        Just meterSignature ->
          [MeterChange meterSignature]
        _ ->
          []
    _ ->
      []


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
    112 ->
      Right "hdd"
    96 ->
      Right "hd"
    64 ->
      Right "h"
    56 ->
      Right "qdd"
    48 ->
      Right "qd"
    32 ->
      Right "q"
    28 ->
      Right "8dd"
    24 ->
      Right "8d"
    16 ->
      Right "8"
    14 ->
      Right "16dd"
    12 ->
      Right "16d"
    8 ->
      Right "16"
    7 ->
      Right "32dd"
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

buildMusicSpecFromNs :: TickableContext -> Array Int -> Either String (Array NoteSpec) -> Either String MusicSpec
buildMusicSpecFromNs tCtx midBarNoteIndex ens =
  map (\ns -> MusicSpec
    { noteSpecs : ns
    , tuplets : []
    , ties : []
    , tickableContext : tCtx
    , contextChanges : []
    , midBarNoteIndex : midBarNoteIndex
    }) ens

buildMusicSpecFromN :: TickableContext -> Int -> Array Int -> Boolean -> Either String NoteSpec -> Either String MusicSpec
buildMusicSpecFromN tCtx noteIndex midBarNoteIndex isTied ens =
    map (\ns -> MusicSpec
      { noteSpecs : [ns]
      , tuplets : []
      , ties :
         -- if the note is tied then save its index into the note array
         if isTied then [noteIndex] else []
      , tickableContext : tCtx
      , contextChanges : []
      , midBarNoteIndex : midBarNoteIndex
      }) ens

buildMusicSpecFromContextChange :: Array ContextChange -> Either String MusicSpec
buildMusicSpecFromContextChange contextChange =
    Right $ MusicSpec
      { noteSpecs : []
      , tuplets : []
      , ties : []
      , tickableContext : mempty
      , contextChanges : contextChange
      , midBarNoteIndex : []
      }
