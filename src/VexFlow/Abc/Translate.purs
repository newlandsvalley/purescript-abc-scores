module VexFlow.Abc.Translate (bar, keySignature, musics) where

-- | Translate between Abc types and VexFlow types which are
-- | (at base) Strings
import Data.Abc

import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (normaliseModalKey)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.NonEmpty (head, toUnfoldable) as Nel
import Data.List (toUnfoldable)
import Data.Rational (numerator, denominator)
import Data.String.Common (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude ((<>), ($), (*), (+), map, mempty, show)
import VexFlow.Abc.Utils (dotCount, normaliseBroken, noteDotCount, noteTicks)
import VexFlow.Types (AbcContext, BarSpec, NoteSpec, TupletSpec, MusicSpec(..))
import VexFlow.Abc.TickableContext (NoteCount, TickableContext(..), getTickableContext)
import VexFlow.Abc.ContextChange (ContextChange(..))

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

bar :: AbcContext -> Int -> Bar -> Either String BarSpec
bar context barNumber abcBar =
  let
    emusics = musics context $ toUnfoldable abcBar.music
  in
    case emusics of
      Right m ->
        let
          barSpec =
            { barNumber : barNumber
            , startLine : abcBar.startLine
            , musicSpec : m
            }
         in
           Right barSpec
      Left err ->
        Left (err <> ": bar " <> show barNumber)

musics :: AbcContext -> Array Music -> Either String MusicSpec
musics context abcMusics =
  foldOverMusics context abcMusics

foldOverMusics :: AbcContext -> Array Music -> Either String MusicSpec
foldOverMusics context=
  let
    acc =
      Right $ mempty :: MusicSpec
  in
    foldl (foldMusicsFunction context) acc

-- | fold the music function over the array of music.
-- | the monoidal behaviour of TickableContext within MusicSpec
-- | accumulates this context by threading through the fold
foldMusicsFunction ::
  AbcContext ->
  Either String MusicSpec ->
  Music ->
  Either String MusicSpec
foldMusicsFunction context eacc m =
  let
    (TickableContext position duration) =
      case eacc of
        Right (MusicSpec acc) ->
          acc.tickableContext
        _ ->
          mempty
    enext :: Either String MusicSpec
    enext = music context position m
  in
    case (Tuple eacc enext) of
      (Tuple (Right acc) (Right next)) ->
        Right $ acc <> next

      (Tuple (Left err) _ ) ->
        Left err

      (Tuple _ (Left err) ) ->
        Left err

-- | translate any ABC music item that produces score to a VexFlow music spec
-- | producing an empty array if it doesn't do so
-- | NoteCount is the count of 'tickable' items (notes or otherwise)
-- | that precede this music item in the bar
music :: AbcContext -> NoteCount -> Music -> Either String MusicSpec
music context tickablePosition m =
  let
    -- find the number and size of 'tickable' items in this music item
    tickableContext :: TickableContext
    tickableContext = getTickableContext m
  in
    case m of
      Note abcNote ->
        buildMusicSpecFromN tickableContext $ note context abcNote

      Rest dur ->
        buildMusicSpecFromN tickableContext $ rest context dur

      Chord abcChord ->
        buildMusicSpecFromN tickableContext $ chord context abcChord

      BrokenRhythmPair abcNote1 broken abcNote2 ->
        buildMusicSpecFromNs tickableContext $ brokenRhythm context abcNote1 broken abcNote2

      Tuplet signature rOrNs ->
        let
          eRes = tuplet context tickablePosition signature (Nel.toUnfoldable rOrNs)
        in
          map (\tupletSpec ->
             MusicSpec
                { noteSpecs : tupletSpec.noteSpecs
                , tuplets : [tupletSpec.vexTuplet]
                , tickableContext : tickableContext
                , contextChange : mempty
                }
              ) eRes

      _ ->
        buildMusicSpecFromNs tickableContext (Right [])

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

buildMusicSpecFromNs :: TickableContext -> Either String (Array NoteSpec) -> Either String MusicSpec
buildMusicSpecFromNs tCtx ens =
    map (\ns -> MusicSpec
      { noteSpecs : ns
      , tuplets : []
      , tickableContext : tCtx
      , contextChange : mempty
      }) ens

buildMusicSpecFromN :: TickableContext -> Either String NoteSpec -> Either String MusicSpec
buildMusicSpecFromN tCtx ens =
    map (\ns -> MusicSpec
      { noteSpecs : [ns]
      , tuplets : []
      , tickableContext : tCtx
      , contextChange : mempty
      }) ens
