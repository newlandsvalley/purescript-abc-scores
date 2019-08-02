module VexFlow.Abc.Utils
  ( applyContextChanges
  , vexDuration
  , compoundVexDuration
  , chordalNoteLength
  , normaliseBroken
  , noteDotCount
  , noteTicks
  , initialAbcContext
  , updateAbcContext
  , nextStaveNo
  , isEmptyMusicSpec
  , canvasHeight) where

import Data.Abc (AbcChord, AbcTune, AbcNote, Broken(..), GraceableNote, KeySignature,
  ModifiedKeySignature, Accidental(..), Mode(..), NoteDuration, PitchClass(..),
  TempoSignature)
import Data.Abc.Metadata (dotFactor, getMeter, getKeySig, getTempoSig,
       getUnitNoteLength)
import Data.Array (null, replicate) as Array
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Int (round, toNumber) as Int
import Data.List (List(..), length, null)
import Data.List.NonEmpty (head) as Nel
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Rational (fromInt, toNumber, numerator, denominator, (%))
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits (fromCharArray)
import Prelude (map, show, ($), (*), (+), (-), (/), (<>), identity)
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Abc.Beat (beatDuration)
import VexFlow.Types (AbcContext, Config, MusicSpec(..), Tempo,
   VexDuration, staveIndentation)

-- | build a VexDuration
vexDuration :: NoteDuration -> NoteDuration -> Either String VexDuration
vexDuration unitNoteLength d =
  case noteTicks unitNoteLength d of
    128 ->
      Right { vexDurString : "w", dots : 0 }
    112 ->
      Right { vexDurString : "h", dots : 2 }
    96 ->
      Right { vexDurString : "h", dots : 1 }
    64 ->
      Right { vexDurString : "h", dots : 0 }
    56 ->
      Right { vexDurString : "q", dots : 2 }
    48 ->
      Right { vexDurString : "q", dots : 1 }
    32 ->
      Right { vexDurString : "q", dots : 0 }
    28 ->
      Right {vexDurString : "8", dots : 2 }
    24 ->
      Right { vexDurString : "8", dots : 1 }
    16 ->
      Right { vexDurString : "8", dots : 0 }
    14 ->
      Right { vexDurString : "16", dots : 2 }
    12 ->
      Right { vexDurString : "16", dots : 1 }
    8 ->
      Right { vexDurString : "16", dots : 0 }
    7 ->
      Right { vexDurString : "32", dots : 2 }
    6 ->
      Right { vexDurString : "32", dots : 1 }
    4 ->
      Right { vexDurString : "32", dots : 0 }
    3 ->
      Right { vexDurString : "64", dots : 1 }
    2 ->
      Right { vexDurString : "64", dots : 0 }
    _ ->
      Left ("too long or too dotted duration: "
          <> (show $ numerator d)
          <> "/"
          <> (show $ denominator d))

-- | Generate a compound VexFlow duration - i.e. a String which is prefaced
-- | by the basic note duration and this is followd by (optionally) a
-- | number of 'd's which represent the number of dots to diaplay.
compoundVexDuration :: VexDuration -> String
compoundVexDuration vexDur =
 let
   dStr = fromCharArray $ Array.replicate vexDur.dots 'd'
 in
   vexDur.vexDurString <> dStr

-- | the length of an ABC note is he length of a note in the chord multiplied
-- | by the length of the chord itself
-- | note this is not a full duration as that must take into account the
-- | unit note length
chordalNoteLength :: AbcChord -> NoteDuration
chordalNoteLength abcChord =
  (Nel.head abcChord.notes).duration * abcChord.duration

-- | build a VexFlow tempo from the BPM and the tempo note duration
buildTempo :: Int -> NoteDuration -> Either String Tempo
buildTempo bpm d =
  case (vexDuration (fromInt 1) d) of
    Right vexDur ->
      Right { duration : vexDur.vexDurString, dots : vexDur.dots, bpm }
    Left err ->
      Left err

-- | calculate the number of dots in a dotted note
noteDotCount :: AbcContext -> AbcNote -> Int
noteDotCount ctx abcNote =
  case vexDuration ctx.unitNoteLength abcNote.duration of
    Right vexDur ->
      vexDur.dots
    _ ->
      0

-- | note duration in ticks - 1 beat split into 128 possible unit ticks
noteTicks :: NoteDuration -> NoteDuration -> Int
noteTicks unitNoteLength d =
  Int.round $ toNumber $
     unitNoteLength * d * (fromInt 128)

-- | apply the specified broken rhythm to each note in the note pair (presented individually)
-- | and return the broken note pair presented conventionally
normaliseBroken :: Broken -> GraceableNote -> GraceableNote -> (Tuple GraceableNote GraceableNote )
normaliseBroken broken gn1 gn2 =
  let
    down i =
      (fromInt 1) - (dotFactor i)

    up i =
      (fromInt 1) + (dotFactor i)
  in
    case broken of
      LeftArrow i ->
        let
          lefta =
            gn1.abcNote { duration = gn1.abcNote.duration * (down i) }

          righta =
            gn2.abcNote { duration = gn2.abcNote.duration * (up i) }
        in
          (Tuple (gn1 {abcNote = lefta}) (gn2 {abcNote = righta}) )

      RightArrow i ->
        let
          lefta =
            gn1.abcNote { duration = gn1.abcNote.duration * (up i) }

          righta =
            gn2.abcNote { duration = gn2.abcNote.duration * (down i) }
        in
          (Tuple (gn1 {abcNote = lefta}) (gn2 {abcNote = righta}) )

initialAbcContext :: AbcTune -> Config -> Either String AbcContext
initialAbcContext tune config =
  let
    meterSignature =
      fromMaybe (Tuple 4 4) $ getMeter tune
    (Tuple numerator denominator) = meterSignature
    unitNoteLength :: NoteDuration
    unitNoteLength =
      fromMaybe (1 % 8) $ getUnitNoteLength tune
    modifiedKeySignature =
      fromMaybe cMajor $ map identity (getKeySig tune)
    mTempo =
      maybe Nothing (tempoMarking unitNoteLength) (getTempoSig tune)
  in
    if (null modifiedKeySignature.modifications) then
      Right
        { timeSignature : { numerator, denominator }
        , keySignature : modifiedKeySignature.keySignature
        , mTempo: mTempo
        , unitNoteLength : unitNoteLength
        , staveNo : Nothing
        , accumulatedStaveWidth : staveIndentation  -- just the initial margin
        , isMidVolta : false
        , isNewTimeSignature : true  -- when we start off
        , maxWidth : Int.round $
            (Int.toNumber (config.width - staveIndentation)) / config.scale
        , pendingRepeatBegin : false
        , beatDuration : beatDuration  { numerator, denominator }
        }
    else
      Left "modifications to standard key signatures are not supported"


updateAbcContext :: AbcContext -> ContextChange ->  AbcContext
updateAbcContext abcContext change =
  case change of
    MeterChange meterSignature ->
      let
        (Tuple numerator denominator) = meterSignature
        timeSignature = { numerator, denominator }
      in
        abcContext { timeSignature = timeSignature
                   , isNewTimeSignature = true
                   , beatDuration = beatDuration  { numerator, denominator }
                   }
    KeyChange modifiedKeySignature ->
      abcContext { keySignature = modifiedKeySignature.keySignature
                 , isNewTimeSignature = false
                 }
    UnitNoteChange length ->
      abcContext { unitNoteLength = length
                 , isNewTimeSignature = false
                 }

applyContextChanges :: AbcContext -> Either String MusicSpec ->  AbcContext
applyContextChanges abcContext eSpec  =
  case eSpec of
    Right (MusicSpec spec) ->
      foldl updateAbcContext abcContext spec.contextChanges
    _ ->
      abcContext


nextStaveNo :: Maybe Int -> Maybe Int
nextStaveNo Nothing = Just 0
nextStaveNo (Just x) = Just (x + 1)

-- | return true if the MusicSpec is empty
isEmptyMusicSpec :: MusicSpec -> Boolean
isEmptyMusicSpec (MusicSpec contents) =
  Array.null contents.noteSpecs

cMajor :: ModifiedKeySignature
cMajor =
  let
    ks :: KeySignature
    ks =
      {  pitchClass : C
      ,  accidental : Natural
      ,  mode : Major
      }
  in
    { keySignature  : ks
    , modifications : Nil
    }

-- | convert an ABC tempo signature to a VexFlow tempo marker
tempoMarking :: NoteDuration -> TempoSignature -> Maybe Tempo
tempoMarking unitNoteLength tempoSig =
  let
    tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
  in
    hush $ buildTempo tempoSig.bpm tempoNoteLength


-- | Heuristic to measure the canvas height needed to display a tune
canvasHeight :: AbcTune -> Int
canvasHeight tune =
  (length tune.body) * 100
