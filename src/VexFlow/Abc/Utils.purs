module VexFlow.Abc.Utils
  ( applyContextChanges
  , duration
  , dotCount
  , normaliseBroken
  , noteDotCount
  , noteTicks
  , initialAbcContext
  , updateAbcContext
  , nextStaveNo
  , isEmptyMusicSpec
  , canvasHeight) where

import Data.Abc (AbcTune, AbcNote, Broken(..), GraceableNote, KeySignature,
  ModifiedKeySignature, Accidental(..), Mode(..), NoteDuration, PitchClass(..),
  TempoSignature)
import Data.Abc.Metadata (dotFactor, getMeter, getKeySig, getTempoSig,
       getUnitNoteLength)
import Data.Array (null) as Array
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Int (round, toNumber) as Int
import Data.List (List(..), length, null)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Rational (fromInt, toNumber, numerator, denominator, (%))
import Data.Tuple (Tuple(..))
import Prelude (map, show, ($), (*), (+), (-), (/), (<>), identity)
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Types (AbcContext, Config, MusicSpec(..), Tempo, staveIndentation)

-- | translate a duration (from a note or rest), wrapping in a Result which indicates an
-- | unsupported duration.  This rounds values of 'short enough' note durations
-- | to the nearest supported value
duration :: NoteDuration -> NoteDuration -> Either String String
duration unitNoteLength d =
  case noteTicks unitNoteLength d of
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

-- | build a VexFlow tempo from the BPM and the tempo note duration
buildTempo :: Int -> NoteDuration -> Either String Tempo
buildTempo bpm d =
  case noteTicks (fromInt 1) d of
    128 ->
      Right { duration : "w", dots : 0, bpm }
    112 ->
      Right { duration : "h", dots : 2, bpm }
    96 ->
      Right { duration : "h", dots : 1, bpm }
    64 ->
      Right { duration : "h", dots : 0, bpm }
    56 ->
      Right { duration : "q", dots : 2, bpm }
    48 ->
      Right { duration : "q", dots : 1, bpm }
    32 ->
      Right { duration : "q", dots : 0, bpm }
    28 ->
      Right { duration : "8", dots : 2, bpm }
    24 ->
      Right { duration : "8", dots : 1, bpm }
    16 ->
      Right { duration : "8", dots : 0, bpm }
    14 ->
      Right { duration : "16", dots : 2, bpm }
    12 ->
      Right { duration : "16", dots : 1, bpm }
    8 ->
      Right { duration : "16", dots : 0, bpm }
    7 ->
      Right { duration : "32", dots : 2, bpm }
    6 ->
      Right { duration : "32", dots : 1, bpm }
    4 ->
      Right { duration : "32", dots : 0, bpm }
    3 ->
      Right { duration : "64", dots : 1, bpm }
    2 ->
      Right { duration : "64", dots : 0, bpm }
    _ ->
      Left ("too long or too dotted duration: "
          <> (show $ numerator d)
          <> "/"
          <> (show $ denominator d))


-- | the degree to which a note is dotted
dotCount :: AbcContext -> NoteDuration -> Int
dotCount ctx d =
  case noteTicks ctx.unitNoteLength d of
    112 ->
      2
    96 ->
      1
    56 ->
      2
    48 ->
      1
    28 ->
      2
    24 ->
      1
    14 ->
      2
    12 ->
      1
    7 ->
      2
    6 ->
      1
    3 ->
      1
    _ ->
      0

noteDotCount :: AbcContext -> AbcNote -> Int
noteDotCount ctx abcNote =
  dotCount ctx abcNote.duration

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
            (Int.toNumber (config.canvasWidth - staveIndentation)) / config.scale
        , pendingRepeatBegin : false
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
