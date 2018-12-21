module VexFlow.Abc.Utils
  ( applyContextChanges
  , dotCount
  , normaliseBroken
  , noteDotCount
  , noteTicks
  , initialAbcContext
  , updateAbcContext
  , nextStaveNo
  , isEmptyMusicSpec
  , cMajor
  , canvasHeight) where

import Data.Abc (AbcTune, AbcNote, Broken(..), GraceableNote, KeySignature,
  Accidental(..), Mode(..), NoteDuration, PitchClass(..))
import Data.Abc.Metadata (dotFactor, getMeter, getKeySig, getUnitNoteLength)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (round, toNumber) as Int
import Data.List (length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (fromInt, toNumber, (%))
import Data.Tuple (Tuple(..))
import Prelude (map, ($), (*), (+), (-), (/))
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Types (AbcContext, Config, MusicSpec(..), staveIndentation)

-- | the degree to which a note is dotted
dotCount :: AbcContext -> NoteDuration -> Int
dotCount ctx d =
  case noteTicks ctx d of
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
noteTicks :: AbcContext -> NoteDuration -> Int
noteTicks ctx d =
  Int.round $ toNumber $
     ctx.unitNoteLength * d * (fromInt 128)

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

initialAbcContext :: AbcTune -> Config -> AbcContext
initialAbcContext tune config =
  let
    meterSignature =
      fromMaybe (Tuple 4 4) $ getMeter tune
    (Tuple numerator denominator) = meterSignature
    unitNote =
      fromMaybe (1 % 8) $ getUnitNoteLength tune
    keySignature =
      fromMaybe cMajor $ map (\mks -> mks.keySignature) (getKeySig tune)
  in
    { timeSignature : { numerator, denominator }
    , keySignature : keySignature
    , unitNoteLength : unitNote
    , staveNo : Nothing
    , accumulatedStaveWidth : staveIndentation  -- just the initial margin
    , isMidVolta : false
    , isNewTimeSignature : true  -- when we start off
    , maxWidth : Int.round $
        (Int.toNumber (config.canvasWidth - staveIndentation)) / config.scale
    , pendingRepeatBegin : false
    }


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
  null contents.noteSpecs

cMajor :: KeySignature
cMajor =
  {  pitchClass : C
  ,  accidental : Natural
  ,  mode : Major
  }

-- | Heuristic to measure the canvas height needed to display a tune
canvasHeight :: AbcTune -> Int
canvasHeight tune =
  (length tune.body) * 100
