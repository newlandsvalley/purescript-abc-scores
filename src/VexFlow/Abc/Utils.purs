module VexFlow.Abc.Utils
  ( applyContextChanges
  , vexDuration
  , compoundVexDuration
  , noteDotCount
  , noteTicks
  , initialAbcContext
  , updateAbcContext
  , nextStaveNo
  , isEmptyMusicSpec
  , getBarFill
  , getComposerAndOrigin
  , canvasHeight
  ) where

import Data.Abc (AbcNote, AbcTune, ModifiedKeySignature, NoteDuration, TempoSignature, TimeSignature)
import Data.Abc.KeySignature (defaultKey, getKeySig)
import Data.Abc.Meter (getDefaultedMeter)
import Data.Abc.Optics (_headers, _properties, _Composer, _Origin, _Voice)
import Data.Abc.Tempo (getTempoSig)
import Data.Abc.UnitNote (getUnitNoteLength)
import Data.Array (null, replicate) as Array
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Int (round, toNumber) as Int
import Data.Lens.At (at)
import Data.Lens.Fold (firstOf, lastOf)
import Data.Lens.Lens (Lens')
import Data.Lens.Traversal (traversed)
import Data.List (length, null)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Rational (fromInt, toNumber, numerator, denominator, (%))
import Data.String.CodeUnits (fromCharArray)
import Prelude (join, map, show, ($), (*), (+), (-), (/), (<>), (>), (&&), (<<<), (==), (<$>), (<*>), identity)
import VexFlow.Abc.Beat (beatDuration)
import VexFlow.Abc.ContextChange (ContextChange(..), Clef(..))
import VexFlow.Abc.TickableContext (TickableContext(..))
import VexFlow.Types (AbcContext, BarFill(..), Config, MusicSpec(..), Tempo, VexDuration, staveIndentation, staveSeparation, titleDepth)

-- | build a VexDuration
vexDuration :: NoteDuration -> NoteDuration -> Either String VexDuration
vexDuration unitNoteLength d =
  case noteTicks unitNoteLength d of
    128 ->
      Right { vexDurString: "w", dots: 0 }
    112 ->
      Right { vexDurString: "h", dots: 2 }
    96 ->
      Right { vexDurString: "h", dots: 1 }
    64 ->
      Right { vexDurString: "h", dots: 0 }
    56 ->
      Right { vexDurString: "q", dots: 2 }
    48 ->
      Right { vexDurString: "q", dots: 1 }
    32 ->
      Right { vexDurString: "q", dots: 0 }
    28 ->
      Right { vexDurString: "8", dots: 2 }
    24 ->
      Right { vexDurString: "8", dots: 1 }
    16 ->
      Right { vexDurString: "8", dots: 0 }
    14 ->
      Right { vexDurString: "16", dots: 2 }
    12 ->
      Right { vexDurString: "16", dots: 1 }
    8 ->
      Right { vexDurString: "16", dots: 0 }
    7 ->
      Right { vexDurString: "32", dots: 2 }
    6 ->
      Right { vexDurString: "32", dots: 1 }
    4 ->
      Right { vexDurString: "32", dots: 0 }
    3 ->
      Right { vexDurString: "64", dots: 1 }
    2 ->
      Right { vexDurString: "64", dots: 0 }
    _ ->
      Left
        ( "too long or too dotted duration: "
            <> (show $ numerator d)
            <> "/"
            <> (show $ denominator d)
        )

-- | Generate a compound VexFlow duration - i.e. a String which is prefaced
-- | by the basic note duration and this is followd by (optionally) a
-- | number of 'd's which represent the number of dots to diaplay.
compoundVexDuration :: VexDuration -> String
compoundVexDuration vexDur =
  let
    dStr = fromCharArray $ Array.replicate vexDur.dots 'd'
  in
    vexDur.vexDurString <> dStr

-- | build a VexFlow tempo from the BPM and the tempo note duration
buildTempo :: Int -> NoteDuration -> Either String Tempo
buildTempo bpm d =
  case (vexDuration (fromInt 1) d) of
    Right vexDur ->
      Right { duration: vexDur.vexDurString, dots: vexDur.dots, bpm }
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

initialAbcContext :: AbcTune -> Config -> Either String AbcContext
initialAbcContext tune config =
  let
    timeSignature =
      getDefaultedMeter tune

    unitNoteLength :: NoteDuration
    unitNoteLength =
      fromMaybe (1 % 8) $ getUnitNoteLength tune
    modifiedKeySignature =
      fromMaybe cMajor $ map identity (getKeySig tune)
    mTempo =
      maybe Nothing tempoMarking (getTempoSig tune)
    clef = fromMaybe Treble (getVoiceClef tune)
  in
    if (null modifiedKeySignature.modifications) then
      Right
        { timeSignature
        , keySignature: modifiedKeySignature.keySignature
        , mTempo: mTempo
        , unitNoteLength: unitNoteLength
        , clef: clef
        , staveNo: Nothing
        , accumulatedStaveWidth: staveIndentation -- just the initial margin
        , isMidVolta: false
        , isNewTimeSignature: true -- when we start off
        , maxWidth: Int.round $
            (Int.toNumber (config.width - staveIndentation)) / config.scale
        , pendingRepeatBegin: false
        , beatDuration: beatDuration timeSignature
        , noteSeparation: config.noteSeparation
        , showChordSymbols: config.showChordSymbols
        }
    else
      Left "modifications to standard key signatures are not supported"

updateAbcContext :: AbcContext -> ContextChange -> AbcContext
updateAbcContext abcContext change =
  case change of
    MeterChange timeSignature ->
      abcContext
        { timeSignature = timeSignature
        , isNewTimeSignature = true
        , beatDuration = beatDuration 
          { numerator: timeSignature.numerator
          , denominator: timeSignature.denominator 
          }
        }
    KeyChange modifiedKeySignature ->
      abcContext
        { keySignature = modifiedKeySignature.keySignature
        , isNewTimeSignature = false
        }
    UnitNoteChange length ->
      abcContext
        { unitNoteLength = length
        , isNewTimeSignature = false
        }
    ClefChange clef ->
      abcContext
        { clef = clef
        , isNewTimeSignature = false
        }

applyContextChanges :: AbcContext -> Either String MusicSpec -> AbcContext
applyContextChanges abcContext eSpec =
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
  Array.null contents.noteSpecs && Array.null contents.repetitions

cMajor :: ModifiedKeySignature
cMajor =
  defaultKey

-- | convert an ABC tempo signature to a VexFlow tempo marker
tempoMarking :: TempoSignature -> Maybe Tempo
tempoMarking tempoSig =
  let
    tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
  in
    hush $ buildTempo tempoSig.bpm tempoNoteLength

-- | Heuristic to measure the canvas height needed to display a tune
canvasHeight :: AbcTune -> Boolean -> Int
canvasHeight tune titled =
  if titled then
    (length tune.body) * staveSeparation + titleDepth
  else
    (length tune.body) * staveSeparation

-- a lens focused on the key "clef" in a string-keyed collection
_clef :: forall a. Lens' (Map String a) (Maybe a)
_clef = at "clef"

-- | Get the clef from the last voice header if it exists
-- | We only recognise treble, alto, tenor and bass clefs at the moment.  Default is treble.
getVoiceClef :: AbcTune -> Maybe Clef
getVoiceClef tune =
  let
    clefString =
      join $ lastOf (_headers <<< traversed <<< _Voice <<< _properties <<< _clef) tune

    f :: String -> Clef
    f s =
      case s of
        "Alto" -> Alto
        "alto" -> Alto
        "Tenor" -> Tenor
        "tenor" -> Tenor
        "Bass" -> Bass
        "bass" -> Bass
        _ -> Treble
  in
    map f clefString
 
-- | get the composer and origin data from the header metadata in the ABC 
-- | and format it (if any of it exists) as one of:
-- |
-- | ```purescript 
-- | composer
-- | origin
-- | composer (origin)
-- | ```
getComposerAndOrigin :: AbcTune -> Maybe String
getComposerAndOrigin tune = 
  if (isNothing composer)
    then origin
  else if (isNothing origin)
    then composer
  else 
    frameAndCombine <$> composer <*> origin

  where 
  composer =
    firstOf (_headers <<< traversed <<< _Composer) tune
  origin =
    firstOf (_headers <<< traversed <<< _Origin) tune

  frameAndCombine :: String -> String -> String 
  frameAndCombine a b = 
    a <> " (" <> b <> ")"

-- | How full is the bar full according to the time signature
-- | e.g. a signature of 9/8 required 9 eighth notes
-- | and so a full bar requires a total note duration of (9 % 8)
getBarFill :: TimeSignature -> NoteDuration  -> TickableContext -> BarFill
getBarFill timeSignature unitNoteLength (TickableContext _ _ barNotesDuration) =
  if (barDuration == (fromInt 0))
    then 
      Empty 
  else if (barDuration == signatureDuration)
    then 
      Full
  else if (barDuration > signatureDuration)
    then 
      OverFull
  else  
    Partial

  where

  barDuration = unitNoteLength * barNotesDuration
  signatureDuration = timeSignature.numerator % timeSignature.denominator
