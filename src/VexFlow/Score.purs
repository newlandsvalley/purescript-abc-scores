module VexFlow.Score
  ( Stave
  , addTimeSignature
  , displayBar
  , displayBars
  , displayBarBeginRepeat
  , displayMusics
  , displayStave
  , initialise
  , newStave) where

import Data.Abc (Accidental(..), Bar, KeySignature, Mode(..), Music, PitchClass(..), Repeat(..))
import Data.Array (null, mapWithIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse_)
import Effect (Effect)
import Effect.Console (log)
import Prelude (($), (<>), (+), (*), (==), Unit, bind, discard, pure, unit)
import VexFlow.Abc.Translate (bar, keySignature, musics) as Translate
import VexFlow.Types (AbcContext, BarSpec, Config, MusicSpec(..), MusicSpecContents, NoteSpec, StaveConfig, TimeSignature)


-- | a stave
foreign import data Stave :: Type

staveWidth :: Int
staveWidth = 250

staveSeparation :: Int
staveSeparation = 100

-- temporary constants until we can parse the key signature
dMajor :: KeySignature
dMajor =
  {  pitchClass : D
  ,  accidental : Natural
  ,  mode : Major
  }

addTimeSignature :: Stave -> TimeSignature -> Effect Unit
addTimeSignature stave timeSignature =
  timeSignatureImpl stave timeSignature

staveConfig :: Int -> Int -> StaveConfig
staveConfig staveNo barNo =
  { x : 10 + (staveWidth * barNo)
  , y : 10 + (staveSeparation * staveNo)
  , width : staveWidth
  , barNo : barNo
  }


newStave :: StaveConfig -> KeySignature -> Effect Stave
newStave staveCnfg ks =
  newStaveImpl staveCnfg (Translate.keySignature ks)

-- | display all the bars in a stave of music
displayBars :: AbcContext -> Int -> Array Bar -> Effect Unit
displayBars abcContext staveNo bars =
  let
    eBarSpecs :: Either String (Array BarSpec)
    eBarSpecs =
      sequence $ mapWithIndex (Translate.bar abcContext) bars
  in
    case eBarSpecs of
      Right barSpecs ->
        traverse_ (displayBarSpec abcContext staveNo) barSpecs
      Left err -> do
        _ <- log ("error in translating stave  " <> err)
        pure unit

-- | display a single bar of music
displayBar :: AbcContext -> Int -> Int -> Bar -> Effect Unit
displayBar abcContext staveNo barNo abcBar =
  let
    eBarSpec :: Either String BarSpec
    eBarSpec = Translate.bar abcContext barNo abcBar
  in
    case eBarSpec of
      Right barSpec ->
        displayBarSpec abcContext staveNo barSpec
      Left err ->
        do
          _ <- log ("error in translating bar " <> err)
          pure unit

-- | display a single bar from the (translated) BarSpec
displayBarSpec :: AbcContext -> Int -> BarSpec -> Effect Unit
displayBarSpec abcContext staveNo barSpec=
  let
    (MusicSpec musicSpec) = barSpec.musicSpec
  in
    do
      staveBar <- newStave (staveConfig staveNo barSpec.barNumber) dMajor
      if (barSpec.barNumber == 0)
        then
          addTimeSignature staveBar abcContext.timeSignature
        else
          pure unit

      if (barSpec.startLine.repeat == Just Begin)
        then
          displayBarBeginRepeat staveBar
        else
          pure unit

      if (null musicSpec.tuplets)
        then
          displayAutoBeamedNotesImpl abcContext staveBar musicSpec.noteSpecs
        else
          displayTupletedNotesImpl abcContext staveBar musicSpec
      displayStave staveBar


displayMusics :: AbcContext -> Stave -> Array Music -> Effect Unit
displayMusics abcContext stave abcMusics =
  let
    eMusicSpec = Translate.musics abcContext abcMusics
    isAutoBeam = true
  in
    case eMusicSpec of
      Right (MusicSpec musicSpec) ->
        if (isAutoBeam) then
          if (null musicSpec.tuplets) then
            displayAutoBeamedNotesImpl abcContext stave musicSpec.noteSpecs
          else
            displayTupletedNotesImpl abcContext stave musicSpec
        else
          displayNotesImpl stave musicSpec.noteSpecs
      Left err ->
        do
          _ <- log ("error in translating musics: " <> err)
          pure unit


foreign import initialise :: Config -> Effect Unit
foreign import newStaveImpl :: StaveConfig -> String -> Effect Stave
foreign import displayNotesImpl :: Stave -> Array NoteSpec -> Effect Unit
foreign import displayAutoBeamedNotesImpl :: AbcContext -> Stave -> Array NoteSpec -> Effect Unit
foreign import displayTupletedNotesImpl :: AbcContext -> Stave -> MusicSpecContents -> Effect Unit
foreign import displayStave :: Stave -> Effect Unit
foreign import displayBarBeginRepeat :: Stave -> Effect Unit
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
