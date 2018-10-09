module VexFlow.Score (Stave, addTimeSignature, displayBar, displayMusics, displayStave, initialise, newStave) where

import Data.Abc (Accidental(..), Bar, KeySignature, Mode(..), Music, PitchClass(..))
import Data.Array (null)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude ((<>), (+), (*), Unit, bind, discard, pure, unit)
import VexFlow.Abc.Translate (bar, keySignature, musics) as Translate
import VexFlow.Types (AbcContext, Config, MusicSpec(..), MusicSpecContents, NoteSpec, StaveConfig, TimeSignature)


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

-- | display a single bar of music
displayBar :: AbcContext -> Int -> Int -> Bar -> Effect Unit
displayBar abcContext staveNo barNo abcBar =
  let
    eBarSpec = Translate.bar abcContext barNo abcBar
  in
    case eBarSpec of
      Right barSpec ->
        let
          (MusicSpec musicSpec) = barSpec.musicSpec
        in
          do
            staveBar <- newStave (staveConfig staveNo barNo) dMajor
            _ <- addTimeSignature staveBar abcContext.timeSignature
            if (null musicSpec.tuplets)
              then
                displayAutoBeamedNotesImpl abcContext staveBar musicSpec.noteSpecs
              else
                displayTupletedNotesImpl abcContext staveBar musicSpec
            displayStave staveBar
      Left err ->
        do
          _ <- log ("error in translating bar " <> err)
          pure unit


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
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
