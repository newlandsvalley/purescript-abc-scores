module VexFlow.Score (Stave, addTimeSignature, displayMusics, displayStave, initialise, newStave) where

import Data.Either (Either(..))
import Data.Abc (KeySignature, Music)
import Data.Array (null)
import Effect.Console (log)
import Effect (Effect)
import Prelude ((<>), Unit, bind, pure, unit)
import VexFlow.Abc.Translate (keySignature, musics) as Translate
import VexFlow.Types (AbcContext, Config, MusicSpec, NoteSpec, StaveConfig, TimeSignature)


-- | a stave
foreign import data Stave :: Type

addTimeSignature :: Stave -> TimeSignature -> Effect Unit
addTimeSignature stave timeSignature =
  timeSignatureImpl stave timeSignature

newStave :: StaveConfig -> KeySignature -> Effect Stave
newStave staveConfig ks =
  newStaveImpl staveConfig (Translate.keySignature ks)

{-}
displayNotes :: AbcContext -> Boolean -> Stave -> Array AbcNote -> Effect Unit
displayNotes abcContext isAutoBeam stave abcNotes =
  let
    eNotes = Stringify.notes abcContext abcNotes
  in
    case eNotes of
      Right notes ->
        if (isAutoBeam) then
          displayAutoBeamedNotesImpl abcContext stave  notes
        else
          displayNotesImpl stave notes
      _ ->
        pure unit
-}

displayMusics :: AbcContext -> Boolean -> Stave -> Array Music -> Effect Unit
displayMusics abcContext isAutoBeam stave abcMusics =
  let
    eMusicSpec = Translate.musics abcContext abcMusics
  in
    case eMusicSpec of
      Right musicSpec ->
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
foreign import displayTupletedNotesImpl :: AbcContext -> Stave -> MusicSpec -> Effect Unit
foreign import displayStave :: Stave -> Effect Unit
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
