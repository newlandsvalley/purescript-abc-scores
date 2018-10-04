module VexFlow.Score (Stave, addTimeSignature, displayNotes, displayStave, initialise, newStave) where

import Data.Either (Either(..))
import Data.Abc (KeySignature, AbcNote)
import Effect (Effect)
import Prelude (Unit, pure, unit)
import VexFlow.Abc.Stringify (keySignature, notes) as Stringify
import VexFlow.Types (AbcContext, Config, NoteSpec, StaveConfig, TimeSignature, VexNote)


-- | a stave
foreign import data Stave :: Type

addTimeSignature :: Stave -> TimeSignature -> Effect Unit
addTimeSignature stave timeSignature =
  timeSignatureImpl stave timeSignature

newStave :: StaveConfig -> KeySignature -> Effect Stave
newStave staveConfig ks =
  newStaveImpl staveConfig (Stringify.keySignature ks)

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


foreign import initialise :: Config -> Effect Unit
foreign import newStaveImpl :: StaveConfig -> String -> Effect Stave
foreign import displayNotesImpl :: Stave -> Array NoteSpec -> Effect Unit
foreign import displayAutoBeamedNotesImpl :: AbcContext -> Stave -> Array NoteSpec -> Effect Unit
foreign import displayStave :: Stave -> Effect Unit
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
