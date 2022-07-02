module VexFlow.ApiBindings where

-- | Low to Medium level FFI into the VexFlow API

import Prelude

import Data.Abc (BarLine, KeySignature)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Abc.Translate (keySignature) as Translate
import Effect (Effect)
import VexFlow.Abc.Slur (VexCurves)
import VexFlow.Types (BeamSpec, Config, MusicSpecContents, StaveConfig, Tempo, TimeSignature)
import VexFlow.Abc.Volta (VexVolta)

-- | the Vex renderer
foreign import data Renderer :: Type
-- | a stave
foreign import data Stave :: Type

-- | create a new stave 
newStave :: StaveConfig -> String -> KeySignature -> Effect Stave
newStave staveCnfg clefString ks =
  newStaveImpl staveCnfg clefString (Translate.keySignature ks)

-- | render the tune title
renderTuneTitle :: Renderer -> String -> Int -> Int -> Effect Unit
renderTuneTitle renderer title x y =
  renderText renderer title " 25pt Arial" x y
 
-- | Add the tempo signature to the score is there is one
addTempoMarking :: Stave -> Maybe Tempo -> Effect Unit
addTempoMarking stave mTempo =
  maybe (pure unit) (addTempoMarkingImpl stave) mTempo

-- | display bar begin repeat markers

processBarBeginRepeat :: Stave -> BarLine -> Effect Unit
processBarBeginRepeat staveBar barLine =
  case barLine.startRepeats of
    0 ->
      pure unit
    1 ->
      displayBarBeginRepeat staveBar ""
    n ->
      displayBarBeginRepeat staveBar ("play " <> show (n + 1) <> " times")

-- | display bar end repeat markers
processBarEndRepeat :: Stave -> Boolean -> Effect Unit
processBarEndRepeat staveBar isRepeat =
  when isRepeat do
    displayBarEndRepeat staveBar

processVolta :: Stave -> Maybe VexVolta -> Effect Unit
processVolta staveBar mVolta =
  case mVolta of
    Just volta ->
      displayVolta staveBar volta
    _ ->
      pure unit

displayContextChange :: Stave -> ContextChange -> Effect Unit
displayContextChange staveBar contextChange =
  case contextChange of
    MeterChange (Tuple numerator denominator) ->
      addTimeSignature staveBar { numerator, denominator }
    KeyChange modifiedKeySignature ->
      -- note - this is dropping the modifications
      addKeySignature staveBar (Translate.keySignature modifiedKeySignature.keySignature)
    UnitNoteChange _ ->
      -- this has no immediate effect on the displayed stave
      pure unit
    ClefChange _clef ->
      -- perhaps we need to display it immediately but I doubt it 
      -- because voice headers cannot appear inline
      pure unit
 

-- | initialise VexFlow against the canvas where it renders
foreign import initialiseCanvas :: Config -> Effect Renderer
-- | resize the canvas
-- | we return the renderer to give the illusion that it's not operating by side-effect
foreign import resizeCanvas :: Renderer -> Config -> Effect Renderer
-- | clear the score from the canvas
foreign import clearCanvas :: Renderer -> Effect Unit
-- | create a new stave bar
foreign import newStaveImpl :: StaveConfig -> String -> String -> Effect Stave
-- | get the width of a stave
foreign import getStaveWidth :: Stave -> Effect Int
-- | display text on the canvas, but bypassing the VexFlow API
foreign import renderText :: Renderer -> String -> String -> Int -> Int -> Effect Unit
-- | display all the contents of the bar, using explicit beaming for the notes
foreign import renderBarContents :: Renderer -> Stave -> Array BeamSpec -> VexCurves -> MusicSpecContents -> Effect Unit
-- | display the (filled) bar
foreign import renderStave :: Renderer -> Stave -> Effect Unit
-- | dispay a bar begin repeat
foreign import displayBarBeginRepeat :: Stave -> String -> Effect Unit
-- | dispay a bar begin repeat
foreign import displayBarEndRepeat :: Stave -> Effect Unit
-- | dispay a bar begin-and-end repeat
foreign import displayBarBothRepeat :: Stave -> Effect Unit
-- | display a Volta
foreign import displayVolta :: Stave -> VexVolta -> Effect Unit
-- | add the time signature
foreign import addTimeSignature :: Stave -> TimeSignature -> Effect Unit
-- | add the key signature
foreign import addKeySignature :: Stave -> String -> Effect Unit
-- | add the tempo signature
foreign import addTempoMarkingImpl :: Stave -> Tempo -> Effect Unit

