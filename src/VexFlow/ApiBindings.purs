module VexFlow.ApiBindings where

-- | Low to Medium level FFI into the VexFlow API

import Prelude

import Data.Abc (KeySignature)
import Data.Maybe (Maybe, maybe)
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

