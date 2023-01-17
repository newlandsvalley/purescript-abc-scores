module VexFlow.ApiBindings 
  ( Renderer
  , Stave
  , clearCanvas
  , initialiseCanvas
  , resizeCanvas 
  , displayContextChange
  , newStave
  , getStaveWidth
  , renderStave
  , renderBarContents
  , processBarBeginRepeat
  , processBarEndRepeat
  , displayBarBothRepeat
  , processVolta
  , addTimeSignature
  , addTempoMarking
  , renderText
  , renderTuneOrigin   
  , renderTuneTitle    
  ) where

-- | Low to Medium level FFI into the VexFlow API

import Prelude

import Data.Abc (BarLine, KeySignature, TimeSignature)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Abc.Slur (VexCurves)
import VexFlow.Abc.Translate (keySignature) as Translate
import VexFlow.Abc.Volta (VexVolta)
import VexFlow.Types (BeamSpec, Config, MusicSpecContents, StaveConfig, Tempo)

-- | the Vex renderer
foreign import data Renderer :: Type
-- | a stave
foreign import data Stave :: Type

-- | create a new stave 
newStave :: StaveConfig -> String -> KeySignature -> Effect Stave
newStave staveCnfg clefString ks =
  makeStave staveCnfg clefString (Translate.keySignature ks)

-- | render the tune title
renderTuneTitle :: Renderer -> String -> Int -> Int -> Effect Unit
renderTuneTitle renderer title x y =
  renderText renderer title " 25pt Arial" x y

-- | render the tune origin and/or composer (if present).
-- | Here we follow folkwiki's example which depends on when we see in the 
-- | C (composer) and O (origin) headers:
-- | ```purescript
-- | composer
-- | origin 
-- | composer (origin)
-- | ```
renderTuneOrigin :: Renderer -> Maybe String -> Int -> Int -> Effect Unit
renderTuneOrigin renderer mOrigin x y =
  case (mOrigin) of 
    Just origin -> 
      renderText renderer origin " italic 18pt Arial" x y
    _ -> 
      pure unit
 
-- | Add the tempo signature to the score is there is one
addTempoMarking :: Stave -> Maybe Tempo -> Effect Unit
addTempoMarking stave mTempo =
  maybe (pure unit) (addTempoSignature stave) mTempo

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

-- | display a Volta
processVolta :: Stave -> Maybe VexVolta -> Effect Unit
processVolta staveBar mVolta =
  case mVolta of
    Just volta ->
      displayVolta staveBar volta
    _ ->
      pure unit

-- display a context change (of meter or key)
displayContextChange :: Stave -> ContextChange -> Effect Unit
displayContextChange staveBar contextChange =
  case contextChange of
    MeterChange timeSignature ->
      addTimeSignature staveBar timeSignature
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
initialiseCanvas :: Config -> Effect Renderer
initialiseCanvas = runEffectFn1 initialiseCanvasImpl

foreign import initialiseCanvasImpl :: EffectFn1 Config Renderer 

-- | resize the canvas
-- | we return the renderer to give the illusion that it's not operating by side-effect
resizeCanvas :: Renderer -> Config -> Effect Renderer
resizeCanvas = runEffectFn2 resizeCanvasImpl

foreign import resizeCanvasImpl :: EffectFn2 Renderer Config Renderer

-- | clear the score from the canvas
clearCanvas :: Renderer -> Effect Unit
clearCanvas = runEffectFn1 clearCanvasImpl

foreign import clearCanvasImpl :: EffectFn1 Renderer Unit

-- | create a new stave bar
makeStave :: StaveConfig -> String -> String -> Effect Stave
makeStave = runEffectFn3 makeStaveImpl

foreign import makeStaveImpl :: EffectFn3 StaveConfig String String Stave 

-- | get the width of a stave
getStaveWidth :: Stave -> Effect Int
getStaveWidth = runEffectFn1 getStaveWidthImpl

foreign import getStaveWidthImpl :: EffectFn1 Stave Int

-- | display text on the canvas, but bypassing the VexFlow API
renderText :: Renderer -> String -> String -> Int -> Int -> Effect Unit
renderText = runEffectFn5 renderTextImpl

foreign import renderTextImpl :: EffectFn5 Renderer String String Int Int Unit

-- | display all the contents of the bar, using explicit beaming for the notes
renderBarContents :: Renderer -> Stave -> Array BeamSpec -> VexCurves -> MusicSpecContents -> Effect Unit
renderBarContents = runEffectFn5 renderBarContentsImpl

foreign import renderBarContentsImpl :: EffectFn5 Renderer Stave (Array BeamSpec) VexCurves MusicSpecContents Unit

-- | display the (filled) bar
renderStave :: Renderer -> Stave -> Effect Unit
renderStave = runEffectFn2 renderStaveImpl

foreign import renderStaveImpl :: EffectFn2 Renderer Stave Unit

-- | dispay a bar begin repeat
displayBarBeginRepeat :: Stave -> String -> Effect Unit
displayBarBeginRepeat = runEffectFn2 displayBarBeginRepeatImpl

foreign import displayBarBeginRepeatImpl :: EffectFn2 Stave String Unit

-- | dispay a bar begin repeat
displayBarEndRepeat :: Stave -> Effect Unit
displayBarEndRepeat = runEffectFn1 displayBarEndRepeatImpl

foreign import displayBarEndRepeatImpl :: EffectFn1 Stave Unit

-- | dispay a bar begin-and-end repeat
-- | (I don't think this is used in abc-scores)
displayBarBothRepeat :: Stave -> Effect Unit
displayBarBothRepeat = runEffectFn1 displayBarBothRepeatImpl

foreign import displayBarBothRepeatImpl :: EffectFn1 Stave Unit

-- | display a Volta
displayVolta :: Stave -> VexVolta -> Effect Unit
displayVolta = runEffectFn2 displayVoltaImpl 

foreign import displayVoltaImpl :: EffectFn2 Stave VexVolta Unit

-- | add the time signature
addTimeSignature :: Stave -> TimeSignature -> Effect Unit
addTimeSignature = runEffectFn2 addTimeSignatureImpl

foreign import addTimeSignatureImpl :: EffectFn2 Stave TimeSignature Unit

-- | add the key signature
addKeySignature :: Stave -> String -> Effect Unit
addKeySignature = runEffectFn2 addKeySignatureImpl

foreign import addKeySignatureImpl ::EffectFn2 Stave String Unit

-- | add the tempo signature
addTempoSignature :: Stave -> Tempo -> Effect Unit
addTempoSignature = runEffectFn2 addTempoSignatureImpl

foreign import addTempoSignatureImpl :: EffectFn2 Stave Tempo Unit


