module VexFlow.Score
  ( Renderer
  , Stave
  , createScore
  , renderUntitledScore
  , renderTitledScore
  , renderTune
  , renderFinalTune
  , renderThumbnail
  , renderTuneAtStave
  , initialiseCanvas
  , resizeCanvas
  , newStave
  , clearCanvas
  , setCanvasDepthToTune
  , setCanvasDimensionsToScore
  , module Exports) where

import Data.Abc (AbcTune, BarLine, BodyPart(..), KeySignature)
import Data.Abc.Metadata (getTitle, isEmptyStave, thumbnail)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.List (filter, length)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude ((<>), (*), (==), (/=), (&&), ($), (+), Unit, bind, discard, div, identity, not, pure, show, unit, when)
import VexFlow.Abc.Alignment (justifiedScoreConfig)
import VexFlow.Abc.Alignment (rightJustify) as Exports
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Abc.Slur (VexCurves)
import VexFlow.Abc.Translate (keySignature) as Translate
import VexFlow.Abc.TranslateStateful (runTuneBody)
import VexFlow.Abc.Utils (initialAbcContext)
import VexFlow.Abc.Volta (VexVolta)
import VexFlow.Types (BarSpec, BeamSpec, Config, LineThickness(..), MusicSpec(..), 
       MusicSpecContents, StaveConfig, StaveSpec, Tempo, TimeSignature, VexScore, 
       scoreMarginBottom, staveIndentation, staveSeparation, titleDepth)

-- | the Vex renderer
foreign import data Renderer :: Type
-- | a stave
foreign import data Stave :: Type

staveConfig :: Int -> Boolean -> BarSpec -> StaveConfig
staveConfig staveNo isTitled barSpec=
  let 
    titleVerticalDepth =  
      if isTitled then titleDepth else 0
  in
    { x : barSpec.xOffset
    , y : (staveSeparation * staveNo) + titleVerticalDepth
    , width : barSpec.width
    , barNo : barSpec.barNumber
    , lineColour : "#1a1a1a"       -- vexflow default seems to be a Dark Slate Gray - #999999
    , hasRightBar : (barSpec.endLineThickness /= NoLine)
    , hasDoubleRightBar : (barSpec.endLineThickness == Double)
    }

newStave :: StaveConfig -> KeySignature -> Effect Stave
newStave staveCnfg ks =
  newStaveImpl staveCnfg (Translate.keySignature ks)

-- | render the ABC tune, possibly titled (if indicated by the config), 
-- | but unjustified and with an expansive canvas
renderTune :: Config -> Renderer -> AbcTune -> Effect Boolean
renderTune config renderer abcTune =
  if (config.titled) then
    let
      title = maybe "Untitled" identity $ getTitle abcTune 
    in
      renderTitledScore renderer title $ createScore config abcTune
  else
    renderUntitledScore renderer $ createScore config abcTune

-- | render the final ABC tune, possibly titled( if indicated by the config),
-- | justified and with canvas clipped to tune size
renderFinalTune :: Config -> Renderer -> AbcTune -> Effect Boolean
renderFinalTune config renderer abcTune = 
  let 
    unjustifiedScore = createScore config abcTune
    score = Exports.rightJustify config.width config.scale unjustifiedScore
    config' = justifiedScoreConfig score config
    title = maybe "Untitled" identity $ getTitle abcTune 
  in 
    do
      _ <- resizeCanvas renderer config'
      if (config.titled)
        then renderTitledScore renderer title score
        else renderUntitledScore renderer score

renderThumbnail :: Config -> Renderer -> AbcTune -> Effect Boolean
renderThumbnail config renderer abcTune =   
  let
    unjustifiedScore = createScore config (thumbnail abcTune)
    score = Exports.rightJustify config.width config.scale unjustifiedScore
    config' = justifiedScoreConfig score config
  in do
    _ <- clearCanvas renderer
    _ <- resizeCanvas renderer config'
    renderUntitledScore renderer score     

-- | render the tune but at the required stave number
-- | useful for examples
renderTuneAtStave :: Int -> Config -> Renderer -> AbcTune -> Effect Boolean
renderTuneAtStave staveNo config renderer abcTune =
  renderUntitledScore renderer $ createScoreAtStave staveNo config abcTune

-- | @deprecated in favour of renderTune, renderFinalTune or renderThumbnail
-- | create a Vex Score from the ABC tune
createScore :: Config -> AbcTune -> VexScore
createScore config abcTune  =
  case (initialAbcContext abcTune config) of
    Left error ->
      Left error
    Right abcContext ->
      runTuneBody abcContext abcTune.body

-- | create a Vex Score from the ABC tune but output at the required
-- | stave number.  Useful for examples.
createScoreAtStave :: Int -> Config -> AbcTune -> VexScore
createScoreAtStave staveNo config abcTune  =
  case (initialAbcContext abcTune config) of
    Left error ->
      Left error
    Right abcContext ->
      runTuneBody (abcContext { staveNo = Just staveNo }) abcTune.body

-- | @deprecated in favour of renderTune, renderFinalTune or renderThumbnail
-- | render the untitled Vex Score to the HTML score div
renderUntitledScore :: Renderer -> VexScore -> Effect Boolean
renderUntitledScore renderer eStaveSpecs = 
  case eStaveSpecs of
    Right staveSpecs -> do
      _ <- traverse_ (displayStaveSpec renderer false) staveSpecs
      pure true
    Left err -> do
      _ <- log ("error in producing score: " <> err)
      pure false

-- | @deprecated in favour of renderTune, renderFinalTune or renderThumbnail
-- | render the Vex Score to the HTML score div
renderTitledScore :: Renderer -> String -> VexScore -> Effect Boolean
renderTitledScore renderer title eStaveSpecs =
  case eStaveSpecs of
    Right staveSpecs -> do
      let  
        yPos :: Int
        yPos = div (titleDepth * 3) 4
      _ <- renderTuneTitle renderer title staveIndentation yPos
      _ <- traverse_ (displayStaveSpec renderer true) staveSpecs
      pure true
    Left err -> do
      _ <- log ("error in producing score: " <> err)
      pure false      

displayStaveSpec :: Renderer -> Boolean -> Maybe StaveSpec -> Effect Unit
displayStaveSpec renderer isTitled mStaveSpec =
  case mStaveSpec of
    (Just staveSpec) ->
      traverse_ (displayBarSpec renderer staveSpec isTitled) staveSpec.barSpecs
    _ ->
      -- the body part is merely a header - no display needed
      pure unit

-- | display a single bar from the (translated) BarSpec
displayBarSpec :: Renderer -> StaveSpec -> Boolean ->  BarSpec -> Effect Unit
displayBarSpec renderer staveSpec isTitled barSpec =
  let
    (MusicSpec musicSpec) = barSpec.musicSpec
  in
    do
      staveBar <- newStave (staveConfig staveSpec.staveNo isTitled barSpec) staveSpec.keySignature

      -- add any inline meter or key change markers
      traverse_ (displayContextChange staveBar) musicSpec.contextChanges

      -- add a time signature to the first bar stave.  This only happensif it's
      -- stave 0 or if a BodyPart time sig header change has just occurred
      if (barSpec.barNumber == 0) && (staveSpec.isNewTimeSignature)
        then
          addTimeSignature staveBar barSpec.timeSignature
        else
          pure unit

      -- and add a tempo marking if one is present in the ABC
      if (barSpec.barNumber == 0) && (staveSpec.staveNo == 0)
        then
          addTempoMarking staveBar staveSpec.mTempo
        else
          pure unit

      _ <- processBarBeginRepeat staveBar barSpec.startLine
      _ <- processBarEndRepeat staveBar barSpec.endLineRepeat
      _ <- processVolta staveBar barSpec.volta
      -- only process the notes if we have some
      when (not $ null musicSpec.noteSpecs) $
        renderBarContents renderer staveBar barSpec.beamSpecs barSpec.curves musicSpec
      renderStave renderer staveBar

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
  if isRepeat then
    displayBarEndRepeat staveBar
  else
    pure unit


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
      addTimeSignature staveBar { numerator, denominator}
    KeyChange modifiedKeySignature ->
      -- note - this is dropping the modifications
      addKeySignature staveBar (Translate.keySignature modifiedKeySignature.keySignature)
    UnitNoteChange _ ->
      -- this has no immediate effect on the displayed stave
      pure unit

-- | Add the tempo signature to the score is there is one
addTempoMarking :: Stave -> Maybe Tempo -> Effect Unit
addTempoMarking stave mTempo =
  maybe (pure unit) (addTempoMarkingImpl stave) mTempo

-- | set the canvas depth used by the renderer to an appropriate amount 
-- | governed by the number of (non-empty) score lines in the tune
setCanvasDepthToTune :: AbcTune -> Config -> Renderer -> Effect Renderer
setCanvasDepthToTune tune config renderer =
  let 
    f :: BodyPart -> Boolean 
    f = case _ of 
      Score bars -> not $ isEmptyStave bars
      BodyInfo _ -> false
    scoreLines = length $ filter f tune.body
    pixels = floor $ toNumber ((scoreLines * staveSeparation) + scoreMarginBottom) * config.scale
  in 
    resizeCanvas renderer config { height = pixels }

-- | set the canvas dimensions used by the renderer to an appropriate depth and width
-- | (this requires us to have first generated the score from the tune)
setCanvasDimensionsToScore :: VexScore -> Config -> Renderer -> Effect Renderer 
setCanvasDimensionsToScore score config renderer = 
  let 
    justifiedConfig = justifiedScoreConfig score config 
  in 
    resizeCanvas renderer justifiedConfig

-- | initialise VexFlow against the canvas where it renders
foreign import initialiseCanvas :: Config -> Effect Renderer
-- | resize the canvas
-- | we return the renderer to give the illusion that it's not operating by side-effect
foreign import resizeCanvas :: Renderer -> Config -> Effect Renderer
-- | clear the score from the canvas
foreign import clearCanvas :: Renderer -> Effect Unit
-- | create a new stave bar
foreign import newStaveImpl :: StaveConfig -> String -> Effect Stave
-- | get the width of a stave
foreign import getStaveWidth :: Stave -> Effect Int
-- | display the tune title
foreign import renderTuneTitle :: Renderer -> String -> Int -> Int -> Effect Unit
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
