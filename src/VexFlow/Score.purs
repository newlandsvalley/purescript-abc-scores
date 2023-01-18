module VexFlow.Score
  ( createScore
  , renderUntitledScore
  , renderTitledScore
  , renderTune
  , renderFinalTune
  , renderRightAlignedTune
  , renderThumbnail
  , staveConfig
  , renderTuneAtStave
  , renderTitle
  , renderComposerAndOrigin
  , setCanvasDepthToTune
  , setCanvasDimensionsToScore
  , module Exports
  , module API
  ) where

import Data.Abc (AbcTune, BodyPart(..))
import Data.Abc.Utils (getTitle, isEmptyStave, thumbnail)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Int (floor, toNumber)
import Data.List (filter, length)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (length) as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Prelude ((<>), (*), (==), (/=), (&&), ($), (+), (>), Unit, bind, discard, not, pure, when)
import VexFlow.Abc.Alignment (centeredTitleXPos, justifiedScoreConfig, rightJustifiedOriginXPos)
import VexFlow.Abc.Alignment (rightJustify) as Exports
import VexFlow.Abc.TranslateStateful (runTuneBody)
import VexFlow.Abc.Utils (getComposerAndOrigin, initialAbcContext)
import VexFlow.ApiBindings
import VexFlow.ApiBindings (Renderer, Stave, clearCanvas, initialiseCanvas, renderTuneOrigin, renderText, renderTuneTitle, resizeCanvas) as API
import VexFlow.Types (BarSpec, Config, LineThickness(..), MusicSpec(..), RenderingError, StaveConfig, StaveSpec, VexScore, scoreMarginBottom, staveSeparation, originYPos, titleDepth, titleYPos)

-- | configure a new stave at appropriate coordinates and with appropriate furnishings
staveConfig :: Int -> Boolean -> BarSpec -> StaveConfig
staveConfig staveNo isTitled barSpec =
  let
    titleVerticalDepth =
      if isTitled then titleDepth else 0
  in
    { x: barSpec.xOffset
    , y: (staveSeparation * staveNo) + titleVerticalDepth
    , width: barSpec.width
    , barNo: barSpec.barNumber
    , lineColour: "#1a1a1a" -- vexflow default seems to be a Dark Slate Gray - #999999
    , hasRightBar: (barSpec.endLineThickness /= NoLine)
    , hasDoubleRightBar: (barSpec.endLineThickness == Double)
    }

-- | render the ABC tune, possibly titled (if indicated by the config), 
-- | but unjustified and with an expansive canvas
renderTune :: Config -> Renderer -> AbcTune -> Effect (Maybe RenderingError)
renderTune config renderer abcTune =
  if (config.titled) then
    renderTitledScore config renderer abcTune $ createScore config abcTune
  else
    renderUntitledScore renderer $ createScore config abcTune

-- | render the final ABC tune, possibly titled (if indicated by the config)
-- | and with the staves aligned at the right hand side.
-- | There is no change to the canvas dimensions themselves
renderRightAlignedTune :: Config -> Renderer -> AbcTune -> Effect (Maybe RenderingError)
renderRightAlignedTune config renderer abcTune =
  let
    unjustifiedScore = createScore config abcTune
    score = Exports.rightJustify config.width config.scale unjustifiedScore
  in
    if (config.titled) then renderTitledScore config renderer abcTune score
    else renderUntitledScore renderer score

-- | render the final ABC tune, possibly titled( if indicated by the config),
-- | justified and with canvas clipped to tune size
renderFinalTune :: Config -> Renderer -> AbcTune -> Effect (Maybe RenderingError)
renderFinalTune config renderer abcTune =
  let
    unjustifiedScore = createScore config abcTune
    score = Exports.rightJustify config.width config.scale unjustifiedScore
    config' = justifiedScoreConfig score config
  in
    -- don't render if the tune width exceded the requested canvas width
    if (config'.width > config.width) then 
      pure $ Just "Canvas width exceded"
    else do
      _ <- resizeCanvas renderer config'
      if (config'.titled) then 
        renderTitledScore config' renderer abcTune score
      else 
        renderUntitledScore renderer score

-- | render a thumbnail of the first few bars of the tune with the canvas 
-- | clipped to the thumbnail boundary.
renderThumbnail :: Config -> Renderer -> AbcTune -> Effect (Maybe RenderingError)
renderThumbnail config renderer abcTune =
  let
    unjustifiedScore = createScore config (thumbnail abcTune)
    score = Exports.rightJustify config.width config.scale unjustifiedScore
    config' = justifiedScoreConfig score config
  in
    do
      _ <- clearCanvas renderer
      _ <- resizeCanvas renderer config'
      renderUntitledScore renderer score

-- | render the tune but at the required stave number
-- | useful for examples
renderTuneAtStave :: Int -> Config -> Renderer -> AbcTune -> Effect (Maybe RenderingError)
renderTuneAtStave staveNo config renderer abcTune =
  renderUntitledScore renderer $ createScoreAtStave staveNo config abcTune

-- | @deprecated in favour of renderTune, renderFinalTune or renderThumbnail
-- | create a Vex Score from the ABC tune
createScore :: Config -> AbcTune -> VexScore
createScore config abcTune =
  case (initialAbcContext abcTune config) of
    Left error ->
      Left error
    Right abcContext ->
      runTuneBody abcContext abcTune.body

-- | create a Vex Score from the ABC tune but output at the required
-- | stave number.  Useful for examples.
createScoreAtStave :: Int -> Config -> AbcTune -> VexScore
createScoreAtStave staveNo config abcTune =
  case (initialAbcContext abcTune config) of
    Left error ->
      Left error
    Right abcContext ->
      runTuneBody (abcContext { staveNo = Just staveNo }) abcTune.body

-- | Render the untitled Vex Score to the HTML score div
-- | Although exported, it is not intended to be used by client applications -
-- | prefer renderTune
renderUntitledScore :: Renderer -> VexScore -> Effect (Maybe RenderingError)
renderUntitledScore renderer eStaveSpecs =
  case eStaveSpecs of
    Right staveSpecs -> do
      _ <- traverse_ (displayStaveSpec renderer false) staveSpecs
      pure Nothing
    Left err -> 
      pure $ Just ("error in producing score: " <> err)

-- | Render the titled Vex Score to the HTML score div
-- | This function works out the titling and other metadata and renders the final score.  
-- | Although exported, it is not intended to be used by client applications 
-- | Prefer renderTune or renderFinalTune (or renderThumbnail for thumbnails)

renderTitledScore :: Config -> Renderer -> AbcTune -> VexScore -> Effect (Maybe RenderingError)
renderTitledScore config renderer tune eStaveSpecs =
  case eStaveSpecs of
    Right staveSpecs -> do
      _ <- renderTitle config renderer tune
      _ <- renderComposerAndOrigin config renderer tune
      _ <- traverse_ (displayStaveSpec renderer true) staveSpecs    
      pure Nothing
    Left err -> 
      pure $ Just ("error in producing score: " <> err)

displayStaveSpec :: Renderer -> Boolean -> StaveSpec -> Effect Unit
displayStaveSpec renderer isTitled staveSpec =
  traverse_ (displayBarSpec renderer staveSpec isTitled) staveSpec.barSpecs

-- | display a single bar from the (translated) BarSpec
displayBarSpec :: Renderer -> StaveSpec -> Boolean -> BarSpec -> Effect Unit
displayBarSpec renderer staveSpec isTitled barSpec =
  let
    (MusicSpec musicSpec) = barSpec.musicSpec
    staveBarConfig = staveConfig staveSpec.staveNo isTitled barSpec
  in
    do
      staveBar <- newStave staveBarConfig staveSpec.clefString staveSpec.keySignature

      -- add any inline meter or key change markers
      traverse_ (displayContextChange staveBar) musicSpec.contextChanges

      -- add a time signature to the first bar stave.  This only happens if it's
      -- stave 0 or if a BodyPart time sig header change has just occurred
      when ((barSpec.barNumber == 0) && (staveSpec.isNewTimeSignature)) do
        addTimeSignature staveBar barSpec.timeSignature

      -- and add a tempo marking if one is present in the ABC
      when ((barSpec.barNumber == 0) && (staveSpec.staveNo == 0)) do
        addTempoMarking staveBar staveSpec.mTempo

      _ <- processBarBeginRepeat staveBar barSpec.startLine
      _ <- processBarEndRepeat staveBar barSpec.endLineRepeat
      _ <- processVolta staveBar barSpec.volta
      -- only process the notes if we have some
      when (not $ null musicSpec.noteSpecs) $
        renderBarContents renderer staveBar barSpec.beamSpecs barSpec.curves musicSpec
      renderStave renderer staveBar


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


-- | render the title
renderTitle :: Config -> Renderer -> AbcTune -> Effect Unit
renderTitle config renderer tune = do
  let
    title = fromMaybe "untitled" $ getTitle tune
    titleXPos = centeredTitleXPos config (String.length title)
  renderTuneTitle renderer title titleXPos titleYPos

-- | render the Composer and origin (if either or both are present)
renderComposerAndOrigin :: Config -> Renderer -> AbcTune -> Effect Unit
renderComposerAndOrigin config renderer tune = do
  let 
    origin = getComposerAndOrigin tune
    originLength = maybe 0 String.length origin
    originXPos = rightJustifiedOriginXPos config originLength  
  renderTuneOrigin renderer origin originXPos originYPos



