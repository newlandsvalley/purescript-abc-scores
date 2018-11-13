module VexFlow.Score
  ( Stave
  , addTimeSignature
  , createScore
  , renderScore
  , renderTune
  , renderFullStave
  , initialiseCanvas
  , newStave
  , clearCanvas) where

import Data.Abc (AbcTune, BodyPart, KeySignature, Repeat(..))
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Prelude ((<>), (*), (==), (&&), ($), Unit, bind, discard, pure, unit)
import VexFlow.Abc.Translate (keySignature) as Translate
import VexFlow.Abc.TranslateStateful (runBodyPart, runTuneBody)
import VexFlow.Types (AbcContext, BarSpec, Config, MusicSpec(..)
         , MusicSpecContents, StaveConfig, StaveSpec, TimeSignature
         , VexScore)
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Abc.Volta (Volta)
import VexFlow.Abc.Alignment (alignStaves)
import VexFlow.Abc.Utils (initialAbcContext)


-- | a stave
foreign import data Stave :: Type

staveSeparation :: Int
staveSeparation = 100

staveMargin :: Int
staveMargin = 10

addTimeSignature :: Stave -> TimeSignature -> Effect Unit
addTimeSignature stave timeSignature =
  timeSignatureImpl stave timeSignature

addKeySignature :: Stave -> KeySignature -> Effect Unit
addKeySignature stave ks =
  keySignatureImpl stave (Translate.keySignature ks)

staveConfig :: Int -> BarSpec -> StaveConfig
staveConfig staveNo barSpec=
  { x : barSpec.xOffset
  , y : staveSeparation * staveNo
  , width : barSpec.width
  , barNo : barSpec.barNumber
  , hasEndLine : barSpec.hasEndLine
  }

newStave :: StaveConfig -> KeySignature -> Effect Stave
newStave staveCnfg ks =
  newStaveImpl staveCnfg (Translate.keySignature ks)

-- | render the ABC tune with no right-alignment
renderTune :: Config -> AbcTune -> Effect Boolean
renderTune config abcTune =
  renderScore config false $ createScore config abcTune

-- | create a Vex Score from the ABC tune
createScore :: Config -> AbcTune -> VexScore
createScore config abcTune  =
  let
    abcContext = initialAbcContext abcTune config
  in
    runTuneBody abcContext abcTune.body

-- | render the Vex Score to the HTML page
-- | aligning on the RHS if required
renderScore :: Config -> Boolean -> VexScore -> Effect Boolean
renderScore config rightAlign eStaveSpecs  =
  case eStaveSpecs of
    Right staveSpecs -> do
      let
        alignedScore =
          if rightAlign then
            alignStaves config staveSpecs
          else
            staveSpecs
      _ <- traverse_ displayStaveSpec alignedScore
      pure true
    Left err -> do
      _ <- log ("error in translating tune  " <> err)
      pure false

-- | display a full stave of music
-- | (in cases where the stave consists of actual music)
-- | Only used in single line display tests
renderFullStave :: AbcContext -> BodyPart -> Effect Unit
renderFullStave abcContext bodyPart =
  let
    emStaveSpec :: Either String (Maybe StaveSpec)
    emStaveSpec =
      runBodyPart abcContext bodyPart
  in
    case emStaveSpec of
      Right (Just staveSpec) ->
        traverse_ (displayBarSpec staveSpec) staveSpec.barSpecs
      Right _ ->
        -- the body part is merely a header - no display needed
        pure unit
      Left err -> do
        _ <- log ("error in translating stave  " <> err)
        pure unit


displayStaveSpec :: Maybe StaveSpec -> Effect Unit
displayStaveSpec mStaveSpec =
  case mStaveSpec of
    (Just staveSpec) ->
      traverse_ (displayBarSpec staveSpec) staveSpec.barSpecs
    _ ->
      -- the body part is merely a header - no display needed
      pure unit

-- | display a single bar from the (translated) BarSpec
displayBarSpec :: StaveSpec -> BarSpec -> Effect Unit
displayBarSpec staveSpec barSpec =
  let
    (MusicSpec musicSpec) = barSpec.musicSpec
  in
    do
      staveBar <- newStave (staveConfig staveSpec.staveNo barSpec) staveSpec.keySignature

      -- add any inline meter or key change markers
      traverse_ (displayContextChange staveBar) musicSpec.contextChanges

      -- add a time signature to the first bar stave.  This only happensif it's
      -- stave 0 or if a BodyPart time sig header change has just occurred
      if (barSpec.barNumber == 0) && (staveSpec.isNewTimeSignature)
      --      if (barSpec.barNumber == 0) && (staveSpec.staveNo == 0)
        then
          addTimeSignature staveBar barSpec.timeSignature
        else
          pure unit

      _ <- processBarBeginRepeat staveBar barSpec.startLine.repeat
      _ <- processBarEndRepeat staveBar barSpec.endLineRepeat
      _ <- processVolta staveBar barSpec.volta

      if (null musicSpec.tuplets)
        then
          displayAutoBeamedNotesImpl staveBar barSpec.timeSignature barSpec.beatsPerBeam musicSpec
        else
          displayTupletedNotesImpl staveBar barSpec.timeSignature barSpec.beatsPerBeam musicSpec
      displayStave staveBar

-- | display bar begin repeat markers
processBarBeginRepeat :: Stave -> Maybe Repeat -> Effect Unit
processBarBeginRepeat staveBar mRepeat =
  case mRepeat of
    Just Begin ->
      displayBarBeginRepeat staveBar
    Just End ->
      -- we ignore this because we now record bar end repeat separately
      pure unit
    Just BeginAndEnd ->
      -- just show begin because we now record bar end repeat separately
      displayBarBeginRepeat staveBar
    _ ->
      pure unit

-- | display bar end repeat markers
processBarEndRepeat :: Stave -> Boolean -> Effect Unit
processBarEndRepeat staveBar isRepeat =
  if isRepeat then
    displayBarEndRepeat staveBar
  else
    pure unit


processVolta :: Stave -> Maybe Volta -> Effect Unit
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
      addKeySignature staveBar modifiedKeySignature.keySignature
    UnitNoteChange _ ->
      -- this has no immediate effect on the displayed stave
      pure unit

-- | initialise VexFlow against the canvas where it renders
foreign import initialiseCanvas :: Config -> Effect Unit
-- | clear the score from the canvas
foreign import clearCanvas :: Effect Unit
-- | create a new stave bar
foreign import newStaveImpl :: StaveConfig -> String -> Effect Stave
-- | get the width of a stave
foreign import getStaveWidth :: Stave -> Effect Int
-- | display the notes in a stave bar using auto-beaming
foreign import displayAutoBeamedNotesImpl :: Stave -> TimeSignature -> Int -> MusicSpecContents -> Effect Unit
-- | display the notes in a stave bar where tuplets exist in the bar
foreign import displayTupletedNotesImpl :: Stave -> TimeSignature -> Int -> MusicSpecContents -> Effect Unit
-- | display the (filled) bar
foreign import displayStave :: Stave -> Effect Unit
-- | dispay a bar begin repeat
foreign import displayBarBeginRepeat :: Stave -> Effect Unit
-- | dispay a bar begin repeat
foreign import displayBarEndRepeat :: Stave -> Effect Unit
-- | dispay a bar begin-and-end repeat
foreign import displayBarBothRepeat :: Stave -> Effect Unit
-- | display a Volta
foreign import displayVolta :: Stave -> Volta -> Effect Unit
-- | add the time signature
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
-- | add the key signature
foreign import keySignatureImpl :: Stave -> String -> Effect Unit
