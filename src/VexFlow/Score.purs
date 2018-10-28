module VexFlow.Score
  ( Stave
  , addTimeSignature
  , displayTune
  , displayFullStave
  , initialise
  , newStave) where

import Data.Abc (AbcTune, Accidental(..), BodyPart, KeySignature
     ,Mode(..), PitchClass(..), Repeat(..))
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Prelude ((<>), (+), (*), (==), Unit, bind, discard, pure, unit)
import VexFlow.Abc.Translate (keySignature) as Translate
import VexFlow.Abc.TranslateStateful (runBodyPart, runTuneBody)
import VexFlow.Types (AbcContext, BarSpec, Config, MusicSpec(..)
         , MusicSpecContents, NoteSpec, StaveConfig, StaveSpec, TimeSignature
         , staveWidth)
import VexFlow.Abc.ContextChange (ContextChange(..))
import VexFlow.Abc.Volta (Volta)
import VexFlow.Abc.Utils (initialAbcContext)


-- | a stave
foreign import data Stave :: Type

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

addKeySignature :: Stave -> KeySignature -> Effect Unit
addKeySignature stave ks =
  keySignatureImpl stave (Translate.keySignature ks)

{-}
staveConfig :: Int -> Int -> StaveConfig
staveConfig staveNo barNo =
  { x : 10 + (staveWidth * barNo)
  , y : 10 + (staveSeparation * staveNo)
  , width : staveWidth
  , barNo : barNo
  }
-}

staveConfig :: Int -> Int -> Int -> Int -> StaveConfig
staveConfig staveNo barNo xOffset width =
  { x : xOffset
  , y : 10 + (staveSeparation * staveNo)
  , width : width
  , barNo : barNo
  }


newStave :: StaveConfig -> KeySignature -> Effect Stave
newStave staveCnfg ks =
  newStaveImpl staveCnfg (Translate.keySignature ks)


displayTune :: AbcTune -> Effect Unit
displayTune abcTune =
  let
    abcContext = initialAbcContext abcTune
    eStaveSpecs :: Either String (Array (Maybe StaveSpec))
    eStaveSpecs = runTuneBody abcContext abcTune.body
  in
    case eStaveSpecs of
      Right staveSpecs ->
        traverse_ displayStaveSpec staveSpecs
      Left err -> do
        _ <- log ("error in translating tune  " <> err)
        pure unit


-- | display a full stave of music
-- | (in cases where the stave consists of actual music)
-- | Only used in single line display tests
displayFullStave :: AbcContext -> BodyPart -> Effect Unit
displayFullStave abcContext bodyPart =
  let
    emStaveSpec :: Either String (Maybe StaveSpec)
    emStaveSpec =
      runBodyPart abcContext bodyPart
  in
    case emStaveSpec of
      Right (Just staveSpec) ->
        traverse_ (displayBarSpec staveSpec.staveNo) staveSpec.barSpecs
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
      traverse_ (displayBarSpec staveSpec.staveNo) staveSpec.barSpecs
    _ ->
      -- the body part is merely a header - no display needed
      pure unit

{- Just for debug
-- | display all the bars in a stave of music
displayBarsStateless :: AbcContext -> Int -> List Bar -> Effect Unit
displayBarsStateless abcContext staveNo bars =
  let
    eBarSpecs :: Either String (Array BarSpec)
    eBarSpecs =
      sequence $ mapWithIndex (Translate.bar abcContext) (fromFoldable bars)
  in
    case eBarSpecs of
      Right barSpecs ->
        traverse_ (displayBarSpec staveNo) barSpecs
      Left err -> do
        _ <- log ("error in translating stave  " <> err)
        pure unit



displayBars :: AbcContext -> Int -> List Bar -> Effect Unit
displayBars abcContext staveNo bars =
  let
    eBarSpecs :: Either String (Array BarSpec)
    eBarSpecs =
      runBars abcContext bars
  in
    case eBarSpecs of
      Right barSpecs ->
        traverse_ (displayBarSpec abcContext staveNo) barSpecs
      Left err -> do
        _ <- log ("error in translating stave  " <> err)
        pure unit
-}


-- | display a single bar from the (translated) BarSpec
displayBarSpec :: Int -> BarSpec -> Effect Unit
displayBarSpec staveNo barSpec =
  let
    (MusicSpec musicSpec) = barSpec.musicSpec
    -- xOffset hard coded at the moment - fixed width bars
    -- xOffset = 10 + (staveWidth * barSpec.barNumber)
    -- now tracked in the BarSpec
    xOffset = barSpec.xOffset
  in
    do
      staveBar <- newStave (staveConfig staveNo barSpec.barNumber xOffset staveWidth) dMajor

      -- add any meter or key change markers
      traverse_ (displayContextChange staveBar) musicSpec.contextChanges

      if (barSpec.barNumber == 0)
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


{- Just for debug
-- | display a single bar of music
displayBarStateless :: AbcContext -> Int -> Int -> Bar -> Effect Unit
displayBarStateless abcContext staveNo barNo abcBar =
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
-}

{- not really needed now}
-- | ditto with state threading
displayBar :: AbcContext -> Int -> Int -> Bar -> Effect Unit
displayBar abcContext staveNo barNo abcBar =
  let
    eBarSpec :: Either String BarSpec
    eBarSpec = runBar abcContext barNo abcBar
  in
    case eBarSpec of
      Right barSpec ->
        displayBarSpec abcContext staveNo barSpec
      Left err ->
        do
          _ <- log ("error in translating stateful bar " <> err)
          pure unit
-}

{- not really used now
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
-}

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



foreign import initialise :: Config -> Effect Unit
foreign import newStaveImpl :: StaveConfig -> String -> Effect Stave
foreign import getStaveWidth :: Stave -> Effect Int
foreign import displayNotesImpl :: Stave -> Array NoteSpec -> Effect Unit
foreign import displayAutoBeamedNotesImpl :: Stave -> TimeSignature -> Int -> MusicSpecContents -> Effect Unit
foreign import displayTupletedNotesImpl :: Stave -> TimeSignature -> Int -> MusicSpecContents -> Effect Unit
foreign import displayStave :: Stave -> Effect Unit
foreign import displayBarBeginRepeat :: Stave -> Effect Unit
foreign import displayBarEndRepeat :: Stave -> Effect Unit
foreign import displayBarBothRepeat :: Stave -> Effect Unit
foreign import displayVolta :: Stave -> Volta -> Effect Unit
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
foreign import keySignatureImpl :: Stave -> String -> Effect Unit
