module VexFlow.Score
  ( Stave
  , addTimeSignature
  , displayTune
  , displayFullStave
  , displayBars
  , initialise
  , newStave) where

import Data.Abc (AbcTune, Accidental(..), Bar, BodyPart, KeySignature
     ,Mode(..), Music, PitchClass(..), Repeat(..))
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse_)
import Data.List (List)
import Effect (Effect)
import Effect.Console (log)
import Prelude (($), (<>), (+), (*), (==), Unit, bind, discard, pure, unit)
import VexFlow.Abc.Translate (bar, keySignature, musics) as Translate
import VexFlow.Abc.TranslateStateful (runBar, runBars, runBodyPart, runTuneBody)
import VexFlow.Types (AbcContext, BarSpec, Config, MusicSpec(..)
         , MusicSpecContents, NoteSpec, StaveConfig, StaveSpec, TimeSignature
         , staveWidth)
import VexFlow.Abc.ContextChange (ContextChange(..))
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
        traverse_ (displayStaveSpec abcContext) staveSpecs
      Left err -> do
        _ <- log ("error in translating tune  " <> err)
        pure unit

-- | display a full stave of music
-- | (in cases where the stave consists of actual music)
displayFullStave :: AbcContext -> BodyPart -> Effect Unit
displayFullStave abcContext bodyPart =
  let
    emStaveSpec :: Either String (Maybe StaveSpec)
    emStaveSpec =
      runBodyPart abcContext bodyPart
  in
    case emStaveSpec of
      Right (Just staveSpec) ->
        traverse_ (displayBarSpec abcContext staveSpec.staveNo) staveSpec.barSpecs
      Right _ ->
        -- the body part is merely a header - no display needed
        pure unit
      Left err -> do
        _ <- log ("error in translating stave  " <> err)
        pure unit

displayStaveSpec :: AbcContext -> Maybe StaveSpec -> Effect Unit
displayStaveSpec abcContext mStaveSpec =
  case mStaveSpec of
    (Just staveSpec) ->
      traverse_ (displayBarSpec abcContext staveSpec.staveNo) staveSpec.barSpecs
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
        traverse_ (displayBarSpec abcContext staveNo) barSpecs
      Left err -> do
        _ <- log ("error in translating stave  " <> err)
        pure unit
-}


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


-- | display a single bar from the (translated) BarSpec
displayBarSpec :: AbcContext -> Int -> BarSpec -> Effect Unit
displayBarSpec abcContext staveNo barSpec =
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
      traverse_ (displayContextChange abcContext staveBar) musicSpec.contextChanges

      if (barSpec.barNumber == 0)
        then
          addTimeSignature staveBar abcContext.timeSignature
        else
          pure unit

      if (barSpec.startLine.repeat == Just Begin)
        then
          displayBarBeginRepeat staveBar
        else
          pure unit

      if (null musicSpec.tuplets)
        then
          displayAutoBeamedNotesImpl abcContext staveBar musicSpec.noteSpecs
        else
          displayTupletedNotesImpl abcContext staveBar musicSpec
      displayStave staveBar



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

displayContextChange :: AbcContext -> Stave -> ContextChange -> Effect Unit
displayContextChange abcContext staveBar contextChange =
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
foreign import displayAutoBeamedNotesImpl :: AbcContext -> Stave -> Array NoteSpec -> Effect Unit
foreign import displayTupletedNotesImpl :: AbcContext -> Stave -> MusicSpecContents -> Effect Unit
foreign import displayStave :: Stave -> Effect Unit
foreign import displayBarBeginRepeat :: Stave -> Effect Unit
foreign import timeSignatureImpl :: Stave -> TimeSignature -> Effect Unit
foreign import keySignatureImpl :: Stave -> String -> Effect Unit
