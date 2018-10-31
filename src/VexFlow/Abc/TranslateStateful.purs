module VexFlow.Abc.TranslateStateful
  ( runBodyPart
  , runTuneBody
  , execBodyPart)  where

-- The stateful part of translation
-- The translation wrapped in a State Monad which is inside ExceptT
-- We need to thread the AbcContext throughout the translation because
-- Any change in headers for time signature, key signature or unit note length
-- will alter the state.

import Prelude (($), (<>), (+), (==), bind, mempty, pure, show)
import Control.Monad.Except.Trans
import Control.Monad.State (State, evalStateT, execStateT, get, put)
import VexFlow.Abc.Translate (headerChange, music) as Trans
import Data.Either (Either, either)
import Data.Foldable (foldM, foldl)
import Data.List (List, toUnfoldable, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Array ((..), zip)
import Data.Array (length) as Array
import Data.Traversable (traverse)
import Data.Abc (Bar, BodyPart(..), Music)
import Data.Abc.Metadata (isEmptyStave)
import VexFlow.Abc.Utils (applyContextChanges, nextStaveNo, updateAbcContext
                         ,isEmptyMusicSpec)
import VexFlow.Types (AbcContext, BarSpec, MusicSpec(..), StaveSpec
      ,staveIndentation)
import VexFlow.Abc.TickableContext (NoteCount, TickableContext(..), estimateBarWidth)
import VexFlow.Abc.BarEnd (repositionBarEndRepeats, fillStaveLine)
import VexFlow.Abc.Volta (startVolta, isMidVolta)

type Translation a = ExceptT String (State AbcContext) a


runBodyPart :: AbcContext -> BodyPart -> Either String (Maybe StaveSpec)
runBodyPart abcContext bp =
  unwrap $ evalStateT (runExceptT $ bodyPart bp) abcContext

runTuneBody :: AbcContext -> List BodyPart -> Either String (Array (Maybe StaveSpec))
runTuneBody abcContext bps =
  unwrap $ evalStateT (runExceptT $ tuneBody bps) abcContext

execBodyPart :: AbcContext -> BodyPart -> AbcContext
execBodyPart abcContext bp =
  unwrap $ execStateT (runExceptT $ bodyPart bp) abcContext

zipBars :: List Bar -> Array (Tuple Int Bar)
zipBars bs =
  let
    barArray = toUnfoldable bs
    intArray = 0 .. length bs
  in
    zip intArray barArray

tuneBody :: List BodyPart -> Translation (Array (Maybe StaveSpec))
tuneBody bodyParts =
  traverse bodyPart $ toUnfoldable bodyParts

bodyPart :: BodyPart -> Translation (Maybe StaveSpec)
bodyPart bp =
  case bp of
    Score bs ->
      if (isEmptyStave bs) then
        pure Nothing
      else do
        -- increment the stave number and save to state
        abcContext <- get
        let
          mStaveNo = nextStaveNo abcContext.staveNo
          staveNo = fromMaybe 0 mStaveNo
          -- reset the stave offset to be just the margin
        _ <- put (abcContext { staveNo = mStaveNo
                             , accumulatedStaveWidth = staveIndentation})
        -- then translate the bars
        staveBars <- bars staveNo bs
        let
          normalisedStaveBars = fillStaveLine abcContext.maxWidth $ repositionBarEndRepeats staveBars
        -- return the stave specification
        pure $ Just { staveNo : staveNo
                    , keySignature : abcContext.keySignature
                    , barSpecs : normalisedStaveBars}
    BodyInfo header ->
      do
        -- save the new Abc context to state governed by any header change
        abcContext <- get
        let
          contextChanges = Trans.headerChange abcContext header
          newAbcContext = foldl updateAbcContext abcContext contextChanges
        _ <- put newAbcContext
        pure Nothing

bars :: Int -> List Bar -> Translation (Array BarSpec)
bars staveNumber bs =
  let
    tupleArray = zipBars bs
  in
    traverse (\(Tuple index b) -> bar staveNumber index b) tupleArray

bar :: Int -> Int -> Bar -> Translation BarSpec
bar staveNumber barNumber abcBar =
  do
    musicSpec <- foldOverMusics $ toUnfoldable abcBar.music
    -- we must get the context AFTER iterating through the music
    abcContext <- get
    let
      isEmptyBar = isEmptyMusicSpec musicSpec
      displayedKeySig =
        if (barNumber == 0) then
          Just abcContext.keySignature
        else
          Nothing
      width =
        estimateBarWidth (barNumber == 0) (staveNumber == 0) displayedKeySig abcBar
      barSpec :: BarSpec
      barSpec =
        { barNumber : barNumber
        , width : width
        , xOffset : abcContext.accumulatedStaveWidth
        , startLine : abcBar.startLine
        , hasEndLine : true
        , endLineRepeat : false
        , volta : startVolta abcBar.startLine isEmptyBar abcContext.isMidVolta
        , timeSignature : abcContext.timeSignature
        , beatsPerBeam : abcContext.beatsPerBeam
        , musicSpec : musicSpec
        }
      -- check if we're in the midst of a volta
      newIsMidVolta = isMidVolta abcBar.startLine isEmptyBar abcContext.isMidVolta
      -- accumulate the bar width
      newWidth = abcContext.accumulatedStaveWidth + barSpec.width
      -- set the new state
      newAbcContext = abcContext { accumulatedStaveWidth = newWidth
                                 , isMidVolta = newIsMidVolta
                                 }
    _ <- put newAbcContext
    withExceptT (\err -> err <> ": bar " <> show barNumber) $ pure barSpec

music :: NoteCount -> Int -> Music -> Translation MusicSpec
music tickablePosition noteIndex m = do
  -- thread the context state through the translation
  abcContext <- get
  let
    spec = Trans.music abcContext tickablePosition noteIndex m
    newContext = applyContextChanges abcContext spec
  _ <- put newContext
  either throwError pure spec

foldOverMusics :: Array Music -> Translation MusicSpec
foldOverMusics =
  foldM foldMusicsFunction mempty

-- | fold the music function over the array of music.
-- | the monoidal behaviour of TickableContext within MusicSpec
-- | accumulates this context by threading through the fold
foldMusicsFunction ::
  MusicSpec ->
  Music ->
  Translation MusicSpec
foldMusicsFunction eacc m = do
  let
    (MusicSpec acc) = eacc
     -- find the position of the next note in the bar
    (TickableContext position duration) = acc.tickableContext
    noteIndex = Array.length acc.noteSpecs
  (MusicSpec enext) <- music position noteIndex m
  pure $ MusicSpec (acc <> enext)
