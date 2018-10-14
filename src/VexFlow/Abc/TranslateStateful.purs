module VexFlow.Abc.TranslateStateful  where

-- The stateful part of translation
-- The translation wrapped in a State Monad which is inside ExceptT
-- We need to thread the AbcContext throughout the translation because
-- Any change in headers for time signature, key signature or unit note length
-- will alter the state.

import Prelude (($), (<>), (+), Unit, bind, discard, mempty, pure, show)
import Control.Monad.Except.Trans
import Control.Monad.State
import VexFlow.Abc.Translate as Trans
import Data.Either (Either, either)
import Data.Foldable (foldM, foldl)
import Data.List (List, toUnfoldable, length)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Array ((..), zip)
import Data.Traversable (traverse)
import Data.Abc (Bar, BodyPart(..), Music)
import VexFlow.Abc.Utils (applyContextChanges, nextStaveNo, updateAbcContext)
import VexFlow.Types (AbcContext, BarSpec, MusicSpec(..), StaveSpec
      , staveIndentation, staveWidth)
import VexFlow.Abc.TickableContext (NoteCount, TickableContext(..))

type Translation a = ExceptT String (State AbcContext) a

runBar :: AbcContext -> Int -> Bar -> Either String BarSpec
runBar abcContext barNumber abcBar =
  unwrap $ evalStateT (runExceptT $ bar barNumber abcBar) abcContext

runBars :: AbcContext -> List Bar -> Either String (Array BarSpec)
runBars abcContext bs =
  unwrap $ evalStateT (runExceptT $ bars bs) abcContext

runBodyPart :: AbcContext -> BodyPart -> Either String (Maybe StaveSpec)
runBodyPart abcContext bp =
  unwrap $ evalStateT (runExceptT $ bodyPart bp) abcContext

runTuneBody :: AbcContext -> List BodyPart -> Either String (Array (Maybe StaveSpec))
runTuneBody abcContext bps =
  unwrap $ evalStateT (runExceptT $ tuneBody bps) abcContext

execBars :: AbcContext -> List Bar -> AbcContext
execBars abcContext bs =
  unwrap $ execStateT (runExceptT $ bars bs) abcContext

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
      do
        -- increment the stave number and save to state
        abcContext <- get
        let
          mStaveNo = nextStaveNo abcContext.staveNo
          staveNo = fromMaybe 0 mStaveNo
          -- reset the stave offset to be just the margin
        _ <- put (abcContext { staveNo = mStaveNo
                             , accumulatedStaveWidth = staveIndentation})
        -- then translate the bars
        staveBars <- bars bs
        -- return the stave specification
        pure $ Just { staveNo : staveNo, barSpecs : staveBars}
    BodyInfo header ->
      do
        -- save the new Abc context to state governed by any header change
        abcContext <- get
        let
          contextChanges = Trans.headerChange abcContext header
          newAbcContext = foldl updateAbcContext abcContext contextChanges
        _ <- put newAbcContext
        pure Nothing

bars :: List Bar -> Translation (Array BarSpec)
bars bs =
  let
    tupleArray = zipBars bs
  in
    traverse (\(Tuple index b) -> bar index b) tupleArray

bar :: Int -> Bar -> Translation BarSpec
bar barNumber abcBar =
  do
    musicSpec <- foldOverMusics $ toUnfoldable abcBar.music
    -- we MUST get the context after iterating through the music
    abcContext <- get
    let
      barSpec :: BarSpec
      barSpec =
        { barNumber : barNumber
        , width : staveWidth
        , xOffset : abcContext.accumulatedStaveWidth
        , startLine : abcBar.startLine
        , musicSpec : musicSpec
        }
      -- accumulate the bar width
      newWidth = abcContext.accumulatedStaveWidth + barSpec.width
      newAbcContext = abcContext {accumulatedStaveWidth = newWidth}
    _ <- put newAbcContext
    withExceptT (\err -> err <> ": bar " <> show barNumber) $ pure barSpec

music :: NoteCount -> Music -> Translation MusicSpec
music tickablePosition m = do
  -- thread the context state through the translation
  abcContext <- get
  let
    spec = Trans.music abcContext tickablePosition m
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
  (MusicSpec enext) <- music position m
  pure $ MusicSpec (acc <> enext)
