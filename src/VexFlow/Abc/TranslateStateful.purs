module VexFlow.Abc.TranslateStateful  where

-- The stateful part of translation
-- The translation wrapped in a State Monad which is inside ExceptT
-- We need to thread the AbcContext throughout the translation because
-- Any change in headers for time signature, key signature or unit note length
-- will alter the state.

import Prelude (($), (<>), Unit, bind, discard, mempty, pure, show, unit)
import Control.Monad.Except.Trans
import Control.Monad.State
import VexFlow.Abc.Translate as Trans
import Data.Either (Either, either)
import Data.Foldable (foldM)
import Data.List (List, toUnfoldable, length)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)
import Data.Array ((..), zip)
import Data.Traversable (traverse)
import Data.Abc
import VexFlow.Abc.TickableContext (NoteCount)
import VexFlow.Abc.Utils (applyContextChanges)
import VexFlow.Types (AbcContext, BarSpec, NoteSpec, TupletSpec, MusicSpec(..))
import VexFlow.Abc.TickableContext (NoteCount, TickableContext(..), getTickableContext)

type Translation a = ExceptT String (State AbcContext) a

runBar :: AbcContext -> Int -> Bar -> Either String BarSpec
runBar abcContext barNumber abcBar =
  unwrap $ evalStateT (runExceptT $ bar barNumber abcBar) abcContext

runBars :: AbcContext -> List Bar -> Either String (Array BarSpec)
runBars abcContext bs =
  unwrap $ evalStateT (runExceptT $ bars bs) abcContext

execBars :: AbcContext -> List Bar -> AbcContext
execBars abcContext bs =
  unwrap $ execStateT (runExceptT $ bars bs) abcContext    

zipBars :: List Bar -> Array (Tuple Int Bar)
zipBars bars =
  let
    barArray = toUnfoldable bars
    intArray = 0 .. length bars
  in
    zip intArray barArray

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
    let
      barSpec :: BarSpec
      barSpec =
        { barNumber : barNumber
        , startLine : abcBar.startLine
        , musicSpec : musicSpec
        }
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
