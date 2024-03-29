module VexFlow.Abc.TranslateStateful
  ( runBodyPart
  , runTuneBody
  , execBodyPart
  ) where

-- The stateful part of translation
-- The translation wrapped in a State Monad which is inside ExceptT.
--
-- We need to thread the AbcContext throughout the translation because
-- any change in headers for time signature, key signature or unit note length
-- will alter the state.
--
-- We require ExceptT because the computation can fail in three ways:
--  1) We have a modified key signature (e.g. in some Klezmer tunes) which is
--     currently not supported.
--  2) We have an unsupportable note duration (given the unit note length
--     and meter) because we can't represent it using the score's 'dotted'
--     conventions.
--  3) The ABC is effectively empty of notes

import Control.Monad.Except.Trans

import Control.Monad.State (State, evalStateT, execStateT, get, put)
import Data.Abc (Bar, BarLine, BodyPart(..), Music, NoteDuration)
import Data.Abc.Utils (isEmptyStave)
import Data.Array ((..), catMaybes, fromFoldable, zip)
import Data.Array (length) as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either, either)
import Data.Foldable (foldM, foldl)
import Data.List (List, toUnfoldable, length)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe) as U
import Prelude (($), (<>), (+), (*), (==), (&&), bind, map, mempty, pure, show)
import Safe.Coerce (coerce)
import VexFlow.Abc.BarEnd (repositionBarEndRepeats, fillStaveLine, staveWidth, staveEndsWithRepeatBegin)
import VexFlow.Abc.Beam (calculateBeams)
import VexFlow.Abc.Beat (exactBeatNumber)
import VexFlow.Abc.ChordSymbol (attachChordSymbols)
import VexFlow.Abc.Repetition (buildRepetition)
import VexFlow.Abc.Slur (vexCurves)
import VexFlow.Abc.TickableContext (NoteCount, TickableContext(..), estimateBarWidth)
import VexFlow.Abc.Translate (headerChange, music) as Trans
import VexFlow.Abc.Utils (applyContextChanges, nextStaveNo, updateAbcContext, isEmptyMusicSpec, getBarFill)
import VexFlow.Abc.Volta (startVolta, isMidVolta)
import VexFlow.Types (AbcContext, BarSpec, BeatMarker, LineThickness(..), MonophonicScore, MusicSpec(..), StaveSpec, VexScore, staveIndentation)

type Translation a = ExceptT String (State AbcContext) a

runBodyPart :: AbcContext -> BodyPart -> Either String (Maybe StaveSpec)
runBodyPart abcContext bp =
  unwrap $ evalStateT (runExceptT $ bodyPart bp) abcContext

runTuneBody :: AbcContext -> List BodyPart -> VexScore
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

tuneBody :: List BodyPart -> Translation MonophonicScore
tuneBody bodyParts = do
  -- we have an array of Maybes because some body parts of ABC are effectively empty
  mStaveSpecs <- traverse bodyPart $ toUnfoldable bodyParts
  -- so filter just the lines where there is something
  let 
    score = catMaybes mStaveSpecs
  -- fail if it's empty but coerce to a NonEmptyArray (a MonophonicScore) if not
  case (Array.length score) of 
    0 -> throwError "The score is empty"
    _ -> pure $ (coerce :: Array StaveSpec -> MonophonicScore) score


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
          -- work out if we need a new time signature displayed on this stave
          -- (this is set true in the context at the start of the module)
          -- and get this before we process the bars
          isNewTimeSignature = abcContext.isNewTimeSignature
        -- reset the stave offset to be just the margin
        -- and reset the flag for any new time signature
        _ <- put
          ( abcContext
              { staveNo = mStaveNo
              , accumulatedStaveWidth = staveIndentation
              }
          )
        -- then translate the bars
        staveBars <- bars bs
        -- reget the state after processing the bars
        abcContext' <- get
        let
          pendingRepeatBegin = staveEndsWithRepeatBegin staveBars
          normalisedStaveBars = repositionBarEndRepeats staveBars
          accumulatedStaveWidth = staveWidth normalisedStaveBars
          filledStaveLine = fillStaveLine abcContext.maxWidth normalisedStaveBars
          clefString = show abcContext'.clef
        -- save to state whether we need to carry over a Begin Volta from the last stave
        _ <- put (abcContext' { pendingRepeatBegin = pendingRepeatBegin })
        -- return the stave specification
        pure $ Just
          { staveNo: staveNo
          , staveWidth: accumulatedStaveWidth
          , clefString: clefString
          , keySignature: abcContext.keySignature
          , isNewTimeSignature: isNewTimeSignature
          , mTempo: abcContext.mTempo
          , barSpecs: filledStaveLine
          }
    BodyInfo header ->
      do
        -- save the new Abc context to state governed by any header change
        abcContext <- get
        let
          contextChanges = Trans.headerChange header
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
    musicSpec0 <- foldOverMusics abcBar.decorations $ toUnfoldable abcBar.music
    -- we must get the context AFTER iterating through the music
    -- because the fold can chenge the context
    abcContext <- get
    let
      musicSpec1 = attachChordSymbols musicSpec0
      musicSpec = addFinalBeatMarker abcContext musicSpec1
      MusicSpec spec = musicSpec
      displayedKeySig =
        if (barNumber == 0) then
          Just abcContext.keySignature
        else
          Nothing
      width =
        estimateBarWidth (barNumber == 0)
          abcContext.isNewTimeSignature
          displayedKeySig
          abcContext.noteSeparation
          abcBar
      -- continue any volta from the previous bar.  However, the first bar
      -- in a new stave may just be empty of notes, merely displaying the key
      -- signature, in which case we don't display the volta
      volta =
        if (barNumber == 0 && isEmptyMusicSpec musicSpec) then
          Nothing
        else
          startVolta abcBar.startLine abcContext.isMidVolta

      barSpec :: BarSpec
      barSpec =
        { barNumber: barNumber
        , width: width
        , xOffset: abcContext.accumulatedStaveWidth
        , startLine: modifiedStartLine abcContext.pendingRepeatBegin abcBar.startLine
        , endLineThickness: Single -- not yet known
        , endLineRepeat: false -- not yet known
        , fill : getBarFill abcContext.timeSignature abcContext.unitNoteLength spec.tickableContext
        , volta: volta
        , timeSignature: abcContext.timeSignature
        , beamSpecs: calculateBeams
            abcContext.timeSignature
            spec.noteSpecs
            spec.beatMarkers
            spec.typesettingSpaces
        , curves: vexCurves spec.slurBrackets
        , musicSpec: musicSpec
        }
      -- check if we're in the midst of a volta
      newIsMidVolta = isMidVolta abcBar.startLine abcContext.isMidVolta

      -- accumulate the bar width
      newWidth = abcContext.accumulatedStaveWidth + barSpec.width
      -- set the new state.  We must reset isNewTimeSignature here which is only
      -- set after a BodyPart new time signature header
      newAbcContext = abcContext
        { accumulatedStaveWidth = newWidth
        , isMidVolta = newIsMidVolta
        , isNewTimeSignature = false
        , pendingRepeatBegin = false
        }

    _ <- put newAbcContext
    withExceptT (\err -> err <> ": bar " <> show barNumber) $ pure barSpec

foldOverMusics :: List String -> Array Music -> Translation MusicSpec
foldOverMusics barDecorations =
  let
    -- our initial MusicSpec is empty, apart from the fact that we need to 
    -- start off with any decorations against the bar
    (MusicSpec emptySpec) = mempty :: MusicSpec
    repetitions = map buildRepetition (fromFoldable barDecorations)
    initialSpec = emptySpec { repetitions = repetitions }
  in
    -- we then fold over all the bar contents, building the VexFlow spec as we go
    foldM foldMusicsFunction (MusicSpec initialSpec)

-- | fold the music function over the array of music.
-- | the monoidal behaviour of TickableContext within MusicSpec
-- | accumulates this context by threading through the fold
foldMusicsFunction
  :: MusicSpec
  -> Music
  -> Translation MusicSpec
foldMusicsFunction eacc m = do
  let
    (MusicSpec acc) = eacc
    -- find the position of the next note in the bar
    (TickableContext position _ phraseDuration) = acc.tickableContext
    noteIndex = Array.length acc.noteSpecs
  (MusicSpec enext) <- music position noteIndex phraseDuration m
  pure $ MusicSpec (acc <> enext)

music :: NoteCount -> Int -> NoteDuration -> Music -> Translation MusicSpec
music tickablePosition noteIndex phraseDuration m =
  do
    -- thread the context state through the translation
    abcContext <- get
    let
      spec = Trans.music abcContext tickablePosition noteIndex phraseDuration m
      newContext = applyContextChanges abcContext spec
    _ <- put newContext
    either throwError pure spec

-- | add the final beat marker
-- | This will be the note index after the final note in the bar in the
-- | following cases:
-- |       the bar is full
-- |       the bar contains only lead-in notes (less than one full beat)
-- | otherwise no beat narker is added
addFinalBeatMarker :: AbcContext -> MusicSpec -> MusicSpec
addFinalBeatMarker abcContext (MusicSpec ms) =
  let
    (TickableContext noteIndex _ phraseDuration) = ms.tickableContext
    barDuration = phraseDuration * abcContext.unitNoteLength

    mBeatMarker :: Maybe BeatMarker
    mBeatMarker = exactBeatNumber barDuration abcContext.beatDuration noteIndex
  in
    if ((Array.length ms.beatMarkers == 0) && (isNothing mBeatMarker)) then
      -- lead-in notes
      MusicSpec $ ms { beatMarkers = [ { beatNumber: 1, noteIndex: Array.length ms.noteSpecs } ] }
    else
      -- full bar
      MusicSpec $ ms { beatMarkers = ms.beatMarkers <> U.fromMaybe mBeatMarker }

-- | carry over any pending Repeat Begin marker that may have ended the last stave
modifiedStartLine :: Boolean -> BarLine -> BarLine
modifiedStartLine isPendingRepeatbegin barLine =
  if isPendingRepeatbegin then
    barLine { startRepeats = 1 }
  else
    barLine
