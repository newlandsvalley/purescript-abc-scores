module VexFlow.Abc.BarEnd
  ( repositionBarEndRepeats
  , fillStaveLine
  , staveEndsWithRepeatBegin
  , staveWidth) where

-- | Routines to handle the disparity between the manner in which ABC descibes
-- | bars and the manner in which VexFlow does so.  In the former, bars hold a
-- | start barline but no end barline.  The very final bar of a stave is empty
-- | but will hold the final barline.  In the latter, there can be both a start
-- | and end barline which is necessary in order to display a repeat end marker
-- | (only attached to the end barline).

import Prelude

import Control.Monad.State (State, evalState, get, put)
import Data.Abc (BarType, Repeat(..), Thickness(..))
import Data.Array ((:), last, reverse, snoc)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), maybe)
import VexFlow.Abc.Utils (isEmptyMusicSpec)
import VexFlow.Abc.Volta (completeVolta)
import VexFlow.Types (BarSpec, LineThickness(..), MusicSpec)

type BarEnd =
  { isEndRepeat :: Boolean
  , lineThickness :: LineThickness
  }

type BarState = State BarEnd (Array BarSpec)

-- | move any bar end repeat or thickness marker to the previous bar
-- | i.e. a bar end and thiskness should belong to the bar it ends not the
-- | next one that it introduces.
-- | remember we're processing the bars backwards
shiftBarEnd :: Array BarSpec -> BarSpec -> BarState
shiftBarEnd  acc barSpec = do
  lastBarEnd <- get
  let
    -- does the current bar have an end repeat ?
    isEndRepeat = (barSpec.startLine.repeat == Just End) ||
                 (barSpec.startLine.repeat == Just BeginAndEnd)
    -- and its thickness
    lineThickness = barlineThickness barSpec.startLine
    -- complete the volta if we detect that we've arrived at a bar
    -- marker that ends a volta section or if the current bar
    -- starts with a double line
    newVolta =
      if lastBarEnd.isEndRepeat then
        completeVolta barSpec.volta
      else
        barSpec.volta
    -- carry over the bar repeat marker and line thickness from the last bar to
    -- this where it now decorates the right-hand bar line of the bar.
    newBarSpec = barSpec { endLineRepeat = lastBarEnd.isEndRepeat
                         , endLineThickness = lastBarEnd.lineThickness
                         , volta = newVolta
                         }
    -- save the end bar repeat of the current bar to state
  _ <- put { isEndRepeat, lineThickness }
  -- if we come across a bar empty of music (always the case in the final bar
  -- of the stave) then we can now ignore it.
  if (redundantBar barSpec)
    then
      pure acc
    else pure $ newBarSpec : acc

shiftBarEnds :: Array BarSpec -> BarState
shiftBarEnds =
  foldM shiftBarEnd mempty

-- | a bar is redundant if it contains no music and is not the first bar
-- | in the stave (which will hold a time signature etc.)
redundantBar :: BarSpec -> Boolean
redundantBar barSpec =
  (isEmptyMusicSpec barSpec.musicSpec) && (barSpec.barNumber /= 0)

-- | reposition all bar end repeats in a stave to the previous bar
-- | as we're only allowed a foldl we need to reverse at the start
repositionBarEndRepeats :: Array BarSpec -> Array BarSpec
repositionBarEndRepeats bs =
  evalState (shiftBarEnds $ reverse bs)
    { isEndRepeat : false
    , lineThickness : Single
    }

-- | fill out the stave line to the maximum permiitted line width by specifiying
-- | an empty final bar of the residual width
fillStaveLine :: Int -> Array BarSpec -> Array BarSpec
fillStaveLine maxWidth bs =
  case (last bs) of
    Just b ->
      let
        currentWidth = b.xOffset + b.width
      in
        if (currentWidth <= maxWidth) then
          let
            completionBar = b { barNumber = b.barNumber + 1
                              , width = (maxWidth - currentWidth)
                              , xOffset = currentWidth
                              , startLine = simpleBarType
                              , endLineThickness = NoLine
                              , endLineRepeat = false
                              , volta = Nothing
                              , musicSpec = mempty :: MusicSpec
                              }
          in
            snoc bs completionBar
      else
        bs
    Nothing ->
      bs

-- | does the stave end with a begin repeat marker?
-- | (to be carried over to the next stave).
staveEndsWithRepeatBegin :: Array BarSpec -> Boolean
staveEndsWithRepeatBegin bs =
  let
    isBeginVolta b =
       ((b.startLine.repeat == Just Begin) ||
        (b.startLine.repeat == Just BeginAndEnd))
  in
    maybe false isBeginVolta (last bs)

-- | the current width of the stave's Stave Bars
staveWidth :: Array BarSpec -> Int
staveWidth bs =
  maybe 0 (\b -> b.xOffset + b.width) (last bs)

barlineThickness :: BarType -> LineThickness
barlineThickness barType =
  case barType.thickness of
    Thin ->
      Single
    Invisible ->
      NoLine
    _ ->
      Double


simpleBarType :: BarType
simpleBarType =
  { thickness : Thin
  , repeat : Nothing
  , iteration : Nothing
  }
