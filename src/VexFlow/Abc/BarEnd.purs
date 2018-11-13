module VexFlow.Abc.BarEnd
  ( repositionBarEndRepeats
  , fillStaveLine
  , staveWidth) where

-- | Routines to handle the disparity between the manner in which ABC descibes
-- | bars and the manner in which VexFlow does so.  In the former, bars hold a
-- | start barline but no end barline.  The very final bar of a stave is empty
-- | but will hold the final barline.  In the latter, there can be both a start
-- | and end barline which is necessary in order to display a repeat end marker
-- | (only attached to the end barline).

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array ((:), last, reverse, snoc)
import Data.Foldable (foldM)
import Control.Monad.State (State, evalState, get, put)
import VexFlow.Types (BarSpec, MusicSpec)
import VexFlow.Abc.Volta (completeVolta)
import VexFlow.Abc.Utils (isEmptyMusicSpec)
import Data.Abc (BarType, Thickness(..), Repeat(..))

type BarState = State Boolean (Array BarSpec)

-- | move any bar end repeat marker to the previous bar
-- | i.e. a bar end should belong to the bar it ends not the next one
-- | that it introduces.
shiftBarEnd :: Array BarSpec -> BarSpec -> BarState
shiftBarEnd  acc barSpec = do
  lastEndBar <- get
  let
    -- does the current bar have an end repeat ?
    currentEndBar = (barSpec.startLine.repeat == Just End) ||
                 (barSpec.startLine.repeat == Just BeginAndEnd)
    -- complete the volta if we detect that we've arrived at a bar
    -- marker that ends a volta section
    newVolta =
      if lastEndBar then
        completeVolta barSpec.volta
      else
        barSpec.volta
    -- carry over the bar repeat marker from the last bar to this
    newBarSpec = barSpec { endLineRepeat = lastEndBar
                         , volta = newVolta
                         }
    -- save the end bar repeat of the current bar to state
  _ <- put currentEndBar
  -- if we come across a bar empty of music (always the case in the final bar
  -- of the stave) then we can now ignore it.
  if (redundantBar barSpec)
    then pure acc
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
  evalState (shiftBarEnds $ reverse bs) false

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
                              , hasEndLine = false
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

-- | the current width of the stave's Stave Bars
staveWidth :: Array BarSpec -> Int
staveWidth bs =
  case (last bs) of
    Just b ->
      b.xOffset + b.width
    Nothing ->
      0

simpleBarType :: BarType
simpleBarType =
  { thickness : Thin
  , repeat : Nothing
  , iteration : Nothing
  }
