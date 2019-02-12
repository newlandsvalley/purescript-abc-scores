module VexFlow.Abc.Alignment
  ( justifiedScoreConfig
  , rightJustify) where

-- | align the staves on the right hand side
-- |
-- | whilst editing, there is no RHS bar alignment.  A stave may be shorter than
-- | the maximum width (in which case it will eventually be truncated) or longer
-- | (in which case an empty stave line with no bar end marker is written to
-- | fill to the maximum width).
-- |
-- | However, is is usual in music scores where each stave line is of a resonable
-- | width to align both the LHS and RHS with 'real' bars (i.e. no empty stave lines
-- | continuing to the stave end)
-- |
-- | this module provides a function for doing that.
-- |
-- | We measure the staves in VexFlow stave units and scale them when writing to
-- | the canvas.

import Control.Monad.State (State, evalStateT, get, put)
import Data.Array (length, singleton, takeWhile)
import Data.Foldable (foldl, foldM)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Newtype (unwrap)
import Prelude (bind, map, max, mempty, min, pure, ($), (*), (+), (-), (/), (<>), (>=), (/=))
import VexFlow.Abc.BarEnd (staveWidth)
import VexFlow.Types (BarSpec, Config, LineThickness(..), StaveSpec, VexScore,
       scoreVerticalMargin, staveIndentation, staveSeparation)

type Alignment a = State Int a

-- | right-justify the core
rightJustify :: Int -> Number -> VexScore -> VexScore
rightJustify maxCanvasWidth scale score =
  either (\s -> Left s) (\staves -> Right $ alignStaves maxCanvasWidth scale staves) score

-- | recalculate the canvas config based on the dimensions of the justified score
justifiedScoreConfig :: VexScore -> Config -> Config
justifiedScoreConfig score originalConfig =
  let
    justifiedScoreWidth :: Int
    justifiedScoreWidth =
      either (\_ -> 0) (\staves -> justifiedScoreCanvasWidth originalConfig.scale staves) score

    justifiedScoreHeight :: Int
    justifiedScoreHeight =
      either (\_ -> 0) (\staves -> justifiedScoreCanvasHeight originalConfig.scale staves) score
  in
    originalConfig
      { canvasWidth  = justifiedScoreWidth
      , canvasHeight = justifiedScoreHeight
      }

-- | where possible, align all staves so that they are aligned at the right-hand
-- | side of the score (as well, of course, as at the left).
-- |
-- | If the widest stave is wider than the maximum stave width (and hence
-- | truncated) then align to this maximum width.
alignStaves :: Int -> Number -> Array (Maybe StaveSpec) -> Array (Maybe StaveSpec)
alignStaves maxCanvasWidth scale staves =
  let
    newStaves = map removeStaveExtension staves
    maxWidth = maxStaveWidth maxCanvasWidth scale
    alignmentWidth = alignmentStaveWidth maxWidth newStaves
    mapf (Just staveSpec) =
      let
        maybeFactor = incrementFactor alignmentWidth staveSpec.staveWidth
      in
        case maybeFactor of
          Just n ->
            Just $ growStaveSpec n staveSpec
          _ ->
            Just staveSpec
    mapf _ = Nothing
  in
    map mapf newStaves

-- | remove the empty stave bar extension that may occur at the end of a stave
removeStaveExtension  :: Maybe StaveSpec -> Maybe StaveSpec
removeStaveExtension mss =
  case mss of
    Nothing ->
      Nothing
    Just ss ->
      -- drop the last bar if it has no end line marker
      let
        barSpecs = takeWhile (\bs -> bs.endLineThickness /= NoLine) ss.barSpecs
      in
        Just $ ss { barSpecs = barSpecs }

-- | find the widest stave
-- | (if any stave is greater than the maximum width then this max is taken as the
-- | maximum)
alignmentStaveWidth :: Int -> Array (Maybe StaveSpec) -> Int
alignmentStaveWidth maxWidth mss =
  let
    staveWidthf :: Int -> Maybe StaveSpec -> Int
    staveWidthf acc mNext =
      case mNext of
        Nothing ->
          acc
        Just staveSpec ->
          min maxWidth (max acc staveSpec.staveWidth)
  in
    foldl staveWidthf 0 mss


-- | find the increase required to grow each bar in a stave so that
-- | it reaches the required alignment width
incrementFactor :: Int -> Int -> Maybe Number
incrementFactor alignmentWidth staveWidth =
  if (staveWidth >= alignmentWidth) then
    Nothing
  else
    Just $ (toNumber (alignmentWidth-staveIndentation)) /
           (toNumber (staveWidth-staveIndentation))

-- | grow the stave spec to make the stave fit the alignment width
growStaveSpec :: Number -> StaveSpec -> StaveSpec
growStaveSpec enlargement staveSpec =
  let
    barSpecs =
      unwrap $ evalStateT (growStaveSpecDefn enlargement staveSpec) staveIndentation
  in
    staveSpec { barSpecs = barSpecs
              , staveWidth = staveWidth barSpecs
              }

growStaveSpecDefn :: Number -> StaveSpec -> Alignment (Array BarSpec)
growStaveSpecDefn enlargement ss =
  let
    foldf :: Array BarSpec -> BarSpec -> Alignment (Array BarSpec)
    foldf bs b = do
      newStaveBar <- growStaveBar enlargement b
      pure $ bs <> singleton newStaveBar
  in
    foldM foldf mempty ss.barSpecs

-- | grow a stave bar to help make the stave fit the alignment width
growStaveBar :: Number -> BarSpec -> Alignment BarSpec
growStaveBar enlargement barSpec =
  let
    width = floor $ (toNumber barSpec.width) * enlargement
  in
    do
      xOffset <- get
      _ <- put $ xOffset + width
      pure $ barSpec { xOffset = xOffset
                     , width = width
                     }

-- | the maximum stave width depends on the max canvas width and the scale
maxStaveWidth :: Int -> Number -> Int
maxStaveWidth canvasWidth scale =
  floor $ (toNumber canvasWidth) / scale

-- | the canvas width that contains the justified score
justifiedScoreCanvasWidth :: Number -> Array (Maybe StaveSpec) -> Int
justifiedScoreCanvasWidth scale staves =
  let
    staveWidth = (alignmentStaveWidth 10000 staves) + (2 * staveIndentation)
  in
    floor $ (toNumber staveWidth) * scale

-- | the canvas height that contains the justified score
justifiedScoreCanvasHeight :: Number -> Array (Maybe StaveSpec) -> Int
justifiedScoreCanvasHeight scale staves =
  let
    staveHeight = ((length staves) * staveSeparation) + 2 * scoreVerticalMargin
  in
    floor $ (toNumber staveHeight) * scale
