module Examples.Thumbnail.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderThumbnail, initialiseCanvas)
import VexFlow.Types (Config)
import Data.Abc.Parser (parse)
import Examples.Thumbnail.Texts (augustsson, cig, ewa, fastan, smalandPolska,
                                gustavPersson, voltaContinuationSample,
                                keyChangeSample, meterChangeSample,
                                continuationSample, emptyBarSample)

canvasWidth :: Int
canvasWidth = 1000

canvasDepth :: Int
canvasDepth = 200

scale :: Number
scale = 0.8

defaultConfig :: Config
defaultConfig =
  { parentElementId : "canvas"
  , width : canvasWidth
  , height : canvasDepth
  , scale : scale
  , isSVG : false
  , titled : false -- thumbnails are never titled so this is ignored
  }

main :: Effect Boolean
main =
  let
    eAbcTune = parse ewa
  in
    case eAbcTune of
      Right abcTune -> do
        renderer <- initialiseCanvas defaultConfig
        renderThumbnail defaultConfig renderer abcTune 
      _ ->
        pure false
