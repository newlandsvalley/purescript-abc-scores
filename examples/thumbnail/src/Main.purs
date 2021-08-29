module Examples.Thumbnail.Main where

import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Effect (Effect)
import Examples.Thumbnail.Texts (augustsson, cig, ewa, fastan, smalandPolska, gustavPersson, voltaContinuationSample, keyChangeSample, meterChangeSample, continuationSample, emptyBarSample)
import Prelude (bind, pure)
import VexFlow.Score (renderThumbnail, initialiseCanvas)
import VexFlow.Types (Config, defaultConfig)

canvasWidth :: Int
canvasWidth = 1000

canvasHeight :: Int
canvasHeight = 200

config :: Config
config = defaultConfig 
  { width = canvasWidth
  , height = canvasHeight 
  , isSVG = false
  , titled = false 
  }

main :: Effect Boolean
main =
  let
    eAbcTune = parse ewa
  in
    case eAbcTune of
      Right abcTune -> do
        renderer <- initialiseCanvas config
        renderThumbnail config renderer abcTune 
      _ ->
        pure false
