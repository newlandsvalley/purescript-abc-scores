module Main where

import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Texts (augustsson, cig, ewa, fastan, smalandPolska, gustavPersson, voltaContinuationSample, keyChangeSample, meterChangeSample, continuationSample, emptyBarSample)
import Prelude (bind, pure, ($))
import VexFlow.Score (renderThumbnail, initialiseCanvas)
import VexFlow.Types (Config, RenderingError, Titling(..), defaultConfig)

canvasWidth :: Int
canvasWidth = 500

canvasHeight :: Int
canvasHeight = 200

config :: Config
config = defaultConfig 
  { width = canvasWidth
  , height = canvasHeight 
  , isSVG = false
  , titling = NoTitle
  }

main :: Effect (Maybe RenderingError)
main =
  case (parse ewa) of
    Right abcTune -> do
      renderer <- initialiseCanvas config
      renderThumbnail config renderer abcTune 
    _ ->
      pure $ Just "ABC failed to parse"
