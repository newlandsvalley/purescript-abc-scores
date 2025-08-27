module Main where

import Prelude (($), bind, pure)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import VexFlow.Score (renderFinalTuneAtWidth, initialiseCanvas)
import VexFlow.Types (Config, RenderingError, defaultConfig)
import Data.Abc.Parser (parse)
import Texts (arepolskan, augustsson, cig, eklund3, ewa, fastan, smalandPolska, gustavPersson, bakmes)

config :: Config
config =
  defaultConfig 
    { width = 1200
    , showChordSymbols = true 
    , scale = 1.0
    }

main :: Effect (Either RenderingError Config)
main =
  case (parse bakmes) of
    Right abcTune -> do
      renderer <- initialiseCanvas config
      let
        desiredWidth = 400
      renderFinalTuneAtWidth config desiredWidth renderer abcTune
    _ ->
      pure $ Left "ABC failed to parse"
