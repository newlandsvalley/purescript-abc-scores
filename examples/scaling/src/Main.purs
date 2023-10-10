module Examples.Scaling.Main where

import Prelude (($), bind, pure)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import VexFlow.Score (renderFinalTuneAtWidth, initialiseCanvas)
import VexFlow.Types (Config, RenderingError, defaultConfig)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.Scaling.Texts (arepolskan, augustsson, cig, eklund3, ewa, fastan, smalandPolska,
                                chordSymbolExample, gustavPersson, voltaContinuationSample,
                                keyChangeSample, meterChangeSample,
                                continuationSample, emptyBarSample)

config :: Config
config =
  defaultConfig 
    { width = 1200
    , showChordSymbols = true 
    , scale = 1.0
    }

main :: Effect (Maybe RenderingError)
main =
  case (parse cig) of
    Right abcTune -> do
      renderer <- initialiseCanvas config
      let
        desiredWidth = 500
      renderFinalTuneAtWidth config desiredWidth renderer abcTune
    _ ->
      pure $ Just "ABC failed to parse"
