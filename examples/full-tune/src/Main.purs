module Examples.FullTune.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderTune, initialiseCanvas)
import VexFlow.Types (Config, defaultConfig)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.FullTune.Texts (augustsson, cig, ewa, fastan, smalandPolska,
                                chordSymbolExample, gustavPersson, voltaContinuationSample,
                                keyChangeSample, meterChangeSample,
                                continuationSample, emptyBarSample)

config :: Config
config =
  defaultConfig 
    { width = 1200
    , showChordSymbols = true 
    }

main :: Effect Boolean
main =
  -- parse voltaContinuationSample
  -- parse augustsson
  case (parse chordSymbolExample) of
    Right abcTune -> do
      renderer <- initialiseCanvas config
      renderTune config renderer abcTune
    _ ->
      pure false
