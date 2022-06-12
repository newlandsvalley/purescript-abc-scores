module Examples.FullTune.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderFinalTune, initialiseCanvas)
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
      {- this renders with staves extended to the RHS and with confog width repected
      renderTune config renderer abcTune
      -}
      renderFinalTune config renderer abcTune
    _ ->
      pure false
