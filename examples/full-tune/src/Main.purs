module Examples.FullTune.Main where

import Prelude (($), bind, pure)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import VexFlow.Score (renderFinalTune, initialiseCanvas)
import VexFlow.Types (Config, RenderingError, defaultConfig)
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

main :: Effect (Maybe RenderingError)
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
      pure $ Just "ABC failed to parse"
