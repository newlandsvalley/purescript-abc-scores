module Examples.StaveAlignment.Main where

-- This example shows the final version of a tune, with staves right-aligned,
-- canvasd clipped to the tune dimensions and a tune title present. 

import Prelude (($), bind, pure)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import VexFlow.Score (renderFinalTune, initialiseCanvas)
import VexFlow.Types (RenderingError, defaultConfig)
import Data.Abc.Parser (parse)
import Examples.StaveAlignment.Texts (augustsson, blomgren, cig, ewa, fastan,
                          smalandPolska,keyChangeSample, meterChangeSample, titled)

main :: Effect (Maybe RenderingError)
main =
  case (parse blomgren) of
    Right abcTune -> do
      renderer <- initialiseCanvas defaultConfig
      renderFinalTune defaultConfig renderer abcTune
    _ ->
      pure $ Just "ABC failed to parse"
