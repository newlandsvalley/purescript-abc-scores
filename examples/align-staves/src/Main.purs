module Main where

-- This example shows the final version of a tune, with staves right-aligned,
-- canvasd clipped to the tune dimensions and a tune title present. 

import Prelude (($), bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderFinalTune, initialiseCanvas)
import VexFlow.Types (Config, RenderingError, defaultConfig)
import Data.Abc.Parser (parse)
import Texts (augustsson, blomgren, cig, ewa, fastan, smalandPolska,keyChangeSample, meterChangeSample, titled)

main :: Effect (Either RenderingError Config)
main =
  case (parse blomgren) of
    Right abcTune -> do
      renderer <- initialiseCanvas defaultConfig
      renderFinalTune defaultConfig renderer abcTune
    _ ->
      pure $ Left "ABC failed to parse"
