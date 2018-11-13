module Examples.StaveAlignment.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (createScore, renderScore, initialiseCanvas)
import VexFlow.Types (Config)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.StaveAlignment.Texts (augustsson, blomgren, cig, ewa, fastan,
                          smalandPolska,keyChangeSample, meterChangeSample)

canvasWidth :: Int
canvasWidth = 1500

configure :: AbcTune -> Config
configure tune =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : canvasHeight tune
  , scale : 0.8
  }

main :: Effect Boolean
main =
  let
    eAbcTune = parse blomgren
  in
    case eAbcTune of
      Right abcTune -> do
        let
          config = configure abcTune
          score = createScore config abcTune
        _ <- initialiseCanvas config
        renderScore config true score
      _ ->
        pure false
