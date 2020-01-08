module Examples.FullTune.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderTune, initialiseCanvas)
import VexFlow.Types (Config)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.FullTune.Texts (augustsson, cig, ewa, fastan, smalandPolska,
                                gustavPersson, voltaContinuationSample,
                                keyChangeSample, meterChangeSample,
                                continuationSample, emptyBarSample)

canvasWidth :: Int
canvasWidth = 1500

configure :: AbcTune -> Config
configure tune =
  { parentElementId : "canvas"
  , width : canvasWidth
  , height : canvasHeight tune
  , scale : 0.8
  , isSVG : true
  }

main :: Effect Boolean
main =
  let
    -- eAbcTune = parse voltaContinuationSample
    eAbcTune = parse augustsson
  in
    case eAbcTune of
      Right abcTune -> do
        let
          config = configure abcTune
        renderer <- initialiseCanvas config
        renderTune config renderer abcTune
      _ ->
        pure false
