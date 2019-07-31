module Examples.StaveAlignment.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (createScore, renderScore, initialiseCanvas, resizeCanvas)
import VexFlow.Types (Config)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.StaveAlignment.Texts (augustsson, blomgren, cig, ewa, fastan,
                          smalandPolska,keyChangeSample, meterChangeSample)

canvasWidth :: Int
canvasWidth = 1600

canvasDepth :: Int
canvasDepth = 800

scale :: Number
scale = 0.8

configure :: AbcTune -> Config
configure tune =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : canvasHeight tune
  , scale : scale
  }

defaultConfig :: Config
defaultConfig =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : canvasDepth
  , scale : scale
  }

main :: Effect Boolean
main =
  let
    eAbcTune = parse blomgren
  in
    case eAbcTune of
      Right abcTune -> do
        renderer <- initialiseCanvas defaultConfig
        let
          -- config = configure abcTune
          unjustifiedScore = createScore defaultConfig abcTune
          score = rightJustify canvasWidth scale unjustifiedScore
          config = justifiedScoreConfig score defaultConfig
        _ <- resizeCanvas renderer config
        renderScore config renderer score
      _ ->
        pure false
