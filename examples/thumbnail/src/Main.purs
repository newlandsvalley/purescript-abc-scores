module Examples.Thumbnail.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (createScore, renderScore, clearCanvas, initialiseCanvas, resizeCanvas)
import VexFlow.Types (Config)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc.Metadata (thumbnail)
import Data.Abc (AbcTune)
import Examples.Thumbnail.Texts (augustsson, cig, ewa, fastan, smalandPolska,
                                gustavPersson, voltaContinuationSample,
                                keyChangeSample, meterChangeSample,
                                continuationSample, emptyBarSample)

canvasWidth :: Int
canvasWidth = 1000

canvasDepth :: Int
canvasDepth = 200

scale :: Number
scale = 0.8

configure :: AbcTune -> Config
configure tune =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : canvasHeight tune
  , scale : 0.8
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
    eAbcTune = parse ewa
  in
    case eAbcTune of
      Right abcTune -> do
        renderer <- initialiseCanvas defaultConfig
        let
          unjustifiedScore = createScore defaultConfig (thumbnail abcTune)
          score = rightJustify canvasWidth scale unjustifiedScore
          config = justifiedScoreConfig score defaultConfig
        _ <- clearCanvas renderer
        _ <- resizeCanvas renderer config
        renderScore config renderer score
      _ ->
        pure false
