module Examples.StaveAlignment.Main where

-- This example shows the final version of a tune, with staves right-aligned,
-- canvasd clipped to the tune dimensions and a tune title present. 

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderFinalTune, initialiseCanvas)
import VexFlow.Types (Config)
import Data.Abc.Parser (parse)
import Examples.StaveAlignment.Texts (augustsson, blomgren, cig, ewa, fastan,
                          smalandPolska,keyChangeSample, meterChangeSample, titled)

canvasWidth :: Int
canvasWidth = 1600

canvasDepth :: Int
canvasDepth = 800

scale :: Number
scale = 0.8


defaultConfig :: Config
defaultConfig =
  { parentElementId : "canvas"
  , width : canvasWidth
  , height : canvasDepth
  , scale : scale
  , isSVG : true
  , titled : true
  }

main :: Effect Boolean
main =
  let
    eAbcTune = parse blomgren
  in
    case eAbcTune of
      Right abcTune -> do
        renderer <- initialiseCanvas defaultConfig
        renderFinalTune defaultConfig renderer abcTune
      _ ->
        pure false
