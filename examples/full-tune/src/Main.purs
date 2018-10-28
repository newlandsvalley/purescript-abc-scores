module Examples.FullTune.Main where

import Prelude (Unit, bind, pure, unit)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (displayTune, initialise)
import VexFlow.Types (Config)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.FullTune.Texts (augustsson, cig, ewa, fastan)

maxWidth :: Int
maxWidth = 1500

config :: AbcTune -> Config
config tune =
  { canvasDivId : "canvas"
  , canvasWidth : maxWidth
  , canvasHeight : canvasHeight tune
  , scale : 0.8
  }

main :: Effect Unit
main =
  let
    eAbcTune = parse cig
  in
    case eAbcTune of
      Right abcTune -> do
        _ <- initialise (config abcTune)
        displayTune abcTune maxWidth
      _ ->
        pure unit
