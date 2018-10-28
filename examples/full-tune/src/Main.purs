module Examples.FullTune.Main where

import Prelude (Unit, bind, pure, unit)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (displayTune, initialise)
import VexFlow.Types (Config)
import Data.Abc.Parser (parse)
import Examples.FullTune.Texts (augustsson, cig, ewa, fastan)

config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : 1600
  , canvasHeight : 6400
  , scale : 0.8
  }

main :: Effect Unit
main =
  let
    eAbcTune = parse cig
  in
    case eAbcTune of
      Right abcTune -> do
        _ <- initialise config
        displayTune abcTune
      _ ->
        pure unit
