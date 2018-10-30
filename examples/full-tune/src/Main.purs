module Examples.FullTune.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderTune, initialise)
import VexFlow.Types (Config)
import VexFlow.Abc.Utils (canvasHeight)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)
import Examples.FullTune.Texts (augustsson, cig, ewa, fastan, smalandPolska)

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
    eAbcTune = parse smalandPolska
  in
    case eAbcTune of
      Right abcTune -> do
        let
          config = configure abcTune
        _ <- initialise config
        renderTune abcTune config
      _ ->
        pure false
