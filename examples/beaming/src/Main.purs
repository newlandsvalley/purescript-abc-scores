module Examples.Beaming.Main where

import Prelude (Unit, bind, pure, unit, ($), (/))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import VexFlow.Score (initialiseCanvas, renderTuneAtStave)
import VexFlow.Types (Config, AbcContext, staveIndentation)
import VexFlow.Abc.Beat (beatDuration)
import Data.Abc (KeySignature, MeterSignature)
import Data.Abc.Parser (parse)
import Examples.Beaming.Texts

canvasWidth :: Int
canvasWidth = 1200

scale :: Number
scale = 0.8

config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : 1600
  , scale : scale
  }

abcContext :: MeterSignature -> KeySignature -> Int -> AbcContext
abcContext (Tuple numerator denominator ) keySignature staveNo =
  { timeSignature : { numerator, denominator}
  , keySignature : keySignature
  , mTempo : Nothing
  , unitNoteLength : ( 1 % 16)
  , staveNo : Just staveNo
  , accumulatedStaveWidth : staveIndentation
  , isMidVolta : false
  , isNewTimeSignature : false
  , maxWidth : round $ (toNumber canvasWidth / scale)
  , pendingRepeatBegin: false
  , beatDuration: beatDuration { numerator, denominator }
  }

displayAtStave :: String -> Int -> Effect Boolean
displayAtStave text staveNo =
  let
    eAbcTune = parse text
  in
    case eAbcTune of
      Right abcTune -> do
        renderTuneAtStave staveNo config abcTune
      _ ->
        pure false

main :: Effect Unit
main = do
  _ <- initialiseCanvas config
  _ <- displayAtStave beaming44a 0
  _ <- displayAtStave beaming44b 1
  _ <- displayAtStave beaming44c 2
  _ <- displayAtStave beaming44d 3
  _ <- displayAtStave beaming44e 4
  _ <- displayAtStave beaming44f 5
  _ <- displayAtStave beaming44g 6
  _ <- displayAtStave beaming32  7
  pure unit
