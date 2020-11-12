module Examples.Beaming.Main where

import Prelude (Unit, bind, pure, unit, ($), (/))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import VexFlow.Score (Renderer, initialiseCanvas, renderTuneAtStave)
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
  { parentElementId : "canvas"
  , width : canvasWidth
  , height : 1600
  , scale : scale
  , isSVG : true
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

displayAtStave :: Renderer -> String -> Int -> Effect Boolean
displayAtStave renderer text staveNo =
  let
    eAbcTune = parse text
  in
    case eAbcTune of
      Right abcTune -> do
        renderTuneAtStave staveNo config renderer abcTune
      _ ->
        pure false

main :: Effect Unit
main = do
  renderer <- initialiseCanvas config
  _ <- displayAtStave renderer beaming44a 0
  _ <- displayAtStave renderer beaming44b 1
  _ <- displayAtStave renderer beaming44c 2
  _ <- displayAtStave renderer beaming44d 3
  _ <- displayAtStave renderer beaming44e 4
  _ <- displayAtStave renderer beaming44f 5
  _ <- displayAtStave renderer beaming44g 6
  _ <- displayAtStave renderer beaming44h 7
  _ <- displayAtStave renderer beaming32a  8
  _ <- displayAtStave renderer beaming32b  9
  _ <- displayAtStave renderer beam16thInTuplet 10
  _ <- displayAtStave renderer beamingCommonTimeCoalesce 11
  _ <- displayAtStave renderer beamingTypesettingSpace1 12
  _ <- displayAtStave renderer beamingTypesettingSpace2 13
  pure unit
