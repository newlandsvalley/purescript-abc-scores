module Examples.DisplayTests.Main where

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
import Examples.DisplayTests.Texts

canvasWidth :: Int
canvasWidth = 1200

scale :: Number
scale = 0.8

config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : 2000
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

-- | we give each test it's own stave.

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
  _ <- displayAtStave simple68 0
  _ <- displayAtStave simple44 1
  _ <- displayAtStave simple24 2
  _ <- displayAtStave simple34 3
  _ <- displayAtStave chords44 4
  _ <- displayAtStave brokenRhythm44 5
  _ <- displayAtStave triplet34 6
  _ <- displayAtStave triplet44 7
  _ <- displayAtStave quadruplet68 8
  _ <- displayAtStave changeMeter 9
  _ <- displayAtStave changeKey 10
  _ <- displayAtStave doubleDot 11
  _ <- displayAtStave simpleTie 12
  _ <- displayAtStave volta 13
  _ <- displayAtStave grace 14
  _ <- displayAtStave ornament 15
  _ <- displayAtStave articulation 16
  _ <- displayAtStave lineContinuation 17
  _ <- displayAtStave longLine 18
  _ <- displayAtStave emptyBar 19
  _ <- displayAtStave doubleBarLine 20
  _ <- displayAtStave slurs 21
  _ <- displayAtStave stemDirection 22
  pure unit


{-}
main :: Effect Unit
main = do
  _ <- initialise config
  example13
-}
