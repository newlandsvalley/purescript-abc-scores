module Examples.DisplayTests.Main where

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
import Examples.DisplayTests.Texts

canvasWidth :: Int
canvasWidth = 1200

scale :: Number
scale = 0.8

config :: Config
config =
  { parentElementId : "canvas"
  , width : canvasWidth
  , height : 2000
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

-- | we give each test it's own stave.

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
  _ <- displayAtStave renderer simple68 0
  _ <- displayAtStave renderer simple44 1
  _ <- displayAtStave renderer simple24 2
  _ <- displayAtStave renderer simple34 3
  _ <- displayAtStave renderer chords44 4
  _ <- displayAtStave renderer brokenRhythm44 5
  _ <- displayAtStave renderer triplet34 6
  _ <- displayAtStave renderer triplet44 7
  _ <- displayAtStave renderer quadruplet68 8
  _ <- displayAtStave renderer changeMeter 9
  _ <- displayAtStave renderer changeKey 10
  _ <- displayAtStave renderer doubleDot 11
  _ <- displayAtStave renderer simpleTie 12
  _ <- displayAtStave renderer volta 13
  _ <- displayAtStave renderer grace 14
  _ <- displayAtStave renderer ornament 15
  _ <- displayAtStave renderer articulation 16
  _ <- displayAtStave renderer lineContinuation 17
  _ <- displayAtStave renderer longLine 18
  _ <- displayAtStave renderer emptyBar 19
  _ <- displayAtStave renderer crossBeatTriplet 20
  _ <- displayAtStave renderer stemDirection 21
  _ <- displayAtStave renderer tripletWithTie 22
  _ <- displayAtStave renderer repetitions 23
  pure unit


{-}
main :: Effect Unit
main = do
  _ <- initialise config
  example13
-}
