module Examples.DisplayTests.Main where

import Examples.DisplayTests.Texts

import Data.Abc (KeySignature, MeterSignature)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Rational ((%))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Prelude (Unit, bind, pure, unit, ($), (/))
import VexFlow.Abc.Beat (beatDuration)
import VexFlow.Abc.ContextChange (Clef(..))
import VexFlow.Score (Renderer, initialiseCanvas, renderTuneAtStave, renderText)
import VexFlow.Types (Config, AbcContext, defaultConfig, staveIndentation)

canvasWidth :: Int
canvasWidth = 1200

canvasHeight :: Int
canvasHeight = 2400

config :: Config
config =
  defaultConfig 
    { width = canvasWidth
    , height = canvasHeight 
    , titled = false 
    }


abcContext :: MeterSignature -> KeySignature -> Int -> AbcContext
abcContext (Tuple numerator denominator ) keySignature staveNo =
  { timeSignature : { numerator, denominator}
  , keySignature : keySignature
  , mTempo : Nothing
  , unitNoteLength : ( 1 % 16)
  , clef : Treble
  , staveNo : Just staveNo
  , accumulatedStaveWidth : staveIndentation
  , isMidVolta : false
  , isNewTimeSignature : false
  , maxWidth : round $ (toNumber canvasWidth / config.scale)
  , pendingRepeatBegin: false
  , beatDuration: beatDuration { numerator, denominator }
  , showChordSymbols : config.showChordSymbols
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
  _ <- renderText renderer "General display tests" " 25pt Arial" 80 80
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
  _ <- displayAtStave renderer gracesWithAccidentals 24
  _ <- displayAtStave renderer multipleRepeats 25
  pure unit


{-}
main :: Effect Unit
main = do
  _ <- initialise config
  example13
-}
