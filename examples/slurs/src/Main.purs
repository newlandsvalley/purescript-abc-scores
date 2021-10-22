module Examples.Slurs.Main where

import Examples.Slurs.Texts

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
import VexFlow.Score (Renderer, initialiseCanvas, renderText, renderTuneAtStave)
import VexFlow.Types (Config, AbcContext, defaultConfig, staveIndentation)

canvasWidth :: Int
canvasWidth = 1200

canvasHeight :: Int
canvasHeight = 1600

config :: Config
config =
  defaultConfig 
    { width = canvasWidth
    , height = canvasHeight 
    , titled = false 
    }

abcContext :: MeterSignature -> KeySignature -> Int -> AbcContext
abcContext (Tuple numerator denominator) keySignature staveNo =
  { timeSignature : { numerator, denominator }
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
  , showChordSymbols: config.showChordSymbols
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
  _ <- renderText renderer "Slurs" " 25pt Arial" 80 80
  _ <- displayAtStave renderer slurs1 0
  _ <- displayAtStave renderer slurs2 1
  _ <- displayAtStave renderer brokenRhythmSlurs 2
  _ <- displayAtStave renderer tupletSlurs 3
  _ <- displayAtStave renderer tupletPrefaceSlurs 4
  _ <- displayAtStave renderer chordSlurs 5
  pure unit
