module Examples.Slurs.Main where

import Examples.Slurs.Texts

import Data.Abc (KeySignature, TimeSignature)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Rational ((%))
import Effect (Effect)
import Prelude (Unit, bind, pure, unit, ($), (/))
import VexFlow.Abc.Beat (beatDuration)
import VexFlow.Abc.ContextChange (Clef(..))
import VexFlow.Abc.TickableContext (defaultNoteSeparation)
import VexFlow.Score (Renderer, initialiseCanvas, renderText, renderTuneAtStave)
import VexFlow.Types (Config, AbcContext, RenderingError, defaultConfig, staveIndentation)

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

abcContext :: TimeSignature -> KeySignature -> Int -> AbcContext
abcContext timeSignature keySignature staveNo =
  { timeSignature
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
  , beatDuration: beatDuration timeSignature
  , noteSeparation: defaultNoteSeparation
  , showChordSymbols: config.showChordSymbols
  }

displayAtStave :: Renderer -> String -> Int -> Effect (Maybe RenderingError)
displayAtStave renderer text staveNo =
  case (parse text) of
    Right abcTune -> do
      renderTuneAtStave staveNo config renderer abcTune
    _ ->
      pure $ Just "ABC failed to parse"

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
