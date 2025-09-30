module Main where

import Texts

import Effect.Console (log)
import Data.Abc (KeySignature, TimeSignature)
import Data.Abc.Parser (parse)
import Data.Abc.Utils (getTitle)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational ((%))
import Effect (Effect)
import Prelude (Unit, bind, pure, unit, ($), (/), (<>))
import VexFlow.Abc.Beat (beatDuration)
import VexFlow.Abc.ContextChange (Clef(..))
import VexFlow.Abc.TickableContext (defaultNoteSeparation)
import VexFlow.Score (Renderer, initialiseCanvas, renderText, renderTuneAtStave)
import VexFlow.Types (Config, AbcContext, RenderingError, Titling(..), defaultConfig, staveIndentation)


canvasWidth :: Int
canvasWidth = 1200

canvasHeight :: Int
canvasHeight = 1600

config :: Config
config =
  defaultConfig 
    { width = canvasWidth
    , height = canvasHeight
    , titling = NoTitle
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
      mError <- renderTuneAtStave staveNo config renderer abcTune
      case mError of 
        Just error -> do 
          let 
            title = fromMaybe "unknown title" $ getTitle abcTune
          _ <- renderText renderer (title <> ": " <> error) " 25pt Arial" 20 900
          pure mError
        _ -> 
          pure Nothing
    _ ->
      pure $ Just "ABC failed to parse"

main :: Effect Unit
main = do
  renderer <- initialiseCanvas config
  _ <- renderText renderer "Bugs!" " 25pt Arial" 80 80
  _ <- displayAtStave renderer crossBeat16th 0
  _ <- displayAtStave renderer minimLayout 1
  _ <- displayAtStave renderer horizontalLayout 2
  _ <- displayAtStave renderer voltaBrackets 3
  _ <- displayAtStave renderer threeTwoBeaming 4
  _ <- displayAtStave renderer spacing1 5
  _ <- displayAtStave renderer notelength5 6
  pure unit
