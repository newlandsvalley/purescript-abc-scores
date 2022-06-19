module Examples.Beaming.Main where

import Examples.Beaming.Texts

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
import VexFlow.Types (Config, AbcContext, RenderingError, defaultConfig, staveIndentation)

canvasWidth :: Int
canvasWidth = 1200

canvasHeight :: Int
canvasHeight = 1600

scale :: Number 
scale = 
  defaultConfig.scale

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
  , maxWidth : round $ (toNumber canvasWidth / scale)
  , pendingRepeatBegin: false
  , beatDuration: beatDuration { numerator, denominator }
  , showChordSymbols: false
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
  _ <- renderText renderer "Beaming" " 25pt Arial" 80 80
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
