module Example.Main where

import Prelude
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))

import VexFlow.Score (addTimeSignature, displayNotes, displayStave, initialise, newStave)
import VexFlow.Types (Config, StaveConfig, AbcContext)
import Data.Abc
import Abc.Samples

staveWidth :: Int
staveWidth = 250

staveSeparation :: Int
staveSeparation = 100

config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : 1200
  , canvasHeight : 1200
  , scale : 0.8
  }

staveConfig :: Int -> Int -> StaveConfig
staveConfig staveNo barNo =
  { x : 10 + (staveWidth * barNo)
  , y : 10 + (staveSeparation * staveNo)
  , width : staveWidth
  , barNo : barNo
  }

sixEightContext :: AbcContext
sixEightContext =
  { timeSignature : { numerator: 6, denominator: 8 }
  , unitNoteLength : ( 1 % 16)
  }

fourFourContext :: AbcContext
fourFourContext =
  { timeSignature : { numerator: 4, denominator: 4 }
  , unitNoteLength : ( 1 % 16)
  }

-- | simple 6/8
example1 :: Effect Unit
example1 = void $ do
  let
    staveNo = 0
    context = sixEightContext
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayNotes context true stave [c, f, g, g, f, enat, b]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) eMinor
  _ <- displayNotes context true stave1 [c, f, g, g, f, c]
  displayStave stave1

-- | simple 4/4
example2 :: Effect Unit
example2 = void $ do
  let
    staveNo = 1
    context = fourFourContext
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayNotes context true stave [c, f, g, g, f, enat, b, b]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) eMinor
  _ <- displayNotes context true stave1 [c, f, g, g, f, c, g, g]
  displayStave stave1


main :: Effect Unit
main = do
  _ <- initialise config
  _ <- example1
  example2
