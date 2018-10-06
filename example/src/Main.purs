module Example.Main where

import Prelude
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))

import VexFlow.Score (addTimeSignature, displayMusics, displayStave, initialise, newStave)
import VexFlow.Abc.Utils (beatsPerBeam)
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
  , canvasHeight : 6400
  , scale : 0.8
  }

staveConfig :: Int -> Int -> StaveConfig
staveConfig staveNo barNo =
  { x : 10 + (staveWidth * barNo)
  , y : 10 + (staveSeparation * staveNo)
  , width : staveWidth
  , barNo : barNo
  }

abcContext :: MeterSignature -> AbcContext
abcContext (Tuple x y) =
  { timeSignature : { numerator: x, denominator: y }
  , unitNoteLength : ( 1 % 16)
  , beatsPerBeam : beatsPerBeam (Tuple x y)
  }


-- | simple 6/8
example0 :: Effect Unit
example0 = void $ do
  let
    staveNo = 0
    context = abcContext (Tuple 6 8)
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayMusics context true stave [c 2, f 2, g 2, g 2, f 2, enat 1, b 1]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) eMinor
  _ <- displayMusics context true stave1 [c 2, f 2, g 2, g 2, f 2, c 2]
  displayStave stave1

-- | simple 4/4
example1 :: Effect Unit
example1 = void $ do
  let
    staveNo = 1
    context = abcContext (Tuple 4 4)
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayMusics context true stave [c 2, f 2, g 2, g 2, f 2, enat 1, b 1, f 2, g 2]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) dMajor
  _ <- displayMusics context true stave1 [c 2, f 2, bf 2, g 2, f 2, c 2, g 3, g 1]
  displayStave stave1
  stave2 <- newStave (staveConfig staveNo 2) dMajor
  _ <- displayMusics context true stave2 [c 4, r 4]
  displayStave stave2

-- | simple 2/4
example2 :: Effect Unit
example2 = void $ do
  let
    staveNo = 2
    context = abcContext (Tuple 2 4)
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayMusics context true stave [c 4, f 2, r 2]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) eMinor
  _ <- displayMusics context true stave1 [c 2, f 2, f 1, c 1, g 1, g 1]
  displayStave stave1

-- | simple 3/4
example3 :: Effect Unit
example3 = void $ do
  let
    staveNo = 3
    context = abcContext (Tuple 3 4)
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayMusics context true stave [c 4, f 1, f 1, g 1, g 1, f 2, g 2]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) eMinor
  _ <- displayMusics context true stave1 [f 1, c 1, g 1, g 1, gs 8]
  displayStave stave1

-- | chords in 4/4
example4 :: Effect Unit
example4 = void $ do
  let
    staveNo = 4
    context = abcContext (Tuple 4 4)
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayMusics context true stave [c 2, f 2, g 2, g 2, chord 4]
  _ <- displayStave stave
  stave1 <- newStave (staveConfig staveNo 1) dMajor
  _ <- displayMusics context true stave1 [chord 2, chord 2, f 2, c 2, g 3, g 1]
  displayStave stave1

-- | broken rhythm pair in 4/4
example5 :: Effect Unit
example5 = void $ do
  let
    staveNo = 5
    context = abcContext (Tuple 4 4)
  stave <- newStave (staveConfig staveNo 0) dMajor
  _ <- addTimeSignature stave context.timeSignature
  _ <- displayMusics context true stave [brokenRight 2 1, brokenRight 2 1, brokenLeft 2 1, brokenLeft 2 1]
  displayStave stave


main :: Effect Unit
main = do
  _ <- initialise config
  _ <- example0
  _ <- example1
  _ <- example2
  _ <- example3
  _ <- example4
  example5
