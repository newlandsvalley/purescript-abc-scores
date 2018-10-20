module Examples.DisplayTests.Main where

import Prelude (Unit, bind, ($))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))
import Data.Maybe (Maybe(..))
import Data.List (List(..), fromFoldable)
import Data.Array (toUnfoldable)
import VexFlow.Score (initialise, displayFullStave)
import VexFlow.Abc.Utils (beatsPerBeam)
import VexFlow.Types (Config, AbcContext, staveIndentation)
import Data.Abc
import Examples.DisplayTests.Samples


config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : 1200
  , canvasHeight : 6400
  , scale : 0.8
  }

abcContext :: MeterSignature -> Int -> AbcContext
abcContext (Tuple x y) staveNo =
  { timeSignature : { numerator: x, denominator: y }
  , unitNoteLength : ( 1 % 16)
  , beatsPerBeam : beatsPerBeam (Tuple x y)
  , staveNo : Just staveNo
  , accumulatedStaveWidth : staveIndentation
  }

-- | simple 6/8
-- | we can use displayFullStve here because it will always display stave 0
exampleNothing :: Effect Unit
exampleNothing =
  let
    staveNo = 0
    context0 = abcContext (Tuple 6 8) staveNo
    context = context0 { staveNo = Nothing }
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, g 2, f 2, enat 1, b 1]
    }
    bar1 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, g 2, f 2, c 2]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    displayFullStave context bodyPart

-- | simple 4/4
example0 :: Effect Unit
example0 =
  let
    staveNo = 0
    context = abcContext (Tuple 4 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music :  fromFoldable   [c 2, f 2, g 2, g 2, f 2, enat 1, b 1, f 2, g 2]
    }
    bar1 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, bf 2, g 2, f 2, c 2, g 3, g 1]
      }
    bar2 =
      { startLine : barType
      , music : fromFoldable [c 4, r 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1, bar2]
  in
    displayFullStave context bodyPart

-- | simple 2/4
example1 :: Effect Unit
example1 =
  let
    staveNo = 1
    context = abcContext (Tuple 2 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable  [c 4, f 2, r 2]
    }
    bar1 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, f 1, c 1, g 1, g 1]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    displayFullStave context bodyPart

-- | simple 3/4
example2 :: Effect Unit
example2 =
  let
    staveNo = 2
    context = abcContext (Tuple 3 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable [c 4, f 1, f 1, g 1, g 1, f 2, g 2]
    }
    bar1 =
      { startLine : barType
      , music : fromFoldable [f 1, c 1, g 1, g 1, gs 8]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    displayFullStave context bodyPart

-- | chords in 4/4
example3 :: Effect Unit
example3 =
  let
    staveNo = 3
    context = abcContext (Tuple 4 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, g 2, chord 4, c 4]
      }
    bar1 =
      { startLine : barType
      , music : fromFoldable [chord 2, chord 2, f 2, c 2, g 3, g 1]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    displayFullStave context bodyPart


-- | broken rhythm pair in 4/4
example4 :: Effect Unit
example4 =
  let
    staveNo = 4
    context = abcContext (Tuple 4 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music : fromFoldable [brokenRight 2 1, brokenRight 2 1, brokenLeft 2 1, brokenLeft 2 1]
      }
    bodyPart = Score $ toUnfoldable [bar]
  in
    displayFullStave context bodyPart

-- | basic triplet in 3/4
example5 :: Effect Unit
example5 =
  let
    staveNo = 5
    context = abcContext (Tuple 3 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music :fromFoldable [c 2, f 2, triplet 2, c 4]
      }
    bodyPart = Score $ toUnfoldable [bar]
  in
    displayFullStave context bodyPart

-- | basic quadruplet in 6/8 and also illustrates bar repeat markers
example6 :: Effect Unit
example6 =
  let
    staveNo = 6
    context = abcContext (Tuple 6 8) staveNo
    barType0 =
      { thickness : Thin
      , repeat : Just Begin
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType0
      , music : fromFoldable [c 2, f 2, g 2, quadruplet 2]
      }
    barType1 =
        { thickness : Thin
        , repeat : Just End
        , iteration : Nothing
        }
    bar1 =
        { startLine : barType1
        , music : Nil
        }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    displayFullStave context bodyPart

-- | change meter from 4/4 to 6/8 to 3/4
example7 :: Effect Unit
example7 =
  let
    staveNo = 7
    context = abcContext (Tuple 4 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, f 4, g 4, f 4]
      }
    bar1 =
      { startLine : barType
      , music : fromFoldable [meterChange68, f 2, c 2, f 2, g 2, g 2, c 2]
      }
    bar2 =
      { startLine : barType
      , music : fromFoldable [meterChange34, f 2, f 2 , c 2, c 2, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1, bar2]
  in
    displayFullStave context bodyPart

-- | change key to Gm
example8 :: Effect Unit
example8 =
  let
    staveNo = 8
    context = abcContext (Tuple 3 4) staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable [c 4, f 4, g 4]
      }
    bar1 =
      { startLine : barType
      , music : fromFoldable [keyChange, f 2, f 2, c 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    displayFullStave context bodyPart


main :: Effect Unit
main = do
  _ <- initialise config
  _ <- exampleNothing
  _ <- example0
  _ <- example1
  _ <- example2
  _ <- example3
  _ <- example4
  _ <- example5
  _ <- example6
  _ <- example7
  example8

{-}
main :: Effect Unit
main = do
  _ <- initialise config
  example5
-}
