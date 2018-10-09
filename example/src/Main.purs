module Example.Main where

import Prelude
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))
import Data.Maybe (Maybe(..))
import Data.List (fromFoldable)

import VexFlow.Score (displayBar, displayBars, initialise)
import VexFlow.Abc.Utils (beatsPerBeam)
import VexFlow.Types (Config, AbcContext)
import Data.Abc
import Abc.Samples


config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : 1200
  , canvasHeight : 6400
  , scale : 0.8
  }

abcContext :: MeterSignature -> AbcContext
abcContext (Tuple x y) =
  { timeSignature : { numerator: x, denominator: y }
  , unitNoteLength : ( 1 % 16)
  , beatsPerBeam : beatsPerBeam (Tuple x y)
  }

-- | simple 6/8
example0 :: Effect Unit
example0 =
  let
    staveNo = 0
    context = abcContext (Tuple 6 8)
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
  in
    displayBars context staveNo [bar0, bar1]

-- | simple 4/4
example1 :: Effect Unit
example1 =
  let
    staveNo = 1
    context = abcContext (Tuple 4 4)
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
  in
    displayBars context staveNo [bar0, bar1, bar2]

-- | simple 2/4
example2 :: Effect Unit
example2 =
  let
    staveNo = 2
    context = abcContext (Tuple 2 4)
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
  in
    displayBars context staveNo [bar0, bar1]

-- | simple 3/4
example3 :: Effect Unit
example3 =
  let
    staveNo = 3
    context = abcContext (Tuple 3 4)
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
  in
    displayBars context staveNo [bar0, bar1]

-- | chords in 4/4
example4 :: Effect Unit
example4 =
  let
    staveNo = 4
    context = abcContext (Tuple 4 4)
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, g 2, chord 4]
      }
    bar1 =
      { startLine : barType
      , music : fromFoldable [chord 2, chord 2, f 2, c 2, g 3, g 1]
      }
  in
    displayBars context staveNo [bar0, bar1]


-- | broken rhythm pair in 4/4
example5 :: Effect Unit
example5 =
  let
    staveNo = 5
    context = abcContext (Tuple 4 4)
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music : fromFoldable [brokenRight 2 1, brokenRight 2 1, brokenLeft 2 1, brokenLeft 2 1]
      }
  in
    displayBar context staveNo 0 bar

-- | basic triplet in 3/4
example6 :: Effect Unit
example6 =
  let
    staveNo = 6
    context = abcContext (Tuple 3 4)
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music :fromFoldable [c 2, f 2, triplet 2, c 4]
      }
  in
    displayBar context staveNo 0 bar

-- | basic quadruplet in 6/8
example7 :: Effect Unit
example7 =
  let
    staveNo = 7
    context = abcContext (Tuple 6 8)
    barType =
      { thickness : Thin
      , repeat : Just Begin
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, quadruplet 2]
      }
  in
    displayBar context staveNo 0 bar


main :: Effect Unit
main = do
  _ <- initialise config
  _ <- example0
  _ <- example1
  _ <- example2
  _ <- example3
  _ <- example4
  _ <- example5
  _ <- example6
  example7
