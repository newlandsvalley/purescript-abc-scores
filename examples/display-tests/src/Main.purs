module Examples.DisplayTests.Main where

import Prelude (Unit, bind, ($), (/))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))
import Data.Maybe (Maybe(..))
import Data.List (List(..), fromFoldable)
import Data.Array (toUnfoldable)
import Data.Int (round, toNumber)
import VexFlow.Score (initialise, renderFullStave)
import VexFlow.Abc.Utils (beatsPerBeam, cMajor)
import VexFlow.Types (Config, AbcContext, staveIndentation)
import Data.Abc (BodyPart(..), KeySignature, MeterSignature,
                 Repeat(..), Thickness(..))
import Examples.DisplayTests.Samples

canvasWidth :: Int
canvasWidth = 1200

scale :: Number
scale = 0.8

config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : 1600
  , scale : scale
  }

abcContext :: MeterSignature -> KeySignature -> Int -> AbcContext
abcContext (Tuple x y) keySignature staveNo =
  { timeSignature : { numerator: x, denominator: y }
  , keySignature : keySignature
  , unitNoteLength : ( 1 % 16)
  , beatsPerBeam : beatsPerBeam (Tuple x y)
  , staveNo : Just staveNo
  , accumulatedStaveWidth : staveIndentation
  , isMidVolta : false
  , isNewTimeSignature : false
  , maxWidth : round $ (toNumber canvasWidth / scale)
  }

-- | we give each test it's own stave.  The downside is that subsequent staves
-- | do not by default display the time signature (because it's assumed to be inherited)

-- | simple 6/8
-- | we can use displayFullStve here because it will always display stave 0
exampleNothing :: Effect Unit
exampleNothing =
  let
    staveNo = 0
    context0 = abcContext (Tuple 6 8) eMajor staveNo
    context = context0 { staveNo = Nothing }
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music : fromFoldable []
      }
    bar1 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, g 2, f 2, enat 1, b 1]
    }
    bar2 =
      { startLine : barType
      , music : fromFoldable [c 2, f 2, g 2, g 2, f 2, c 2]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1, bar2]
  in
    renderFullStave context bodyPart

-- | simple 4/4
example0 :: Effect Unit
example0 =
  let
    staveNo = 0
    context = abcContext (Tuple 4 4) dMajor staveNo
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
    renderFullStave context bodyPart

-- | simple 2/4
example1 :: Effect Unit
example1 =
  let
    staveNo = 1
    context = abcContext (Tuple 2 4) eMinor staveNo
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
    bar2 =
      { startLine : barType
      , music : fromFoldable [c 8]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1, bar2]
  in
    renderFullStave context bodyPart

-- | simple 3/4
example2 :: Effect Unit
example2 =
  let
    staveNo = 2
    context = abcContext (Tuple 3 4) cMajor staveNo
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
    renderFullStave context bodyPart

-- | chords in 4/4
example3 :: Effect Unit
example3 =
  let
    staveNo = 3
    context = abcContext (Tuple 4 4) cMajor staveNo
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
    renderFullStave context bodyPart


-- | broken rhythm pair in 4/4
example4 :: Effect Unit
example4 =
  let
    staveNo = 4
    context = abcContext (Tuple 4 4) cMajor staveNo
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
    renderFullStave context bodyPart

-- | basic triplet in 3/4
example5 :: Effect Unit
example5 =
  let
    staveNo = 5
    context = abcContext (Tuple 3 4) aFlatMajor staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music :fromFoldable [c 2, f 2, triplet 2, c 4]
      }
    bar1 =
      { startLine : barType
      , music : fromFoldable []
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    renderFullStave context bodyPart

-- | basic quadruplet in 6/8 and also illustrates bar repeat markers
example6 :: Effect Unit
example6 =
  let
    staveNo = 6
    context = abcContext (Tuple 6 8) eMajor staveNo
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
    renderFullStave context bodyPart

-- | change meter from 4/4 to 6/8 to 3/4
example7 :: Effect Unit
example7 =
  let
    staveNo = 7
    context = abcContext (Tuple 4 4) cMajor staveNo
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
    renderFullStave context bodyPart

-- | change key to Gm
example8 :: Effect Unit
example8 =
  let
    staveNo = 8
    context = abcContext (Tuple 3 4) cMajor staveNo
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
    renderFullStave context bodyPart

-- | double dot
example9 :: Effect Unit
example9 =
  let
    staveNo = 9
    context = abcContext (Tuple 3 4) cMajor staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music :fromFoldable [c 1, f 3, c 1, f 3, brokenLeft 1 2]
      }
    bodyPart = Score $ toUnfoldable [bar]
  in
    renderFullStave context bodyPart

-- | simple tie
example10 :: Effect Unit
example10 =
  let
    staveNo = 10
    context = abcContext (Tuple 4 4) cMajor staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType
      , music :  fromFoldable   [c 4, f 4, (tie $ g 4), (tie $ g 4) ]
    }
    bar1 =
      { startLine : barType
      , music : fromFoldable [g 8, c 8]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    renderFullStave context bodyPart

-- | volta
example11 :: Effect Unit
example11 =
  let
    staveNo = 11
    context = abcContext (Tuple 4 4) cMajor staveNo
    -- normal A part
    barType0 =
      { thickness : Thin
      , repeat : Just Begin
      , iteration : Nothing
      }
    bar0 =
      { startLine : barType0
      , music :  fromFoldable   [c 4, f 4, g 4, g 4 ]
    }
    -- 1st repeat
    barType1 =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Just 1
      }
    bar1 =
      { startLine : barType1
      , music : fromFoldable [g 8, c 8]
      }
    -- continution of first repeat
    barType2 =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar2 =
      { startLine : barType2
      , music : fromFoldable [f 8, f 8]
      }
    -- end first repeat and start second repeat
    barType3 =
      { thickness : Thin
      , repeat : Just End
      , iteration : Just 2
      }
    bar3 =
      { startLine : barType3
      , music : fromFoldable [c 8, c 8]
      }
    -- mark end of second repeat with thick bar line
    -- and start B part of tune
    barType4 =
      { thickness : ThinThick
      , repeat : Nothing
      , iteration : Nothing
      }
    bar4 =
      { startLine : barType4
      , music : fromFoldable [f 8, f 8]
      }
    -- normal redundant bar marking stave end
    barType5 =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar5 =
      { startLine : barType5
      , music : fromFoldable []
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1, bar2, bar3, bar4]
  in
    renderFullStave context bodyPart

-- | long line
example12 :: Effect Unit
example12 =
  let
    staveNo = 12
    context = abcContext (Tuple 4 4) cMajor staveNo
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music :  fromFoldable   [c 2, f 2, g 2, g 2, f 2, enat 1, b 1, f 2, g 2]
      }
    bodyPart = Score $ toUnfoldable [bar, bar, bar, bar, bar, bar, bar, bar]
  in
    renderFullStave context bodyPart

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
  _ <- example8
  _ <- example9
  _ <- example10
  _ <- example11
  example12


{-
main :: Effect Unit
main = do
  _ <- initialise config
  _ <- example11
  clearCanvas
-}
