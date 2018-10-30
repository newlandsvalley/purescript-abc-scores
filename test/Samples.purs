module Test.Samples where

import Prelude (($))
import VexFlow.Abc.Utils (beatsPerBeam, cMajor)
import VexFlow.Types (AbcContext, staveIndentation)
import Data.Abc
import Data.Tuple (Tuple(..))
import Data.Rational ((%), fromInt)
import Data.Maybe (Maybe(..))
import Data.List (List(..), fromFoldable)
import Data.Array (toUnfoldable)
import VexFlow.Abc.TranslateStateful (execBodyPart)

startAbcContext :: MeterSignature -> AbcContext
startAbcContext (Tuple x y) =
  { timeSignature : { numerator: x, denominator: y }
  , keySignature : cMajor
  , unitNoteLength : ( 1 % 16)
  , beatsPerBeam : beatsPerBeam (Tuple x y)
  , staveNo : Nothing
  , accumulatedStaveWidth : staveIndentation
  , isMidVolta : false
  , maxWidth : 1200
  }

c :: Int -> Music
c d = Note (cn d)

cn :: Int -> AbcNote
cn d =
  { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

f :: Int -> Music
f d =
  Note { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }


g :: Int ->  Music
g d = Note (gn d)

gn :: Int -> AbcNote
gn d =
  { pitchClass: G, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

-- key signatures
gMajor :: KeySignature
gMajor =
  { pitchClass: G, accidental: Natural, mode: Major }

gMajorM :: ModifiedKeySignature
gMajorM =
    { keySignature : gMajor, modifications : Nil }


-- sample meter change (to 3/4)
meterChange34 :: Music
meterChange34 =
  Inline $ Meter $ Just (Tuple 3 4)

-- sample key change (to G)
keyChangeG :: Music
keyChangeG =
  Inline $ Key gMajorM

meterChangeTo34 :: AbcContext -> AbcContext
meterChangeTo34 initialContext =
  let
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
      , music : fromFoldable [meterChange34, f 2, f 2, c 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    execBodyPart initialContext bodyPart

keyChangeToG :: AbcContext -> AbcContext
keyChangeToG initialContext =
  let
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
      , music : fromFoldable [keyChangeG, f 2, f 2, c 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    execBodyPart initialContext bodyPart


-- | keep generating and storing the accumulated stave width
accumulateBarWidths :: AbcContext -> AbcContext
accumulateBarWidths initialContext =
  let
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music : fromFoldable [c 4, f 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar, bar, bar, bar]
  in
    execBodyPart initialContext bodyPart
