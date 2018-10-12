module Test.Samples where

import Prelude (($))
import VexFlow.Abc.Utils (beatsPerBeam)
import VexFlow.Types (AbcContext)
import Data.Abc
import Data.Tuple (Tuple(..))
import Data.Rational ((%), fromInt)
import Data.Maybe (Maybe(..))
import Data.List (List (..), fromFoldable)
import Data.Array (toUnfoldable)
import VexFlow.Abc.TranslateStateful (execBars)

abcContext :: MeterSignature -> AbcContext
abcContext (Tuple x y) =
  { timeSignature : { numerator: x, denominator: y }
  , unitNoteLength : ( 1 % 16)
  , beatsPerBeam : beatsPerBeam (Tuple x y)
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

-- sample meter change (to 3/4)
meterChange34 :: Music
meterChange34 =
  Inline $ Meter $ Just (Tuple 3 4)

meterChangeTo34 :: AbcContext
meterChangeTo34 =
  let
    context = abcContext (Tuple 4 4)
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
  in
    execBars context $ toUnfoldable [bar0, bar1]
