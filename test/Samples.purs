module Test.Samples where

import Prelude (($), (<>))
import VexFlow.Types (AbcContext, staveIndentation)
import Data.Abc
import Data.Tuple (Tuple(..))
import Data.Rational ((%), fromInt)
import Data.Maybe (Maybe(..))
import Data.Map (empty)
import Data.List (List(..), fromFoldable)
import Data.Array (toUnfoldable)
import VexFlow.Abc.TranslateStateful (execBodyPart)
import VexFlow.Abc.Beat (beatDuration)


cMajor :: KeySignature
cMajor =
    {  pitchClass : C
    ,  accidental : Natural
    ,  mode : Major
    }

startAbcContext :: MeterSignature -> AbcContext
startAbcContext (Tuple numerator denominator) =
  { timeSignature : { numerator, denominator }
  , keySignature : cMajor
  , mTempo : Nothing
  , unitNoteLength : ( 1 % 16)
  , staveNo : Nothing
  , accumulatedStaveWidth : staveIndentation
  , isMidVolta : false
  , isNewTimeSignature : false
  , maxWidth : 1200
  , pendingRepeatBegin: false
  , beatDuration: beatDuration { numerator, denominator }
  }

c :: Int -> Music
c d =
  Note (cn d)

cn :: Int -> GraceableNote
cn d =
  let
    abcNote =
      { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }
  in
    { maybeGrace : Nothing, leftSlurs : 0, decorations : Nil, abcNote, rightSlurs : 0 }

f :: Int -> Music
f d =
  let
    abcNote = { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }
  in
    Note { maybeGrace : Nothing, leftSlurs : 0, decorations : Nil, abcNote, rightSlurs : 0 }


g :: Int ->  Music
g d = Note (gn d)

gn :: Int -> GraceableNote
gn d =
  let
    abcNote =
      { pitchClass: G, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }
  in
    { maybeGrace : Nothing, leftSlurs : 0, decorations : Nil, abcNote, rightSlurs : 0 }

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
  Inline $ Key gMajorM empty

meterChangeTo34 :: AbcContext -> AbcContext
meterChangeTo34 initialContext =
  let
    bar0 =
      { decorations : Nil
      , startLine : barLine
      , music : fromFoldable [c 4, f 4, g 4]
      }
    bar1 =
      { decorations : Nil
      , startLine : barLine
      , music : fromFoldable [meterChange34, f 2, f 2, c 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    execBodyPart initialContext bodyPart

keyChangeToG :: AbcContext -> AbcContext
keyChangeToG initialContext =
  let
    bar0 =
      { decorations : Nil
      , startLine : barLine
      , music : fromFoldable [c 4, f 4, g 4]
      }
    bar1 =
      { decorations : Nil
      , startLine : barLine
      , music : fromFoldable [keyChangeG, f 2, f 2, c 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar0, bar1]
  in
    execBodyPart initialContext bodyPart


-- | keep generating and storing the accumulated stave width
accumulateBarWidths :: AbcContext -> AbcContext
accumulateBarWidths initialContext =
  let
    bar =
      { decorations : Nil
      , startLine : barLine
      , music : fromFoldable [c 4, f 4, g 4]
      }
    bodyPart = Score $ toUnfoldable [bar, bar, bar, bar]
  in
    execBodyPart initialContext bodyPart

barLine :: BarLine
barLine = 
  { endRepeats : 0
  , thickness : Thin
  , startRepeats : 0
  , iteration : Nothing
  }

borddajnsijn :: String
borddajnsijn =
  "X: 1\r\n" 
  <> "T: Borddajnsijn\r\n"
  <> "R: polka\r\n"
  <> "L: 1/8\r\n"
  <> "Q: 1/4=150\r\n"
  <> "K: GMajor\r\n"
  <> "|: d2 d2 | d3 B | dc AF | GA Bc | d2 g2 | d3 B | dc AF | G2 z2 :|\r\n"
  <> "|: DF AF | DG Bd | dc AF | GB d2 | DF AF | DG Bd | dc AF | G2 z2 :|\r\n" 
  <> "\r\n"
  <> "\r\n"