module Examples.DisplayTests.Samples where

import Prelude (($))
import Data.NonEmpty ((:|))
import Data.List.Types ((:), NonEmptyList(..))
import Data.List (List(..))
import Data.Rational (fromInt)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Abc


-- temporary constants
dMajor :: KeySignature
dMajor =
  {  pitchClass : D
  ,  accidental : Natural
  ,  mode : Major
  }


eMinor :: KeySignature
eMinor =
  {  pitchClass : E
  ,  accidental : Natural
  ,  mode : Minor
  }

cs :: AbcNote
cs =
  { pitchClass: C, accidental: Sharp, octave: 5, duration: fromInt 1, tied: false }


ds :: AbcNote
ds =
  { pitchClass: D, accidental: Sharp, octave: 5, duration: fromInt 1, tied: false }


eb :: AbcNote
eb =
  { pitchClass: E, accidental: Flat, octave: 4, duration: fromInt 1, tied: false }


enat :: Int -> Music
enat d =
  Note { pitchClass: E, accidental: Natural, octave: 4, duration: fromInt d, tied: false }


b :: Int -> Music
b d =
  Note { pitchClass: B, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

c :: Int -> Music
c d = Note (cn d)

cn :: Int -> AbcNote
cn d =
  { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

e :: Int -> Music
e d = Note (en d)

en :: Int -> AbcNote
en d =
  { pitchClass: E, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

bf :: Int -> Music
bf d =
  Note { pitchClass: B, accidental: Flat, octave: 4, duration: fromInt d, tied: false }

f :: Int -> Music
f d =
  Note { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

fnatn :: Int -> AbcNote
fnatn d =
  { pitchClass: F, accidental: Natural, octave: 4, duration: fromInt d, tied: false }

fnat :: Int ->  Music
fnat d = Note (fnatn d)

g :: Int ->  Music
g d = Note (gn d)

gn :: Int -> AbcNote
gn d =
  { pitchClass: G, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

gs :: Int ->  Music
gs d =
  Note { pitchClass: G, accidental: Sharp, octave: 4, duration: fromInt d, tied: false }

r :: Int ->  Music
r d =
  Rest { duration: fromInt d }

-- sample chord
chord :: Int -> Music
chord d =
  Chord
   { notes : NonEmptyList (gn 2 :| ( cn 2 : fnatn 2 : Nil))
   , duration : (fromInt d)
   }

-- sample broken rhythm > pair
brokenRight :: Int -> Int -> Music
brokenRight d breakage =
  BrokenRhythmPair (gn d) (RightArrow breakage) (cn d)

-- sample broken rhythm < pair
brokenLeft :: Int -> Int -> Music
brokenLeft d breakage =
  BrokenRhythmPair (gn d) (LeftArrow breakage) (cn d)

-- sample triplet
triplet :: Int -> Music
triplet d =
  let
    signature = ({ p : 3, q : 2, r : 3})
    notes = NonEmptyList ((Right $ gn 2) :| ( (Right $ cn d) : (Right $ fnatn d) : Nil))
  in
    Tuplet signature notes

-- sample quadruplet
quadruplet :: Int -> Music
quadruplet d =
  let
    signature = ({ p : 4, q : 4, r : 4})
    notes = NonEmptyList ((Right $ gn 2) :| ( (Right $ cn d) : (Right $ fnatn d) : (Right $ cn d) : Nil))
  in
    Tuplet signature notes

-- sample meter change (to 3/4)
meterChange :: Music
meterChange =
  Inline $ Meter $ Just (Tuple 3 4)

-- sample meter change (to Gm)
keyChange :: Music
keyChange =
  let
    gMinor =
      {  pitchClass : G
      ,  accidental : Natural
      ,  mode : Minor
      }
  in
    Inline $ Key $ { keySignature : gMinor
                   , modifications : Nil
                   }
