module Abc.Samples where

import Data.Rational (fromInt)
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


enat :: Int -> AbcNote
enat d =
  { pitchClass: E, accidental: Natural, octave: 4, duration: fromInt d, tied: false }


b :: Int -> AbcNote
b d =
  { pitchClass: B, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

c :: Int -> AbcNote
c d =
  { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }



bf :: Int -> AbcNote
bf d =
  { pitchClass: B, accidental: Flat, octave: 4, duration: fromInt d, tied: false }


f :: Int -> AbcNote
f d =
  { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }


fnat :: AbcNote
fnat =
  { pitchClass: F, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }


g :: Int ->  AbcNote
g d =
  { pitchClass: G, accidental: Implicit, octave: 4, duration: fromInt d, tied: false }

gs :: Int ->  AbcNote
gs d =
  { pitchClass: G, accidental: Sharp, octave: 4, duration: fromInt d, tied: false }
