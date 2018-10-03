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


enat :: AbcNote
enat =
  { pitchClass: E, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }


b :: AbcNote
b =
  { pitchClass: B, accidental: Implicit, octave: 4, duration: fromInt 1, tied: false }

c :: AbcNote
c =
  { pitchClass: C, accidental: Implicit, octave: 4, duration: fromInt 2, tied: false }



bnat :: AbcNote
bnat =
  { pitchClass: B, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }


f :: AbcNote
f =
  { pitchClass: F, accidental: Implicit, octave: 4, duration: fromInt 2, tied: false }


fnat :: AbcNote
fnat =
  { pitchClass: F, accidental: Natural, octave: 4, duration: fromInt 1, tied: false }


g :: AbcNote
g =
  { pitchClass: G, accidental: Implicit, octave: 4, duration: fromInt 2, tied: false }
