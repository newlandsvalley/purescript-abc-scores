module Examples.DisplayTests.Texts where

import Prelude ((<>))

simple68 :: String
simple68 =
  "X:1\r\n"
  <> "T: simple 6/8\r\n"
  <> "M: 6/8\r\n"
  <> "L: 1/16\r\n"
  <> "K: E\r\n"
  <> "| C2F2G2 G2F2 =EB | C2F2G2 G2 F2 C2 |\r\n"

simple44 :: String
simple44 =
  "X:1\r\n"
  <> "T: simple 4/4\r\n"
  <> "M: 4/4\r\n"
  <> "L: 1/16\r\n"
  <> "K: D\r\n"
  <> "| C2F2G2G2 F2 =EBF2G2 | C2F2_B2G2 F2C2G3G1 | C4 z4 |\r\n"

simple24 :: String
simple24 =
  "X:1\r\n"
  <> "T: simple 2/4\r\n"
  <> "M: 2/4\r\n"
  <> "L: 1/16\r\n"
  <> "K: Em\r\n"
  <> "| C4 F2z2 | C2F2 FCGG | C8 |\r\n"

simple34 :: String
simple34 =
  "X:1\r\n"
  <> "T: simple 3/4\r\n"
  <> "M: 3/4\r\n"
  <> "L: 1/16\r\n"
  <> "K: C\r\n"
  <> "| C4 FFGG F2G2 | FCGG ^G8 |\r\n"

chords44 :: String
chords44 =
  "X:1\r\n"
  <> "T: chords in 4/4\r\n"
  <> "M: 4/4\r\n"
  <> "L: 1/16\r\n"
  <> "K: C\r\n"
  <> "| C2F2G2G2 [G8C8F8] | [C4G4F4] [C3F3A3] F3C2 G3G1 |\r\n"

brokenRhythm44 :: String
brokenRhythm44 =
  "X:1\r\n"
  <> "T: broken rhythm in 4/4\r\n"
  <> "M: 4/4\r\n"
  <> "L: 1/16\r\n"
  <> "K: C\r\n"
  <> "| G2>C2 G2>C2 G2<C2 G2<C2 |\r\n"

triplet34 :: String
triplet34 =
    "X:1\r\n"
    <> "T: triplet in 3/4\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: Ab\r\n"
    <> "| C2F2  (3G2C2=F2 C4 |\r\n"

triplet44 :: String
triplet44 =
    "X:1\r\n"
    <> "T: triplet in 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: Ab\r\n"
    <> "| C2F2 (3G2C2=F2 G2F2 C4 |\r\n"

quadruplet68 :: String
quadruplet68 =
    "X:1\r\n"
    <> "T: quadruplet in 6/8 with bar repeats\r\n"
    <> "M: 6/8\r\n"
    <> "L: 1/16\r\n"
    <> "K: E\r\n"
    <> "|: C2F2G2 (4G2C2=F2C2 :|\r\n"

changeMeter :: String
changeMeter =
    "X:1\r\n"
    <> "T: change meter 4/4-6/8-3/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| C2F2 F4 G4 F4 |[M: 6/8] F2C2 F2G2 G2C2 |[M: 3/4] F2F2 C2C2 G4 |\r\n"

changeKey :: String
changeKey =
    "X:1\r\n"
    <> "T: change key to Gm\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| C4 F4 G4 |[K: Gm] F2F2 C4 G4 |\r\n"

doubleDot :: String
doubleDot =
    "X:1\r\n"
    <> "T: doubly dotted note pairs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| CF3 cF3 G2<<C2 |\r\n"

simpleTie :: String
simpleTie =
    "X:1\r\n"
    <> "T: simple tie\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| C4 F4 G4- G4- | G8 C8 |\r\n"

volta :: String
volta =
    "X:1\r\n"
    <> "T: volta\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "|: C4F4 G4G4 |1 G8 C8 | F8 F8 :|2 C8 C8 | F8 F8 |: E8 E8\r\n"

grace :: String
grace =
    "X:1\r\n"
    <> "T: grace notes\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| G2 {A}G4 F2 C4 {E}F2G2 | c2f2 (3c2{a}f2g2 c2f2 g2>{^g}a2| \r\n"

ornament :: String
ornament =
    "X:1\r\n"
    <> "T: ornaments\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| G2 !trill! G4 F2 !turn! C4  F2G2 | G2 !lowermordent!  G4 F2 !uppermordent! C4 F2G2 | \r\n"

articulation :: String
articulation =
    "X:1\r\n"
    <> "T: articulations\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| G2 H G4 F2 L C4  F2G2 | !tenuto! G2 u G4 F2 v C4  .F2.G2 |\r\n"

lineContinuation :: String
lineContinuation =
    "X:1\r\n"
    <> "T: line continuation\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| C2F2 G2 \\ \r\n"
    <> " G2 F6 =E2 |\r\n"

longLine :: String
longLine =
    "X:1\r\n"
    <> "T: long line\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2  \\ \r\n"
    <> "| C2F2G2G2 F2=eb f2 g2\r\n"

emptyBar :: String
emptyBar =
    "X:1\r\n"
    <> "T: empty bar and double barline\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: D\r\n"
    <> "| C2 F2 G2 | | G2 F2 =EB || A6 |\r\n"

crossBeatTriplet :: String
crossBeatTriplet =
    "X:1\r\n"
    <> "T: triplet spanning beats\r\n"
    <> "M: 2/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: Gm\r\n"
    <> "| d3 (3cBA F3 |\r\n"

stemDirection :: String
stemDirection =
    "X:1\r\n"
    <> "T: stem direction\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| a4 g4 f4 e4 | e4 d4 c4 B4 | c4 B4 A4 G4 |\r\n"

tripletWithTie :: String
tripletWithTie =
  "X: 1\r\n"
  <> "M: 3/4\r\n"
  <> "L: 1/16\r\n"
  <> "K: D\r\n"
  <> "| (3B2G4A6- A2G2 |\r\n"

repetitions :: String
repetitions =
  "X:1\r\n"
  <> "T: coda and da capo\r\n"
  <> "M:2/4\r\n"
  <> "L:1/16\r\n"
  <> "R:marsch\r\n"
  <> "K:Dmaj\r\n"
  <> "| g3f -f2ed | c2A2 ABc2 !D.C.! y || !coda! y [| d2d2 d4 |]\r\n"

gracesWithAccidentals :: String
gracesWithAccidentals =
    "X:1\r\n"
    <> "T: graces with accidentals\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: C\r\n"
    <> "| {d^f}ga |\r\n"    

multipleRepeats :: String
multipleRepeats =
    "X:1\r\n"
    <> "T: volta\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "|:: C4F4 G4G4 | G8 C8 | F8 F8 ::|8\r\n"

