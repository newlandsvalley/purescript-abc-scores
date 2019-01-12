module Examples.DisplayTests.Texts where

import Prelude ((<>))

simple68 :: String
simple68 =
  "X:1\r\n"
  <> "T: simple 6/8\r\n"
  <> "M: 6/8\r\n"
  <> "L: 1/16\r\n"
  <> "K: E\r\n"
  <> "| C2F2G2G2 F2 =EB | C2F2G2G2 F2 C2 |\r\n"

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
  <> "| C2F2G2G2 [G8C8F8] | [G4C4F4] [G4C4F4] F2C2 G3G1 |\r\n"

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
    <> "| G2 {A}G4 F2 C4 {E}F2G2 | c2f2 (3c2{a}f2g2 c2f2 g2>{b}a2| \r\n"

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
    <> "T: empty bar\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| C2 F2 G2 | | G2 F2 =EB |\r\n"

doubleBarLine :: String
doubleBarLine =
    "X:1\r\n"
    <> "T: double bar line\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| C2 F2 G2 || G2 F2 =EB |\r\n"
