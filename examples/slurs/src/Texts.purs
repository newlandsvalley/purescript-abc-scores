module Texts where

import Prelude ((<>))

slurs1 :: String
slurs1 =
    "X:1\r\n"
    <> "T: slurs\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| (A2B2)c2d2 (e2f2g2a2 | (a2g2f2e2) (d2c2BA) y (GF) |\r\n"

slurs2 :: String
slurs2 =
    "X:1\r\n"
    <> "T: slurs 2\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: D\r\n"
    <> "| C2 (F2G2) A2 | (G2 F2 (=EB) cd) | (C2 (3D2E2F2) G2 |\r\n"

brokenRhythmSlurs :: String
brokenRhythmSlurs =
    "X:1\r\n"
    <> "T: broken rhythm slurs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| (A2>B2) (g4 f2>g2) |\r\n"

tupletSlurs :: String
tupletSlurs =
    "X:1\r\n"
    <> "T: tuplet slurs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| (3(A2B2C2) D4 (3:2:4D2E2(FG) |\r\n"
        
tupletPrefaceSlurs :: String
tupletPrefaceSlurs =
    "X:1\r\n"
    <> "T: tuplet preface slurs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| ((3A2B2C2 D4) ((3:2:4D2E2FG) |\r\n"

chordSlurs :: String
chordSlurs =
    "X:1\r\n"
    <> "T: chord slurs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: C\r\n"
    <> "| ([GB] c) dd (e [gb]) |\r\n"
