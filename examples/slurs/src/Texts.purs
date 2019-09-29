module Examples.Slurs.Texts where

import Prelude ((<>))

slurs1 :: String
slurs1 =
    "X:1\r\n"
    <> "T: slurs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| (A2B2)c2d2 (e2f2g2a2 | (a2g2f2e2) (d2c2BA) (GF) |\r\n"

slurs2 :: String
slurs2 =
    "X:1\r\n"
    <> "T: double bar line\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: D\r\n"
    <> "| C2 (F2G2) A2 | (G2 F2 (=EB) cd) | (C2 (3D2E2F2) G2 |\r\n"

brokenRhythmSlurs :: String
brokenRhythmSlurs =
    "X:1\r\n"
    <> "T: slurs\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| (A2>B2) (g4 f2>g2) |\r\n"
