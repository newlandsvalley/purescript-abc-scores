module Examples.ChordSymbols.Texts where

import Prelude ((<>))

-- The tempo marking must be against stave 0, bar o but for 
-- some reason, it does not display here
tempoMarking :: String
tempoMarking =
    "X: 1\r\n"
    <> "T: chord symbols plus tempo marking\r\n"
    <> "Q: 1/4=120\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: C\r\n"
    <> "\"C\" c2 cd ef | gg/a/ ge ca | \"F\" a>g \"Em\" ge \"Am\" c2 |\r\n"      

basic :: String
basic =
    "X:1\r\n"
    <> "T: basic chord symbols\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: G\r\n"
    <> "\"G\"G2G>B \"D7\"A>F | \"G\"G2B>dg2 |\r\n"

tuplet :: String 
tuplet =
    "X:1\r\n"
    <> "T:chord symbols plus tuplet\r\n"
    <> "R:Polska\r\n"
    <> "L:1/8\r\n"
    <> "M:3/4\r\n"
    <> "K:Bm\r\n"
    <> "\"Bm\"(3FGFB2F>B | \"F#7\"^A>B c2 e>f | \"Bm\"(3ded (3cdc ^A>B | \"F#7\"c/d/c/^A/ B>A F2 |\r\n"


voltaBrackets :: String
voltaBrackets =
    "X:1\r\n"
    <> "T: chord symbols plus volta brackets\r\n"
    <> "M: 2/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> " \"G\" B2BG BGBG |1 \"D\" B2A2 A4 :|2 \"D\" B2A2 A3c |\r\n"

ornament :: String
ornament =
    "X:1\r\n"
    <> "T: chord symbols plus ornaments\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: G\r\n"
    <> "| G2 \"G\" !trill! G4 F4 | \"Am\" !turn! C4  F2G2 |\r\n"        