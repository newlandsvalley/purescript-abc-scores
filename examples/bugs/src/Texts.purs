module Examples.Bugs.Texts where

import Prelude ((<>))

crossBeat16th :: String
crossBeat16th =
    "X:1\r\n"
    <> "T: beam 16th with small tuplet crossing the beat\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "ABCDE | A2B2c (3cde (3cde ABc |\r\n"


minimLayout :: String
minimLayout =
    "X:1\r\n"
    <> "T: minim layout\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: C\r\n"
    <> "| c4 de | ed c4 |\r\n"


horizontalLayout :: String 
horizontalLayout = 
    "x: 1\r\n"
    <> "T: horizontal layout\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: AMinor\r\n"
    <> "e2>a2 a4>e4 | gfed e4>B4 | cdcB A2B2 c2d2 | e4 e8 |\r\n"

commonTimeBeaming :: String
commonTimeBeaming =
    "X:1\r\n"
    <> "T: common time beaming\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: D\r\n"
    <> "| DEFG GABA | DEFG GA (3BAG | DEFG GAB/A/G |\r\n"

voltaBrackets :: String
voltaBrackets =
    "X:1\r\n"
    <> "T: volta brackets\r\n"
    <> "M: 2/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "B2BG BGBG |1 B2A2 A4 :|2 B2A2 A3c |[M: 3/4] e2gf d8 |\r\n"

bSharp :: String
bSharp =
    "X:1\r\n"
    <> "T: B#\r\n"
    <> "M: 3/2\r\n"
    <> "L: 1/8\r\n"
    <> "K: G\r\n"
    <> "c4 c2B2 A3G | G6 ^B2 ^c2^B2 |\r\n"

threeTwoBeaming :: String
threeTwoBeaming =
    "X:1\r\n"
    <> "T: three-two beaming\r\n"
    <> "M: 3/2\r\n"
    <> "L: 1/4\r\n"
    <> "K: D\r\n"
    <> "a g/a/ b a/g/ a g/a/|\r\n"

spacing :: String
spacing =
    "X:1\r\n"
    <> "T: spacing\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: E\r\n"
    <> "| C6 |\r\n"     
    <> "| C6 |\r\n"    
    
spacing1 :: String
spacing1 =
    "X:1\r\n"
    <> "T: spacing 1\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "Q: 1/4=70\r\n"
    <> "K: Gm\r\n"
    <> "G | C6 |\r\n"     

