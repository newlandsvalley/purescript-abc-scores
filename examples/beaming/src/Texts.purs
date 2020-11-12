module Examples.Beaming.Texts where

import Prelude ((<>))

beaming44a :: String
beaming44a =
    "X:1\r\n"
    <> "T: Beaming in 4/4  - 2,1,1\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "B | c2f2g2g2 f4 e2e2 |\r\n"

beaming44b :: String
beaming44b =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - lead-in\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "de | f4 e2e2 c2f2g2g2 |\r\n"

beaming44c :: String
beaming44c =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - 1\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| f4 e2e2 f4 e2e2|\r\n"

beaming44d :: String
beaming44d =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - 2\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| c2f2g2g2 c2f2g2g2 |\r\n"

beaming44e :: String
beaming44e =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - grace\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| c2f2g2g2 c2f2{a}g2g2  |\r\n"

beaming44f :: String
beaming44f =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - 16th notes\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| c2f2g2g2 c2f2g2ga  |\r\n"

beaming44g :: String
beaming44g =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - schottis\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| A4 A2Bc F8 |\r\n"

beaming44h :: String
beaming44h =
    "X:1\r\n"
    <> "T: Beaming in 4/4 - subsumed triplets\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| (3c2d2e2 f2g2 c2f2 (3g2a2b2 |\r\n"

beaming32a :: String
beaming32a =
    "X:1\r\n"
    <> "T: Beaming in 3/2\r\n"
    <> "M: 3/2\r\n"
    <> "L: 1/8\r\n"
    <> "K: Dm\r\n"
    <> "| e2c4 de f2c2 | d3c c2BA B3A |\r\n"

beaming32b :: String
beaming32b =
    "X:1\r\n"
    <> "T:3/2Hornpipe\r\n"
    <> "M:3/2\r\n"
    <> "L:1/8\r\n"
    <> "K:Gm\r\n"
    <> "G4 D4 GABG | ABcA B2d2 F2D2 |\r\n"

beam16thInTuplet :: String
beam16thInTuplet =
    "X:1\r\n"
    <> "T: beam 16th notes inside long tuplets\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| (3:2:4c4d4e2f2 ABcd | (3:2:5B2c2d4e2f2 ABcd |\r\n"


beamingCommonTimeCoalesce :: String
beamingCommonTimeCoalesce  =
    "X:1\r\n"
    <> "T: coalesce beaming incommon time\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: D\r\n"
    <> "| DEFG GABA | DEFG GA (3BAG | DEFG GAB/A/G |\r\n"

beamingTypesettingSpace1 :: String
beamingTypesettingSpace1 =
    "X:1\r\n"
    <> "T: beaming with typesetting space between beats in 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| (3c2d2e2 y f2g2 c2f2 y g2a2 |\r\n"

beamingTypesettingSpace2 :: String
beamingTypesettingSpace2 =
    "X:1\r\n"
    <> "T: beaming with typesetting space within beat\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: D\r\n"
    <> "| (3cde y fg f2g2 c2f2 |\r\n"
