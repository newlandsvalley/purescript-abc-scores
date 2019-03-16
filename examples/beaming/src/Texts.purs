module Examples.Beaming.Texts where

import Prelude ((<>))

beaming44a :: String
beaming44a =
    "X:1\r\n"
    <> "T: Beaming in 4/4  - 2,1,1\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| c2f2g2g2 f4 e2e2 |\r\n"

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

beaming32 :: String
beaming32 =
    "X:1\r\n"
    <> "T: Beaming in 3/2\r\n"
    <> "M: 3/2\r\n"
    <> "L: 1/8\r\n"
    <> "K: Dm\r\n"
    <> "| e2c4 de f2c2 | d3c c2BA B3A |\r\n"
