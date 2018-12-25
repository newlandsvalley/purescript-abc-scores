module Examples.Beaming.Texts where

import Prelude ((<>))

beaming44a :: String
beaming44a =
    "X:1\r\n"
    <> "T: Beaming in 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| c2f2g2g2 f4 e2e2 |\r\n"

beaming44b :: String
beaming44b =
    "X:1\r\n"
    <> "T: Beaming in 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| f4 e2e2 c2f2g2g2 |\r\n"

beaming44c :: String
beaming44c =
    "X:1\r\n"
    <> "T: Beaming in 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| f4 e2e2 f4 e2e2|\r\n"

beaming44d :: String
beaming44d =
    "X:1\r\n"
    <> "T: Beaming in 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "| c2f2g2g2 c2f2g2g2 |\r\n"
