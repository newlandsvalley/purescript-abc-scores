module Examples.Clefs.Texts where

import Prelude ((<>))

useClef :: String -> String
useClef clef = 
    "X:1\r\n"
    <> "T: use provided clef\r\n"
    <> "M: 3/2\r\n"
    <> "L: 1/8\r\n"
    <> "K: D\r\n"
    <> "V: B1 clef=" <> clef <> "\r\n"
    <> "A,2 A,4 A,2 DCB,A, | D,2 D,4 A,2 CDEF | (3G,4A,4G,4  D,E,F,G, | z2 A,4 A,2 [G,4D,4] |\r\n"