module Examples.StaveAlignment.Texts where

import Prelude ((<>))

augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

fastan :: String
fastan =
  "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | ge3 c4 A4- | (3:5:3A4B4cBA2 B2d2 | de3 c8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | (3:4:3g2a2f4g4 e4- | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"

ewa :: String
ewa =
  "T: Ewa\r\n"
  <> "R: polska\r\n"
  <> "C: Josefina Paulson\r\n"
  <> "M: 9/8\r\n"
  <> "L: 1/8\r\n"
  <> "Q: 3/8=110\r\n"
  <> "K: G\r\n"
  <> "|: D2E D3 B3- | B2A B2c BAB | cB2 (2:3AG FEF | GD2 C3 B,3 |\r\n"
  <> "| D2E D3 B3- | B2A B2c BAB | cB2 (2:3AG FEF | G3 G6 :| \r\n"
  <> "|: dBd g2f (2:3ed | cB2 (2:3AG F2G | (2:3AB cAc BGB | AF2  D6 |\r\n"
  <> "| dBd g2f (2:3ed |  cB2 (2:3AG F2G | (2:3AB cAc BGB | AF2  G6 :|\r\n"

cig :: String
cig =
  "X: 1\r\n"
  <> "T: C i G, Grind Hans Jässpôdspolska\r\n"
  <> "Z: Christine Dyer\r\n"
  <> "R: Polska\r\n"
  <> "O: Rättvik, Dalarna\r\n"
  <> "M: 3/4\r\n"
  <> "L: 1/8\r\n"
  <> "K:Gmaj\r\n"
  <> "|: D2 G2 A>B | (3c2B2G2 E2- | E2 c>B A>G | F/2G/2A- AF D2- |\r\n"
  <> "D2 G2 A>B | (3c2B2G2 E2- | E2 c>B AG |1 (3FDF G4 :|2  (3FDF G3 D |\r\n"
  <> "|: G>B dc B>A | G<c ed c>B | A>B c/2d/2c/2A/2 B>G |F/2G/2A- AF D>D |\r\n"
  <> "G>B dc B>A | G<c ed c>B | A>B c/2d/2c/2A/2 B>G |1 FD/2F/2 G3 D :|2 FD/2F/2 G4 |\r\n"
  <> "|  |\r\n"

smalandPolska :: String
smalandPolska =
  "X:1\r\n"
  <> "T:Polska från Småland\r\n"
  <> "M:3/4\r\n"
  <> "L:1/16\r\n"
  <> "K:Bmin\r\n"
  <> "R:polska\r\n"
  <> "|: B4 A4 (B4 | d2)f2 e2dc c2(d2 |B2)B2 A2A2 B2B2 |d2f2 e2dc d4 |\r\n"
  <> "F2GA B2AB c2Bc |d2cd (ed)cB A2(F2 | F2)GA B2AB c2Bc |d2cd (ed)cB A2(F2 |\r\n"
  <> "F2)GA (B2c2) d3B |(B2A2) B8 :|\r\n"
  <> "K:Amaj\r\n"
  <> "|: f4 e4 (f4 |g2)a2 b2ag g2(a2 |f2)f2 e2e2 f2f2 |g2a2 b2ag a4 |\r\n"
  <> "c2de f2ef g2fg |a2ga (ba)gf e2c2 | c2de f2ef g2fg |a2ga (ba)gf e2c2 |\r\n"
  <> "c2de f2g2 a3f |f2e2 f8 :|\r\n"

-- this sample shows a continuation character
blomgren :: String
blomgren =
  "X:11\r\n"
  <> "T:Polonäs i Gm efter Jean E Blomgren\r\n"
  <> "R:Slängpolska\r\n"
  <> "M:3/4\r\n"
  <> "L:1/16\r\n"
  <> "Q:1/4=120\r\n"
  <> "K:Gm\r\n"
  <> "GFGA B4 A4 | G2g2 bagf g2d2 | \\\r\n"
  <> "edcB dcBA cBAG | F2G2 A2d2 D4 | \r\n"
  <> "GFGA B4 A4 | G2g2 bagf g2d2 | edcB dcBA cBAG | FAdF G4 G,4 :: \r\n"
  <> "BdBG BdBG BdBG | c2cd e2d2 c2B2 | AcAF AcAF AcAF | BABc d2B2 A2F2 | \r\n"
  <> "GFGA B4 A4 | G2g2 bagf g2d2 | edcB dcBA cBAG | FAdF G4 G,4 :|\r\n"


keyChangeSample :: String
keyChangeSample =
  "K: D\r\n"
  <> "M: 4/4\r\n"
  <> "| A B c |[K: A] F |\r\n"
  <> "| f g a |\r\n"

meterChangeSample :: String
meterChangeSample =
  "K: D\r\n"
  <> "M: 4/4\r\n"
  <> "| A B c |\r\n"
  <> "M: 3/4\r\n"
  <> "| f g a |\r\n"
