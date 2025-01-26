module Test.Main where

import Test.Samples

import Data.Abc (PitchClass(..))
import Data.Abc.Parser (parse)
import Data.Abc.Meter (commonTime)
import Data.Array (head)
import Data.Array.NonEmpty (head) as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude ((<>), Unit, discard, map, show)
import VexFlow.Abc.Alignment (justifiedScoreConfig)
import VexFlow.Abc.Beat (exactBeatNumber)
import VexFlow.Score (createScore)
import VexFlow.Types (BarSpec, Config, VexScore, MusicSpec(..), Titling(..), defaultConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter] do 
  describe "abc-scores" do
    configThreadingSpec
    slursSpec
    beatSpec
    beamingSpec
    typesettingSpaceSpec
    alignmentSpec

initialConfig :: Config
initialConfig =
  defaultConfig
    { width = 100
    , height = 100
    , isSVG = false
    , titling = NoTitle
    }

assertScoreDepth :: String -> Int -> Aff Unit
assertScoreDepth s expected = do
  case parse s of
    Right tune -> do
      let
        score = createScore initialConfig tune
        newConfig = justifiedScoreConfig score initialConfig
      expected `shouldEqual` newConfig.height

    Left err ->
      fail ("parse failed: " <> (show err))

getFirstBar :: String -> Maybe BarSpec
getFirstBar s =
  case parse s of
    Right abcTune ->
      let
        vexScore = createScore initialConfig abcTune
      in
        firstBarOfScore vexScore
    _ ->
      Nothing

firstBarOfScore :: VexScore -> Maybe BarSpec
firstBarOfScore vexScore =
  case vexScore of
    Right staveSpecs ->
      let 
        staveSpec = NEA.head staveSpecs
      in
        head staveSpec.barSpecs
    _ ->
      Nothing

-- | check that the configuration changes are threaded through the translation
configThreadingSpec :: Spec Unit
configThreadingSpec =
  describe "config threading" do
    it "handles meter change (4/4 to 3/4)" do
      let
        initialContext = startAbcContext commonTime
        abcContext = meterChangeTo34 initialContext
      -- key change alters the time signature and beats per beam
      3 `shouldEqual` abcContext.timeSignature.numerator
      -- score item sets the first stave number to 0
      (Just 0) `shouldEqual` abcContext.staveNo
    it "handles key change (C to G)" do
      let
        initialContext = startAbcContext commonTime
        abcContext = keyChangeToG initialContext
      -- key change alters the time signature and beats per beam
      G `shouldEqual` abcContext.keySignature.pitchClass
    -- this depeds on the noteSeparation constant in TickableContext
    it "calculates width of 4 bars" do
      let
        initialContext = startAbcContext commonTime
        abcContext = accumulateBarWidths initialContext
      -- 476 for a noteSeparation of 35.0
      -- 476 `shouldEqual` abcContext.accumulatedStaveWidth
      -- 409 for a noteSeparation of 30.0
      -- 436 for a noteSeparation of 32.0
      436 `shouldEqual` abcContext.accumulatedStaveWidth

beamingSpec :: Spec Unit
beamingSpec =
  describe "beaming" do
    it "beams standard 2/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\ne2c2 f2c2\r\n"
      (Just [ [ 0, 2 ], [ 2, 4 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams triplet starting 2/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\n(3d2e2c2 f2c2\r\n"
      (Just [ [ 0, 3 ], [ 3, 5 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams standard 2/4 long introductory note" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\nd4 f2c2\r\n"
      (Just [ [ 1, 3 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams triplet starting 2/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\nf2c2 (3d2e2c2 \r\n"
      (Just [ [ 0, 2 ], [ 2, 5 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams standard 3/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\ne2c2 f2c2 B2A2\r\n"
      (Just [ [ 0, 2 ], [ 2, 4 ], [ 4, 6 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    -- notes in triplet are too long to be beamed
    it "beams 3/4 slow triplet start" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\n(3e4c4f4 B2A2\r\n"
      (Just [ [ 3, 5 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    -- notes in triplet are too long to be beamed
    it "beams 3/4 slow triplet end" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3e4c4f4 \r\n"
      (Just []) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "splits beaming in slow triplet" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3:2:5d2e2c4f2g2 \r\n"
      (Just [ [ 1, 3 ], [ 4, 6 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "splits beaming - tuplet before standard" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\n (3:2:4c4d4e2f2 ABcd \r\n"
      (Just [ [ 2, 4 ], [ 4, 8 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams rest in slow triplet" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3:2:4e4z4f2g2 \r\n"
      (Just [ [ 3, 5 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams standard/tuplet overlap 1" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 (3f2c2d2 e2c2 f2c2\r\n"
      (Just [ [ 0, 2 ], [ 2, 5 ], [ 5, 9 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams standard/tuplet overlap 2" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 f2c2 (3f2c2d2 e2c2 \r\n"
      (Just [ [ 0, 4 ], [ 4, 7 ], [ 7, 9 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "beams short lead-in" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\ncde\r\n"
      (Just [ [ 0, 3 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    -- common time optimisations
    it "handles common time optimisation both halves" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 f2c2 e2c2 f2c2\r\n"
      (Just [ [ 0, 4 ], [ 4, 8 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "handles common time optimisation first half" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 f2c2 e4 f2c2\r\n"
      (Just [ [ 0, 4 ], [ 5, 7 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar
    it "handles common time optimisation second half" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne4 f2c2 e2c2 f2c2 \r\n"
      (Just [ [ 1, 3 ], [ 3, 7 ] ]) `shouldEqual`
        map (\b -> b.beamSpecs) mFirstBar

slursSpec :: Spec Unit
slursSpec =
  describe "slurs" do
    it "handles simple slurs" do
      let
        mFirstBar = getFirstBar "CDE (FG)A Bcd efg\r\n"
      (Just [ { from: 3, to: 4 } ]) `shouldEqual`
        map (\b -> b.curves) mFirstBar
    it "handles nested slurs" do
      let
        mFirstBar = getFirstBar "CDE (F(GA) B)cd efg\r\n"
      (Just [ { from: 4, to: 5 }, { from: 3, to: 6 } ]) `shouldEqual`
        map (\b -> b.curves) mFirstBar
    it "handles an unmatched start" do
      let
        mFirstBar = getFirstBar "(CDE (FG)A Bcd efg\r\n"
      (Just [ { from: 3, to: 4 } ]) `shouldEqual`
        map (\b -> b.curves) mFirstBar
    it "handles an unmatched end" do
      let
        mFirstBar = getFirstBar "CDE (FG)A Bcd) efg\r\n"
      (Just [ { from: 3, to: 4 } ]) `shouldEqual`
        map (\b -> b.curves) mFirstBar

beatSpec :: Spec Unit
beatSpec =
  describe "beats" do
    it "handles beat 0" do
      Nothing `shouldEqual` exactBeatNumber (0 % 2) (1 % 4) 0
    it "calculates beat 1" do
      (Just { beatNumber: 1, noteIndex: 1 }) `shouldEqual`
        exactBeatNumber (1 % 4) (1 % 4) 1
    it "calculates beat 2" do
      (Just { beatNumber: 2, noteIndex: 1 }) `shouldEqual`
        exactBeatNumber (1 % 2) (1 % 4) 1
    it "calculates beat 3" do
      (Just { beatNumber: 3, noteIndex: 2 }) `shouldEqual`
        exactBeatNumber (3 % 4) (1 % 4) 2
    it "recognizes off beat" do
      Nothing `shouldEqual` exactBeatNumber (2 % 5) (1 % 4) 1

typesettingSpaceSpec :: Spec Unit
typesettingSpaceSpec =
  describe "typesetting spaces" do
    it "handles a pair of ys" do
      let
        mFirstBar = getFirstBar "(3c2d2e2 y f2g2 c2f2 y (3g2a2b2 \r\n"
      (Just [ 3, 7 ]) `shouldEqual` (map getSpaces mFirstBar)
  where
  getSpaces :: BarSpec -> Array Int
  getSpaces bs =
    let
      (MusicSpec ms) = bs.musicSpec
    in
      ms.typesettingSpaces

alignmentSpec :: Spec Unit
alignmentSpec =
  describe "alignment" do
    it "calculates depth of borddajnsijn" do
      assertScoreDepth borddajnsijn 188

