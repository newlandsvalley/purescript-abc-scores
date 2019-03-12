module Test.Main where

import Prelude (($), Unit, discard, map)
import Effect (Effect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array (head)
import Data.Either (Either(..))
import Data.Rational ((%))
import Control.Monad.Free (Free)
import Data.Abc (AbcTune, PitchClass(..))
import Data.Abc.Parser (parse)
import VexFlow.Types (BarSpec, Config, VexScore)
import VexFlow.Score (createScore)
import VexFlow.Abc.Beat (exactBeatNumber)
import Test.Samples

main :: Effect Unit
main = runTest do
  configThreadingSuite
  beamGroupsSuite
  slursSuite
  beatSuite
  beamingSuite

configure :: AbcTune -> Config
configure tune =
  { canvasDivId : "canvas"
  , canvasWidth : 100
  , canvasHeight : 100
  , scale : 0.8
  }

getFirstBar :: String -> Maybe BarSpec
getFirstBar s =
  case parse s of
    Right abcTune ->
      let
        config = configure abcTune
        vexScore = createScore config abcTune
      in
        firstBarOfScore vexScore
    _ ->
      Nothing


firstBarOfScore :: VexScore -> Maybe BarSpec
firstBarOfScore vexScore =
  case vexScore of
    Right staveSpecs ->
      case (head staveSpecs) of
        Just (Just staveSpec) ->
          head staveSpec.barSpecs
        _ ->
          Nothing
    _ ->
      Nothing

-- | check that the configuration changes are threaded through the translation
configThreadingSuite :: Free TestF Unit
configThreadingSuite =
  suite "config threading" do
    test "meter change (4/4 to 3/4)" do
       let
         initialContext = startAbcContext (Tuple 4 4)
         abcContext = meterChangeTo34 initialContext
       -- key change alters the time signature and beats per beam
       Assert.equal 3 abcContext.timeSignature.numerator
       -- score item sets the first stave number to 0
       Assert.equal (Just 0) abcContext.staveNo
    test "key change (C to G)" do
        let
          initialContext = startAbcContext (Tuple 4 4)
          abcContext = keyChangeToG initialContext
        -- key change alters the time signature and beats per beam
        Assert.equal G abcContext.keySignature.pitchClass
    -- this now depends on varianle bar widths
    test "4 bars width" do
      let
        initialContext = startAbcContext (Tuple 4 4)
        abcContext = accumulateBarWidths initialContext
        -- key change alters the time signature and beats per beam
      Assert.equal 465 abcContext.accumulatedStaveWidth

beamingSuite :: Free TestF Unit
beamingSuite =
  suite "beaming" do
    test "standard 2/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\ne2c2 f2c2\r\n"
      Assert.equal (Just [[0,2], [2,4]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "triplet starting 2/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\n(3d2e2c2 f2c2\r\n"
      Assert.equal (Just [[0,3], [3,5]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "standard 2/4 long introductory note" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\nd4 f2c2\r\n"
      Assert.equal (Just [[1,3]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "triplet starting 2/4" do
     let
       mFirstBar = getFirstBar "L: 1/16\r\nM: 2/4\r\nf2c2 (3d2e2c2 \r\n"
     Assert.equal (Just [[0,2], [2,5]]) $
    map (\b -> b.beamSpecs) mFirstBar
    test "standard 3/4" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\ne2c2 f2c2 B2A2\r\n"
      Assert.equal (Just [[0,2], [2,4], [4,6]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "3/4 slow triplet start" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\n(3e4c4f4 B2A2\r\n"
      Assert.equal (Just [[0,3], [3,5]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "3/4 slow triplet end" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3e4c4f4 \r\n"
      Assert.equal (Just [[1,4]]) $
        map (\b -> b.beamSpecs) mFirstBar







-- deprecateds tests because beam groups will be deprecates
beamGroupsSuite :: Free TestF Unit
beamGroupsSuite =
  suite "beam groups" do
    test "3/2" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 3/2\r\ne2c4 de f2c2\r\n"
      Assert.equal (Just [{ noteCount : 1, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "2/4" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 2/4\r\nCD E/2F/2G/2A/2\r\n"
      Assert.equal (Just [{ noteCount : 1, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "3/4 L=1/8" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 3/4\r\nCD EF GA\r\n"
      Assert.equal (Just [{ noteCount : 1, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "3/4 L=1/16" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC2D2 E2F2 G2A2\r\n"
      Assert.equal (Just [{ noteCount : 1, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "6/8" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 6/8\r\nCDE FGA\r\n"
      Assert.equal (Just [{ noteCount : 3, noteKind : 8}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "9/8" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 9/8\r\nCDE FGA Bcd\r\n"
      Assert.equal (Just [{ noteCount : 3, noteKind : 8}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "12/8" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 12/8\r\nCDE FGA Bcd efg\r\n"
      Assert.equal (Just [{ noteCount : 3, noteKind : 8}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "standard reel (4/4)" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 4/4\r\nCDEF FGAB\r\n"
      Assert.equal (Just [{ noteCount : 2, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "standard hornpipe (4/4) L=1/8" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 4/4\r\nC>DE>F F>GA>B\r\n"
      Assert.equal (Just [{ noteCount : 2, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "standard hornpipe (4/4) L=1/16" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\nC3DE3F F3GA3B\r\n"
      Assert.equal (Just [{ noteCount : 2, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar
    test "other 4/4" do
      let
        mFirstBar = getFirstBar "L: 1/8\r\nM: 4/4\r\nC2EF F2AB\r\n"
      Assert.equal (Just [{ noteCount : 1, noteKind : 4}]) $
        map (\b -> b.beamGroups) mFirstBar

slursSuite :: Free TestF Unit
slursSuite =
  suite "slurs" do
    test "simple" do
      let
        mFirstBar = getFirstBar "CDE (FG)A Bcd efg\r\n"
      Assert.equal (Just [{ from : 3, to : 4}]) $
        map (\b -> b.curves) mFirstBar
    test "nested" do
      let
        mFirstBar = getFirstBar "CDE (F(GA) B)cd efg\r\n"
      Assert.equal (Just [{ from : 4, to : 5}, { from : 3, to : 6}]) $
        map (\b -> b.curves) mFirstBar
    test "unmatched start" do
      let
        mFirstBar = getFirstBar "(CDE (FG)A Bcd efg\r\n"
      Assert.equal (Just [{ from : 3, to : 4}]) $
        map (\b -> b.curves) mFirstBar
    test "unmatched end" do
      let
        mFirstBar = getFirstBar "CDE (FG)A Bcd) efg\r\n"
      Assert.equal (Just [{ from : 3, to : 4}]) $
        map (\b -> b.curves) mFirstBar

beatSuite :: Free TestF Unit
beatSuite =
  suite "beats" do
    test "beat 0" do
      Assert.equal (Just { beatNumber: 0, noteIndex: 1})
        $ exactBeatNumber (0 % 2) (1 % 4) 1
    test "beat 1" do
      Assert.equal (Just { beatNumber: 1, noteIndex: 1})
        $ exactBeatNumber (1 % 4) (1 % 4) 1
    test "beat 2" do
      Assert.equal  (Just { beatNumber: 2, noteIndex: 1})
        $ exactBeatNumber (1 % 2) (1 % 4) 1
    test "beat 3" do
      Assert.equal (Just { beatNumber: 3, noteIndex: 2})
        $ exactBeatNumber (3 % 4) (1 % 4) 2
    test "off beat" do
      Assert.equal Nothing
        $ exactBeatNumber (2 % 5) (1 % 4) 1
