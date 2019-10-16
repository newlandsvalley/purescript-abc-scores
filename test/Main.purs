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
import VexFlow.Types (BarSpec, Config, VexScore, MusicSpec(..))
import VexFlow.Score (createScore)
import VexFlow.Abc.Beat (exactBeatNumber)
import Test.Samples

main :: Effect Unit
main = runTest do
  configThreadingSuite
  slursSuite
  beatSuite
  beamingSuite
  typesettingSpaceSuite

configure :: AbcTune -> Config
configure tune =
  { parentElementId : "canvas"
  , width : 100
  , height : 100
  , scale : 0.8
  , isSVG : false
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
    -- notes in triplet are too long to be beamed
    test "3/4 slow triplet start" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\n(3e4c4f4 B2A2\r\n"
      Assert.equal (Just [[3,5]]) $
        map (\b -> b.beamSpecs) mFirstBar
    -- notes in triplet are too long to be beamed
    test "3/4 slow triplet end" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3e4c4f4 \r\n"
      Assert.equal (Just []) $
        map (\b -> b.beamSpecs) mFirstBar
    test "split beaming in slow triplet" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3:2:5d2e2c4f2g2 \r\n"
      Assert.equal (Just [[1,3],[4,6]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "split beaming - tuplet before standard" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\n (3:2:4c4d4e2f2 ABcd \r\n"
      Assert.equal (Just [[2,4], [4,8]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "rest in slow triplet" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\nC4 (3:2:4e4z4f2g2 \r\n"
      Assert.equal (Just [[3,5]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "standard/tuplet overlap 1" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 (3f2c2d2 e2c2 f2c2\r\n"
      Assert.equal (Just [[0,5], [5,9]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "standard/tuplet overlap 2" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 f2c2 (3f2c2d2 e2c2 \r\n"
      Assert.equal (Just [[0,4], [4,9]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "short lead-in" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 3/4\r\ncde\r\n"
      Assert.equal (Just [[0,3]]) $
        map (\b -> b.beamSpecs) mFirstBar
    -- common time optimisations
    test "common time optimisation both halves" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 f2c2 e2c2 f2c2\r\n"
      Assert.equal (Just [[0,4], [4,8]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "common time optimisation first half" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne2c2 f2c2 e4 f2c2\r\n"
      Assert.equal (Just [[0,4], [5,7]]) $
        map (\b -> b.beamSpecs) mFirstBar
    test "common time optimisation second half" do
      let
        mFirstBar = getFirstBar "L: 1/16\r\nM: 4/4\r\ne4 f2c2 e2c2 f2c2 \r\n"
      Assert.equal (Just [[1,3], [3,7]]) $
        map (\b -> b.beamSpecs) mFirstBar



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

typesettingSpaceSuite :: Free TestF Unit
typesettingSpaceSuite =
  suite "typesetting spaces" do
    test "pair of ys" do
      let
        mFirstBar = getFirstBar "(3c2d2e2 y f2g2 c2f2 y (3g2a2b2 \r\n"
      Assert.equal (Just [3,7]) $
        map getSpaces mFirstBar
        where
          getSpaces :: BarSpec -> Array Int
          getSpaces bs =
            let
              (MusicSpec ms) = bs.musicSpec
            in
              ms.typesettingSpaces
