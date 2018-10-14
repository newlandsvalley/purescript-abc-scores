module Test.Main where

import Prelude (Unit, discard)
import Effect (Effect)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Control.Monad.Free (Free)
import Test.Samples

main :: Effect Unit
main = runTest do
  configThreadingSuite

-- | check that the configuration changes are threaded through the translation
configThreadingSuite :: Free TestF Unit
configThreadingSuite =
  suite "config threading" do
    test "key change (4/4 to 3/4)" do
       let
         initialContext = startAbcContext (Tuple 4 4)
         abcContext = meterChangeTo34 initialContext
       -- key change alters the time signature and beats per beam
       Assert.equal 3 abcContext.timeSignature.numerator
       Assert.equal 1 abcContext.beatsPerBeam
       -- score item sets the first stave number to 0
       Assert.equal (Just 0) abcContext.staveNo
    -- this test must be re-written when we implement variable length bars
    test "4 bars width" do
      let
        initialContext = startAbcContext (Tuple 4 4)
        abcContext = accumulateBarWidths initialContext
        -- key change alters the time signature and beats per beam
      Assert.equal 1010 abcContext.accumulatedStaveWidth
