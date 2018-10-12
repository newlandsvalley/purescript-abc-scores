module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Unit (Test, TestF, suite, test, success, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
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
         abcConfig = meterChangeTo34
       Assert.equal 3 abcConfig.timeSignature.numerator
       Assert.equal 1 abcConfig.beatsPerBeam
