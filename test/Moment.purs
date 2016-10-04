module Test.Moment where

import Prelude
import Moment (MOMENT, Period(Hours), now, create, diff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Data.Either (Either(..), either)
import Test.Unit (describe, it)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert, shouldEqual) 
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck)


main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, moment :: MOMENT, random :: RANDOM | e ) Unit 
main = runTest do
  describe "now" do
    it "should create a moment object with the current time" do
      currentTime <- liftEff' now 
      --logShow currentTime
      assert "failed to create moment" $ either (const false) (const true) currentTime

  describe "create" do
    it "should create a moment object with the given time" do
      newTime <- liftEff' $ create 1234.0
      --logShow currentTime
      assert "failed to create moment" $ either (const false) (const true) newTime

    it "should create a moment object with any given time" do
      quickCheck \time -> true <$ unsafePerformEff $ create time

  describe "diff" do
    it "should be able to diff two different times" do
      currentTime <- liftEff' now
      newTime     <- liftEff' $ create 1475450837174.0
      let diffed = diff <$> currentTime <*> newTime <*> (pure Hours)
      --logShow diffed

      case diffed of
        Right diffed' -> (diffed' > 24.0) `shouldEqual` true
        Left _ -> assert "failed to create time" false

    it "should create a moment diff with any given times" do
      quickCheck \startTime endTime period -> true <$ unsafePerformEff $ do
        start  <- create startTime
        end    <- create endTime
        pure $ diff start end period


