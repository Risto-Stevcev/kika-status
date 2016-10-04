module Test.DoorStatus where

import DoorStatus (DoorStatus, getDoorStatus, doorStatusSummary)
import Prelude (const, bind, ($), (<$>), Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Network.HTTP.Affjax (AJAX)
import Test.Unit (it, describe)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert) 
import Test.Unit.Console (TESTOUTPUT)
import Data.Maybe (maybe)
import Data.Either (Either(..), either, fromRight)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Partial.Unsafe (unsafePartial)


main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, ajax :: AJAX | e ) Unit
main = runTest do
  describe "decodeJsonDoorStatus" do
    let jsonStr = "{\"results\":[{\"series\":[{\"name\":\"landevices\",\"columns\":[\"time\",\"value\",\"total\"],\"values\":[[1475369502000,\"OPEN\"],[1475369702000,\"CLOSED\"]]}]}]}"

    it "should be able to decode DoorStatus from Json" do
      let json = unsafePartial $ fromRight $ jsonParser jsonStr 
      let doorStatus = decodeJson json :: Either String DoorStatus
      --logShow $ numDevicesSummary <$> numDevices
      assert "failed to decode DoorStatus" $ either (const false) (const true) doorStatus

    it "should get the DoorStatus summary" do
      let json = unsafePartial $ fromRight $ jsonParser jsonStr 
      let doorStatus = decodeJson json :: Either String DoorStatus
      let summary = doorStatusSummary <$> doorStatus
      --logShow summary
      
      case summary of
        Right summary' ->
          assert "Couldn't extract summary" $ maybe false (const true) summary'
        Left msg -> assert msg false


  describe "getDoorStatus" do
    it "should get the door status without failing" do
      doorStatus <- getDoorStatus
      let summary = doorStatusSummary <$> doorStatus
      --logShow summary

      case summary of
        Right summary' ->
          assert "Couldn't extract summary" $ maybe false (const true) summary'
        Left msg -> assert msg false
