module Test.NumDevices where

import Prelude (const, bind, ($), (<$>), Unit)
import NumDevices (NumDevices, getNumDevices, numDevicesSummary) 
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
  describe "decodeJsonNumDevices" do
    it "should be able to decode NumDevices from Json" do
      let jsonStr = "{\"results\":[{\"series\":[{\"name\":\"landevices\",\"columns\":[\"time\",\"value\",\"total\"],\"values\":[[1475369702000,0,2]]}]}]}"
      let json = unsafePartial $ fromRight $ jsonParser jsonStr 
      let numDevices = decodeJson json :: Either String NumDevices
      --logShow $ numDevicesSummary <$> numDevices
      assert "failed to decode NumDevices" $ either (const false) (const true) numDevices

  describe "getNumDevices" do
    it "should get the number of devices without failing" do
      numDevices <- getNumDevices
      let summary = numDevicesSummary <$> numDevices
      --logShow summary

      case summary of
        Right summary' ->
          assert "Couldn't extract summary" $ maybe false (const true) summary'
        Left msg -> assert msg false
