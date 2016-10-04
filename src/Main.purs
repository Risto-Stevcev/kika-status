module Main where

import NumDevices (getNumDevices, numDevicesSummary)
import DoorStatus (getDoorStatus, doorStatusSummary)
import Prelude (id, bind, unit, pure, ($), (<$>), (*))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Canceler, launchAff, later')
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Network.HTTP.Affjax (AJAX)
import Data.Either (either)
import Data.Maybe (maybe)


pollInterval :: Int
pollInterval = 30 * 60 * 1000  -- in minutes


main
  :: forall e
  . Eff ( err :: EXCEPTION, ajax :: AJAX, console :: CONSOLE | e )
        (Canceler ( ajax :: AJAX, console :: CONSOLE | e ))
main = launchAff $ forever $ do
  later' 5000 (pure unit)

  -- The status of the lab
  doorStatus <- getDoorStatus
  let summary = doorStatusSummary <$> doorStatus
  logShow $ either id (\s -> maybe "" id s) $ summary

  -- The number of devices connected
  numDevices <- getNumDevices
  let summary' = numDevicesSummary <$> numDevices
  logShow $ either id (\s -> maybe "" id s) $ summary'
