module DoorStatus
  ( getDoorStatus
  , doorStatusSummary
  , DoorStatus
  ) where

import Prelude (pure, show, bind, const, join, flip, (<>), ($), (/=), (<$>), (<<<), (<#>))
import Math ((%))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.StatusCode (StatusCode(..))
import Lenses (_values, _series, _results)
import Moment (Period(..), diff, create, now)
import Data.Lens (lens)
import Data.Lens.Types (LensP)
import Data.Lens.Index (ix)
import Data.Lens.Fold ((^?))
import Data.Lens.Traversal (TraversalP)
import Data.Int (fromNumber)
import Data.Array (findIndex, index, reverse, last, (!!))
import Data.String (toLower)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Traversable (sequence)
import Data.Argonaut.Core (foldJson, foldJsonArray)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))


type Series  = { name :: String, columns :: Array String, values :: Array (Array (Either Number String)) }
type Results = { results :: Array { series :: Array Series }}

data DoorStatus = DoorStatus Results

instance decodeJsonDoorStatus :: DecodeJson DoorStatus where
  decodeJson json = do
    obj <- decodeJson json
    results  <- obj .? "results"
    results' <- sequence $ results <#> \result -> do
      series  <- result .? "series"
      series' <- sequence $ series <#> \elem -> do
        name    <- elem .? "name"
        columns <- elem .? "columns"
        values  <- elem .? "values"
        values' <- sequence $ values <#> \value -> do
          innerVal  <- foldJsonArray (Left "failed to get array") Right value
          innerVal' <- sequence $ innerVal <#> \val -> do
            let failed = (\_ -> Left "failed to get string or number") :: forall a. a -> Either String (Either Number String)
            val' <- (foldJson failed failed (Right <<< Left) (Right <<< Right) failed failed val) :: Either String (Either Number String)
            pure (val' :: Either Number String)
          pure (innerVal' :: Array (Either Number String))
        pure $ ({ name: name, columns: columns, values: values' }) :: Series
      pure $ { series: series' :: Array Series }
    pure $ DoorStatus { results: results' :: Array { series :: Array Series } }


_DoorStatus :: LensP DoorStatus Results 
_DoorStatus = lens (\(DoorStatus rec) -> rec) (\_ -> DoorStatus)

_0result :: TraversalP DoorStatus { series :: Array Series }
_0result = _DoorStatus <<< _results <<< ix 0

_0series :: TraversalP DoorStatus Series 
_0series = _0result <<< _series <<< ix 0

doorStatusUrl :: String
doorStatusUrl = "https://db.softver.org.mk/influxdb/query?db=status&" <> "epoch=ms&" <>
                "q=SELECT+doorstatus+FROM+doorstatus+WHERE+location%3D%27hacklab%27+ORDER+BY+time"


getDoorStatus :: forall e. Aff ( ajax :: AJAX | e ) (Either String DoorStatus)
getDoorStatus = do
  doorStatus <- get doorStatusUrl
  case doorStatus.status of
    (StatusCode 200)  -> pure $ decodeJson doorStatus.response
    (StatusCode code) -> pure (Left $ "Error " <> show code)


doorStatusSummary' :: DoorStatus -> Maybe { status :: String, since :: Number }
doorStatusSummary' result = do
  values <- result ^? (_0series <<< _values)
  status <- join $ (flip index) 1 <$> last values

  let values' = reverse values
  lastChange <- join $ index values' <$> findIndex (\elem -> (elem !! 1) /= (Just status)) values'
  statusSince <- lastChange !! 0
  
  status'      <- either (const Nothing) Just status 
  statusSince' <- either Just (const Nothing) statusSince

  pure { status: status', since: statusSince' }


doorStatusSummary :: DoorStatus -> Maybe String
doorStatusSummary result = do
  summary <- doorStatusSummary' result
  time <- unsafePerformEff $ do
    start <- now
    end   <- create summary.since
    pure $ do
      days  <- fromNumber $ (diff start end Days)    %  1.0
      hours <- fromNumber $ (diff start end Hours)   % 24.0
      mins  <- fromNumber $ (diff start end Minutes) % 60.0 
      secs  <- fromNumber $ (diff start end Seconds) % 60.0
      pure $ (show days) <> " days, " <> (show hours) <> " hours, " <>
             (show mins) <> " minutes, and " <> (show secs) <> " seconds"

  pure $ "Kika has been " <> toLower summary.status <> " for " <> time 
