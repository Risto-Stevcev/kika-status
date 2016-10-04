module NumDevices
  ( getNumDevices
  , numDevicesSummary
  , NumDevices
  ) where

import Prelude (show, pure, bind, (<>), ($), (<<<), (<#>))
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (get, AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Lenses (_values, _series, _results)
import Data.Int (fromNumber)
import Data.Lens (lens)
import Data.Lens.Types (LensP)
import Data.Lens.Index (ix)
import Data.Lens.Fold ((^?))
import Data.Lens.Traversal (TraversalP)
import Data.Array ((!!), last)
import Data.Maybe (Maybe)
import Data.Either (Either(Left))
import Data.Traversable (sequence)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))


type Series  = { name :: String, columns :: Array String, values :: Array (Array Number) }
type Results = { results :: Array { series :: Array Series }}

data NumDevices = NumDevices Results

instance decodeJsonNumDevices :: DecodeJson NumDevices where
  decodeJson json = do
    obj <- decodeJson json
    results  <- obj .? "results"
    results' <- sequence $ results <#> \result -> do
      series  <- result .? "series"
      series' <- sequence $ series <#> \elem -> do
        name    <- elem .? "name"
        columns <- elem .? "columns"
        values  <- elem .? "values"
        pure $ ({ name: name, columns: columns, values: values }) :: Series
      pure $ { series: series' :: Array Series }
    pure $ NumDevices { results: results' :: Array { series :: Array Series } }


_NumDevices :: LensP NumDevices Results 
_NumDevices = lens (\(NumDevices rec) -> rec) (\_ -> NumDevices)

_0result :: TraversalP NumDevices { series :: Array Series }
_0result = _NumDevices <<< _results <<< ix 0

_0series :: TraversalP NumDevices Series 
_0series = _0result <<< _series <<< ix 0


numDevicesUrl :: String
numDevicesUrl = "https://db.softver.org.mk/influxdb/query?" <> "db=status&" <> "epoch=ms&" <>
                "q=SELECT+value%2Ctotal+FROM+landevices+WHERE+location%3D%27hacklab%27+ORDER+BY+time+DESC+LIMIT+1"


getNumDevices :: forall e. Aff ( ajax :: AJAX | e ) (Either String NumDevices)
getNumDevices = do
  numDevices <- get numDevicesUrl
  case numDevices.status of
    (StatusCode 200)  -> pure $ decodeJson numDevices.response
    (StatusCode code) -> pure (Left $ "Error " <> show code)


numDevicesSummary' :: NumDevices -> Maybe { connected :: Number, total :: Number }
numDevicesSummary' result = do
  values <- result ^? (_0series <<< _values)
  latest <- last values
  connected <- latest !! 1
  total     <- latest !! 2 
  pure { connected: connected, total: total }


numDevicesSummary :: NumDevices -> Maybe String
numDevicesSummary result = do
  summary   <- numDevicesSummary' result
  connected <- fromNumber summary.connected
  total     <- fromNumber summary.total
  pure $ show connected <> " devices connected out of " <> show total
