module Moment
  ( now
  , create
  , diff
  , Moment
  , MOMENT
  , Period(..)
  ) where

import Prelude (show, bind, pure, ($))
import Control.Monad.Eff (Eff)
import Data.Show (class Show)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Arbitrary (class Arbitrary)


foreign import data Moment :: *
foreign import data MOMENT :: !

data Period = Days | Hours | Minutes | Seconds

instance showPeriod :: Show Period where
  show Days    = "days"
  show Hours   = "hours"
  show Minutes = "minutes"
  show Seconds = "seconds"

instance arbitraryPeriod :: Arbitrary Period where
  arbitrary = do
    i <- chooseInt 1 4
    pure $ case i of
      1 -> Days
      2 -> Hours
      3 -> Minutes
      4 -> Seconds
      _ -> Seconds

instance showMoment :: Show Moment where
  show moment = show' moment

diff :: Moment -> Moment -> Period -> Number
diff start end period = diff' start end (show period)

foreign import create :: Number -> forall e. Eff ( moment :: MOMENT | e ) Moment
foreign import now    :: forall e. Eff ( moment :: MOMENT | e ) Moment
foreign import diff'  :: Moment -> Moment -> String -> Number
foreign import show'  :: Moment -> String
