module Lenses where

import Data.Lens (view, traversed, _1, _2, _Just, lens)
import Data.Lens.Types (LensP)
import Data.Lens.Zoom (Traversal, Lens, zoom)
import Data.Lens.Index
import Data.Lens.Fold
import Data.Lens.Internal.Wander (wander)
import Data.Lens.Traversal

-- Common lenses used in all of the queries

_values :: forall a b r. Lens { values :: a | r } { values :: b | r } a b
_values = lens _.values (_ { values = _ })

_series :: forall a b r. Lens { series :: a | r } { series :: b | r } a b
_series = lens _.series (_ { series = _ })

_results :: forall a b r. Lens { results :: a | r } { results :: b | r } a b
_results = lens _.results (_ { results = _ })
