module Data.Map.Utils
    ( insert'
    )
where

import Data.Map (Map, insertLookupWithKey)
import Data.Maybe (isJust)
import Prelude hiding (map)

-- | Insert iff the key is not already defined.
insert' :: (Ord k) => k -> v -> Map k v -> Maybe (Map k v)
insert' k v' map =
    let (mV, map') = insertLookupWithKey upd k v' map
     in if isJust mV
            then Nothing
            else Just map'
  where
    -- Note: `upd = undefined` should also work; `upd` should never be
    --   evaluated because of lazyness, but we do not want to trust that.
    upd _ oldV _ = oldV
