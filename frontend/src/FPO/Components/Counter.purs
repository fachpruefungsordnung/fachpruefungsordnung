module FPO.Components.Counter
  ( counter
  , Query(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

data Query a
  = Increment a
  | GetCount (Int -> a)

type State = { count :: Int }

counter :: forall input output m. H.Component Query input output m
counter =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery }
    }
  where
  render :: forall action. State -> H.ComponentHTML action () m
  render { count } =
    HH.div_ [ HH.text $ show count ]

  handleQuery
    :: forall action a. Query a -> H.HalogenM State action () output m (Maybe a)
  handleQuery = case _ of
    Increment a -> do
      H.modify_ \state -> state { count = state.count + 1 }
      pure $ Just a

    GetCount reply -> do
      -- ... do something, then provide the requested `Boolean` to the `reply`
      -- function to produce the `a` we need to return
      { count } <- H.get
      pure $ Just $ reply count
