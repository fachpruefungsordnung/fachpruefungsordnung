-- | Editor page of the application. This page is used to edit
-- | the FPO document. It contains a split view with the editor
-- | on the left and a preview on the right. Heart of the
-- | application.

module FPO.Page.EditorPage (component) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import FPO.Component.Splitview as Splitview
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(..))

data Action = HandleSplitview Splitview.Output

type State = {}

type Slots =
  ( splitview :: H.Slot Splitview.Query Splitview.Output Unit
  )

_splitview = Proxy :: Proxy "splitview"

component
  :: forall query input output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => DocumentID
  -> H.Component query input output m
component docID =
  H.mkComponent
    { initialState: const {}
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.div [ HP.classes [ HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
      [ HH.slot _splitview unit (Splitview.splitview docID) unit HandleSplitview
      ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    HandleSplitview _ -> pure unit
