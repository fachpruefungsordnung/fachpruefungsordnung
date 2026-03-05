-- | Editor page of the application. This page is used to edit
-- | the FPO document. It contains a split view with the editor
-- | on the left and a preview on the right. Heart of the
-- | application.

module FPO.Page.EditorPage (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Splitview as Splitview
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import FPO.UI.Css as HB
import Type.Proxy (Proxy(..))

-- | The input now carries both the document ID and optional query parameters
-- | parsed from the URL so that the editor can restore paragraph, revision,
-- | and split-view state from a shared link.
type Input =
  { docID :: DocumentID
  , params ::
      { revision :: Maybe Int
      , paragraph :: Maybe Int
      , splitview :: Maybe String
      }
  }

data Action
  = HandleSplitview Splitview.Output
  | Receive Input

type State =
  { docID :: DocumentID
  , params ::
      { revision :: Maybe Int
      , paragraph :: Maybe Int
      , splitview :: Maybe String
      }
  }

type Slots =
  ( splitview :: H.Slot Splitview.Query Splitview.Output Unit
  )

_splitview = Proxy :: Proxy "splitview"

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState: \input -> { docID: input.docID, params: input.params }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div [ HP.classes [ HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
      [ HH.slot _splitview unit Splitview.splitview
          { docID: state.docID, params: state.params }
          HandleSplitview
      ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    HandleSplitview _ -> pure unit
    Receive input -> do
      H.modify_ _ { docID = input.docID, params = input.params }
