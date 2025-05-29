-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.

module FPO.Page.Home (component) where

import Prelude

import Components.Preview (Output, Query(TellClickedHttpRequest, TellLoadPdf, TellLoadUploadedPdf, TellShowOrHideWarning), preview) as Preview
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Editor as Editor
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(..))

data Action
  = HandleEditor Editor.Output
  | HandlePreview Preview.Output

type State =
  { editorContent :: Maybe (Array String) }

type Slots =
  ( editor :: H.Slot Editor.Query Editor.Output Unit
  , preview :: H.Slot Preview.Query Preview.Output Unit
  )

_editor = Proxy :: Proxy "editor"
_preview = Proxy :: Proxy "preview"

component
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> State
  initialState _ = { editorContent: Nothing }

  render :: State -> H.ComponentHTML Action Slots m
  render { editorContent } =
    HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.p0, HB.overflowHidden ] ]
      [
        -- the _button is for the Proxy to be able to identify it via a term
        -- the 0 is the unique identifier
        -- and button { label: "Click Me" } is the component with its input (communication from parent to child)
        -- in the HH.slot function there is a way to handle messages from the child to the parent (now the HandleButton parameter)
        HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.containerFluid, HB.p0, HB.flexFill, HB.overflowHidden ] ]
          [ HH.div [ HP.classes [ HB.dFlex, HB.flexGrow1, HB.flexRow, HB.g0, HB.overflowHidden ] ]
              [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1, HB.col6 ] ]
                  [ HH.slot _editor unit Editor.editor { editorText: "" } HandleEditor ]
              , HH.slot _preview unit Preview.preview { editorContent } HandlePreview
              ]
          ]
      ]

  -- output is when our component communicates with a parent
  -- m is relevant when the component performs effects
  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    HandleEditor output -> case output of
      --Editor.ClickedHTTPRequest -> H.tell _preview unit Preview.TellClickedHttpRequest
      Editor.ClickedQuery response -> H.modify_ \st -> st { editorContent = response }
      --Editor.ClickedShowWarning -> H.tell _preview unit Preview.TellShowOrHideWarning
      --Editor.LoadPdf -> H.tell _preview unit Preview.TellLoadPdf
      Editor.SendPDF mURL -> H.tell _preview unit (Preview.TellLoadUploadedPdf mURL)

    HandlePreview _ -> pure unit
