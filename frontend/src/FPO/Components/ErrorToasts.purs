module FPO.Components.ErrorToasts
  ( ErrorAction(..)
  , Input
  , Output
  , component
  ) where

import Prelude

import Data.Array (length, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppError (AppError)
import FPO.Data.Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)

type State = Array AppError

type Input = Array AppError

type Output = Void

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \input -> input
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleNewErrors
        }
    }

data ErrorAction
  = HandleNewErrors (Array AppError)
  | RemoveToast Int
  | AutoRemoveToast Int

handleAction
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ErrorAction
  -> H.HalogenM State ErrorAction () Output m Unit
handleAction = case _ of
  HandleNewErrors errors -> do
    H.put errors
    -- Auto-remove toasts after 5 seconds
    void $ H.fork do
      H.liftAff $ delay (Milliseconds 5000.0)
      when (length errors > 0) do
        handleAction $ AutoRemoveToast 0

  RemoveToast index -> do
    updateStore $ Store.RemoveError index

  AutoRemoveToast index -> do
    updateStore $ Store.RemoveError index

render :: forall m. Array AppError -> H.ComponentHTML ErrorAction () m
render errors = do
  HH.div
    [ HP.class_ (HH.ClassName "fpo-toast-container")
    ]
    [ HH.div_ (mapWithIndex renderToast errors)
    ]

renderToast :: forall m. Int -> AppError -> H.ComponentHTML ErrorAction () m
renderToast index error =
  HH.div
    [ HP.classes
        [ HH.ClassName "fpo-toast"
        , HH.ClassName "fpo-toast-error"
        , HH.ClassName "animate-slide-in"
        ]
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "fpo-toast-content")
        ]
        [ HH.span
            [ HP.class_ (HH.ClassName "fpo-toast-message")
            ]
            [ HH.text (show error) ]
        , HH.button
            [ HP.class_ (HH.ClassName "fpo-toast-close")
            , HE.onClick \_ -> RemoveToast index
            ]
            [ HH.text "x" ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "fpo-toast-progress")
        ]
        [ HH.div
            [ HP.class_ (HH.ClassName "fpo-toast-progress-bar")
            , HP.attr (HH.AttrName "data-toast-id") (show index)
            ]
            []
        ]
    ]
