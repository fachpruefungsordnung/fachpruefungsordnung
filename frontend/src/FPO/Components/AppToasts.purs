module FPO.Components.AppToasts
  ( Input
  , Output
  , component
  ) where

import Prelude

import Data.Array (any, filter, length)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppToast (AppToast(..), AppToastWithId, ToastId)
import FPO.Data.Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (Selector, selectEq)
import Halogen.Themes.Bootstrap5 as HB

type State =
  { toasts :: Array AppToastWithId
  , totalToasts :: Int
  }

type Input = Unit

type Output = Void

selectAppErrors :: Selector Store.Store (Array AppToastWithId)
selectAppErrors = selectEq _.toasts

-- Helper functions
getBootstrapToastClass :: AppToast -> HH.ClassName
getBootstrapToastClass toast = case toast of
  Error _ -> HB.textBgDanger
  Success _ -> HB.textBgSuccess
  Warning _ -> HB.textBgWarning
  Info _ -> HB.textBgInfo

getProgressBarClass :: AppToast -> HH.ClassName
getProgressBarClass toast = case toast of
  Error _ -> HB.bgDanger
  Success _ -> HB.bgSuccess
  Warning _ -> HB.bgWarning
  Info _ -> HB.bgInfo

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
component =
  connect selectAppErrors $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleNewToasts
        }
    }

data ToastAction
  = HandleNewToasts (Connected (Array AppToastWithId) Input)
  | RemoveToast ToastId

handleAction
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ToastAction
  -> H.HalogenM State ToastAction () Output m Unit
handleAction = case _ of
  HandleNewToasts { context: newToasts } -> do
    state <- H.get
    let previouslyUnknownToasts = getNewToasts state.toasts newToasts
    H.put { toasts: newToasts, totalToasts: length previouslyUnknownToasts }

    traverse_
      ( \toast -> do
          let toastId = toast.id
          -- Trigger the auto-remove action for each new toast
          H.fork $ do
            H.liftAff $ delay (Milliseconds 5500.0)
            handleAction $ RemoveToast toastId
            pure unit
      )
      previouslyUnknownToasts

  RemoveToast id -> do
    state <- H.get
    let updatedToasts = removeToastWithId id state.toasts
    H.put state { toasts = updatedToasts }
    updateStore $ Store.SetToasts updatedToasts

initialState :: Connected (Array AppToastWithId) Input -> State
initialState { context } = { toasts: context, totalToasts: length context }

render :: forall m. State -> H.ComponentHTML ToastAction () m
render state = do
  HH.div
    [ HP.classes
        [ HB.toastContainer
        , HB.positionFixed
        , HB.end0
        , HB.p3
        ]
    , HP.style "top: 80px; z-index: 9999;"
    ]
    [ HH.div_ (map renderToast state.toasts)
    ]

renderToast :: forall m. AppToastWithId -> H.ComponentHTML ToastAction () m
renderToast toast =
  HH.div
    [ HP.classes
        [ HB.toast
        , HB.show
        , HB.mb2
        , HB.shadow
        , HB.positionRelative
        , getBootstrapToastClass toast.toast
        , HH.ClassName "animate-slide-in"
        ]
    , HP.attr (HH.AttrName "role") "alert"
    , HP.attr (HH.AttrName "aria-live") "polite"
    , HP.attr (HH.AttrName "aria-atomic") "true"
    , HP.style "overflow: hidden;"
    ]
    [ HH.div
        [ HP.classes
            [ HB.toastBody, HB.dFlex, HB.justifyContentBetween, HB.alignItemsCenter ]
        ]
        [ HH.text (show toast.toast)
        , HH.button
            [ HP.classes
                [ HB.btnClose
                , HB.ms2
                ]
            , HE.onClick \_ -> RemoveToast toast.id
            , HP.attr (HH.AttrName "aria-label") "Close"
            ]
            []
        ]
    , HH.div
        [ HP.classes
            [ HB.progress
            , HB.positionAbsolute
            , HB.bottom0
            , HB.start0
            , HB.end0
            ]
        , HP.style "height: 3px; border-radius: 0;"
        ]
        [ HH.div
            [ HP.classes
                [ HB.progressBar
                , getProgressBarClass toast.toast
                , HH.ClassName "fpo-progress-animate"
                ]
            , HP.attr (HH.AttrName "data-toast-id") (show toast.id)
            ]
            []
        ]
    ]

doesNotContainElement :: Array AppToastWithId -> AppToastWithId -> Boolean
doesNotContainElement toasts element = not $ any
  (\toast -> toast.id == element.id)
  toasts

getNewToasts :: Array AppToastWithId -> Array AppToastWithId -> Array AppToastWithId
getNewToasts oldToasts newToasts' = filter (doesNotContainElement oldToasts)
  newToasts'

removeToastWithId :: ToastId -> Array AppToastWithId -> Array AppToastWithId
removeToastWithId id toasts = filter (\toast -> toast.id /= id) toasts
