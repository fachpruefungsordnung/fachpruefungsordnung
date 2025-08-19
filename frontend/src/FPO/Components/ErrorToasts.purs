module FPO.Components.ErrorToasts
  ( ErrorAction
  , Input
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
import FPO.Data.Store (AppErrorWithId, ErrorId)
import FPO.Data.Store as Store
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)

type State =
  { errors :: Array AppErrorWithId
  , totalToasts :: Int
  }

type Input = Array AppErrorWithId

type Output = Void

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleNewErrors
        }
    }

data ErrorAction
  = HandleNewErrors (Array AppErrorWithId)
  | RemoveToast ErrorId

handleAction
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ErrorAction
  -> H.HalogenM State ErrorAction () Output m Unit
handleAction = case _ of
  HandleNewErrors newErrors -> do
    state <- H.get
    let previouslyUnknownErrors = getNewErrors state.errors newErrors
    H.put { errors: newErrors, totalToasts: length previouslyUnknownErrors }

    traverse_
      ( \error -> do
          let errorId = error.errorId
          -- Trigger the auto-remove action for each new error
          H.fork $ do
            H.liftAff $ delay (Milliseconds 5500.0)
            handleAction $ RemoveToast errorId
            pure unit
      )
      previouslyUnknownErrors

  RemoveToast id -> do
    state <- H.get
    let updatedErrors = removeErrorWithId id state.errors
    H.put state { errors = updatedErrors }
    updateStore $ Store.SetErrors updatedErrors

initialState :: Input -> State
initialState errors = { errors: errors, totalToasts: length errors }

render :: forall m. State -> H.ComponentHTML ErrorAction () m
render state = do
  HH.div
    [ HP.class_ (HH.ClassName "fpo-toast-container")
    ]
    [ HH.div_ (map renderToast state.errors)
    ]

renderToast :: forall m. AppErrorWithId -> H.ComponentHTML ErrorAction () m
renderToast error =
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
            [ HH.text (show error.error) ]
        , HH.button
            [ HP.class_ (HH.ClassName "fpo-toast-close")
            , HE.onClick \_ -> RemoveToast error.errorId
            ]
            [ HH.text "x" ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "fpo-toast-progress")
        ]
        [ HH.div
            [ HP.class_ (HH.ClassName "fpo-toast-progress-bar")
            , HP.attr (HH.AttrName "data-toast-id") (show error.errorId)
            ]
            []
        ]
    ]

doesNotContainElement :: Array AppErrorWithId -> AppErrorWithId -> Boolean
doesNotContainElement errors element = not $ any
  (\error -> error.errorId == element.errorId)
  errors

getNewErrors :: Array AppErrorWithId -> Array AppErrorWithId -> Array AppErrorWithId
getNewErrors oldErrors newErrors' = filter (doesNotContainElement oldErrors)
  newErrors'

removeErrorWithId :: ErrorId -> Array AppErrorWithId -> Array AppErrorWithId
removeErrorWithId id errors = filter (\error -> error.errorId /= id) errors
