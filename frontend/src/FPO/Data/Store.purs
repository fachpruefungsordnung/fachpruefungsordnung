-- | This module defines the Store type and the actions that can be performed on it.
-- | Using the Store type, we can manage the state of the application and store various
-- | pieces of information that need to be accessed across different components (/globally).

module FPO.Data.Store
  ( Action(..)
  , Store
  , addError
  , loadLanguage
  , preventErrorHandlingLocally
  , reduce
  , saveLanguage
  ) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)
import FPO.Data.AppError (AppError(..))
import FPO.Data.AppToast (AppToast(..), AppToastWithId)
import FPO.Data.Route (Route)
import FPO.Translations.Translator (FPOTranslator)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem) as LocalStorage

-- | The Store type represents the global state of the application.
type Store =
  { inputMail :: String -- ^ The email that was input in the login form (example state variable)
  , loginRedirect :: Maybe Route -- ^ The route to redirect to after login
  , currentRoute :: Maybe Route
  , translator :: FPOTranslator
  , language :: String
  , toasts :: Array AppToastWithId
  , totalToasts :: Int
  , handleRequestError :: Boolean
  , errorCooldowns ::
      Map String Instant -- ^ Track when errors were last shown to prevent spam
  }

data Action
  = SetMail String -- ^ Action to set the user's email.
  | SetLoginRedirect (Maybe Route) -- ^ Action to set the redirect route after login.
  | SetCurrentRoute (Maybe Route) -- ^ Action to set the current route.
  | SetLanguage String
  | SetTranslator FPOTranslator
  | AddError AppError
  | AddErrorWithCooldown AppError Instant -- ^ Add error with timestamp for cooldown tracking
  | AddWarning String
  | AddSuccess String
  | AddInfo String
  | SetToasts (Array AppToastWithId)
  | SetHandleRequestError Boolean

-- | Temporarily disables error handling in the store while executing the given action.
-- | This can be used to prevent automatic navigation as well as emission of error toasts
-- | on errors for specific operations.
preventErrorHandlingLocally
  :: forall a m
   . MonadStore Action Store m
  => m a
  -> m a
preventErrorHandlingLocally action = do
  store <- getStore
  let originalSetting = store.handleRequestError
  updateStore $ SetHandleRequestError false
  result <- action
  updateStore $ SetHandleRequestError originalSetting
  pure result

-- | Add an error with cooldown checking to prevent spam
addError
  :: forall m
   . MonadStore Action Store m
  => MonadEffect m
  => AppError
  -> m Unit
addError error = do
  currentTime <- liftEffect now
  store <- getStore
  if isErrorOnCooldown error currentTime store.errorCooldowns then pure unit
  else updateStore $ AddErrorWithCooldown error currentTime

-- | Check if an error is on cooldown (1 second for auth errors, 500ms for others)
isErrorOnCooldown :: AppError -> Instant -> Map String Instant -> Boolean
isErrorOnCooldown error currentTime cooldowns =
  case Map.lookup (show error) cooldowns of
    Nothing -> false
    Just lastShown ->
      let
        cooldownMs = case error of
          AuthError _ -> 1000.0 -- 1 second for auth errors
          _ -> 500.0 -- 500ms for other errors
        -- Simple time difference calculation in milliseconds
        Milliseconds currentMs = unInstant currentTime
        Milliseconds lastMs = unInstant lastShown
        timeDiffMs = currentMs - lastMs
      in
        timeDiffMs < cooldownMs

-- | Update the store based on the action.
reduce :: Store -> Action -> Store
reduce store = case _ of
  SetMail m -> store { inputMail = m }
  -- TODO: Using `Store.loginRedirect`, we can specify the route to redirect to after login.
  --       While sufficent for the case of logins and login redirect handling, this is not
  --       flexible (i.e., usable for pages and actions other than login).
  --       A more scalable approach would be to encode the redirect route in the login's query
  --       parameters, but the Route type must not be cyclic, so we cannot use the Route type
  --       directly.
  --       We could also rename this action to `SetRedirect` and use it for all redirects,
  --       but this way, `NavigateQ` cannot take care of clearing the redirect route
  --       when navigating to a different page (in general) and care must be taken regarding
  --       (re)setting the redirect route.
  SetLoginRedirect r -> store { loginRedirect = r }
  SetCurrentRoute r -> store { currentRoute = r }
  SetLanguage s -> store { language = s }
  SetTranslator t -> store { translator = t }
  AddSuccess msg ->
    store
      { toasts = store.toasts <> [ { id: store.totalToasts + 1, toast: Success msg } ]
      , totalToasts = store.totalToasts + 1
      }
  AddWarning msg ->
    store
      { toasts = store.toasts <> [ { id: store.totalToasts + 1, toast: Warning msg } ]
      , totalToasts = store.totalToasts + 1
      }
  AddInfo msg ->
    store
      { toasts = store.toasts <> [ { id: store.totalToasts + 1, toast: Info msg } ]
      , totalToasts = store.totalToasts + 1
      }
  AddError error -> store
    { toasts = store.toasts <> [ { id: store.totalToasts + 1, toast: Error error } ]
    , totalToasts = store.totalToasts + 1
    }
  AddErrorWithCooldown error currentTime ->
    if isErrorOnCooldown error currentTime store.errorCooldowns then store -- Don't show toast if error is on cooldown
    else store
      { toasts = store.toasts <> [ { id: store.totalToasts + 1, toast: Error error } ]
      , totalToasts = store.totalToasts + 1
      , errorCooldowns = Map.insert (show error) currentTime store.errorCooldowns
      }
  SetToasts toasts -> store { toasts = toasts }
  SetHandleRequestError b -> store { handleRequestError = b }

saveLanguage :: String -> Effect Unit
saveLanguage lang = do
  win <- window
  storage <- localStorage win
  LocalStorage.setItem "userLanguage" lang storage

loadLanguage :: Effect (Maybe String)
loadLanguage = do
  win <- window
  storage <- localStorage win
  LocalStorage.getItem "userLanguage" storage
