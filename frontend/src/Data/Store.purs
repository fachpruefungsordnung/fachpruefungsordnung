-- | This module defines the Store type and the actions that can be performed on it.
-- | Using the Store type, we can manage the state of the application and store various
-- | information such as account information, user data, and other relevant local data.

module FPO.Data.Store where

import Data.Maybe (Maybe)

type User = { userName :: String }

-- | The Store type represents the global state of the application.
type Store =
  { inputMail :: String -- ^ The email that was input in the login form (example state variable)
  , user :: Maybe User -- ^ The user's name
  }

data Action
  = SetMail String -- ^ Action to set the user's email.
  | SetUser (Maybe User) -- ^ Action to set the user's name.

--   Nothing means no user is logged in.

-- | Update the store based on the action.
reduce :: Store -> Action -> Store
reduce store = case _ of
  SetMail m -> store { inputMail = m }
  SetUser u -> store { user = u }