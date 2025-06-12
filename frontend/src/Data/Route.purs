-- | This module defines the routing for the application.

module FPO.Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Routing.Duplex (RouteDuplex', boolean, optional, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

-- | Represents all available routes in the application.
data Route
  = Home
  | Editor
  | Login
  | PasswordReset
  | AdminPanel
  | Page404
  | Profile { loginSuccessful :: Maybe Boolean }

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

-- | The codec for the routes. It defines how to parse and serialize the routes.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Editor": "editor" / noArgs
  , "Login": "login" / noArgs
  , "PasswordReset": "password-reset" / noArgs
  , "AdminPanel": "admin-panel" / noArgs
  , "Page404": "404" / noArgs
  , "Profile": "profile" ? { loginSuccessful: optional <<< boolean }
  }

-- | Converts a route to a string representation.
-- | This is useful for displaying the route in the UI or for debugging purposes.
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Editor -> "Editor"
  Login -> "Login"
  PasswordReset -> "PasswordReset"
  AdminPanel -> "AdminPanel"
  Page404 -> "Page404"
  Profile { loginSuccessful } -> "Profile" <>
    ( if loginSuccessful == Nothing then ""
      else " (loginSuccessful: " <> (show $ fromMaybe false loginSuccessful) <> ")"
    )
