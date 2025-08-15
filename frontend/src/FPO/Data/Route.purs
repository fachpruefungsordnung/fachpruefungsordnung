-- | This module defines the routing for the application.

module FPO.Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromMaybe)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.GroupDto (GroupID)
import Routing.Duplex (RouteDuplex', boolean, int, optional, root, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

-- | Represents all available routes in the application.
data Route
  = Home
  | Editor { docID :: DocumentID }
  | Login
  | PasswordReset
  | AdminViewUsers
  | AdminViewGroups
  | ViewGroupDocuments { groupID :: GroupID }
  | ViewGroupMembers { groupID :: GroupID }
  | GroupAddMembers { groupID :: GroupID }
  | Page404
  | Profile { loginSuccessful :: Maybe Boolean, userId :: Maybe String }

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

-- | The codec for the routes. It defines how to parse and serialize the routes.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Editor": "editor" ? { docID: int }
  , "Login": "login" / noArgs
  , "PasswordReset": "password-reset" / noArgs
  , "AdminViewUsers": "admin-users" / noArgs
  , "AdminViewGroups": "admin-groups" / noArgs
  , "ViewGroupDocuments": "view-group-documents" ? { groupID: int }
  , "ViewGroupMembers": "view-group-members" ? { groupID: int }
  , "GroupAddMembers": "group-add-members" ? { groupID: int }
  , "Page404": "404" / noArgs
  , "Profile": "profile" ?
      { loginSuccessful: optional <<< boolean, userId: optional <<< string }
  }

-- | Converts a route to a string representation.
-- | This is useful for displaying the route in the UI or for debugging purposes.
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Editor docID -> "Editor:" <> show docID
  Login -> "Login"
  PasswordReset -> "PasswordReset"
  AdminViewUsers -> "AdminViewUsers"
  AdminViewGroups -> "AdminViewGroups"
  ViewGroupDocuments groupID -> "ViewGroupDocuments:" <> show groupID
  ViewGroupMembers groupID -> "ViewGroupMembers:" <> show groupID
  GroupAddMembers groupID -> "GroupAddMembers:" <> show groupID
  Page404 -> "Page404"
  Profile { loginSuccessful } -> "Profile" <>
    ( if loginSuccessful == Nothing then ""
      else " (loginSuccessful: " <> (show $ fromMaybe false loginSuccessful) <> ")"
    )
