-- | This module defines the routing for the application.

module FPO.Data.Route where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.GroupDto (GroupID)
import Routing.Duplex (RouteDuplex', boolean, int, optional, params, parse, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

-- | Sub-routes for group management under /administration/groups/:groupID
data GroupSubRoute
  = GroupOverview { tab :: Maybe String }
  | AddMember

derive instance genericGroupSubRoute :: Generic GroupSubRoute _
derive instance eqGroupSubRoute :: Eq GroupSubRoute
derive instance ordGroupSubRoute :: Ord GroupSubRoute

-- | Represents all available routes in the application.
data Route
  = Home
  | Editor { docID :: DocumentID }
  | Login
  | PasswordReset { token :: Maybe String }
  | Administration { tab :: Maybe String }
  | CreateUser
  | CreateGroup
  | GroupRoute GroupID GroupSubRoute
  | Page404
  | Profile { loginSuccessful :: Maybe Boolean, userId :: Maybe String }

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where
  show = routeToString

-- | Codec for group sub-routes
groupSubRouteCodec :: RouteDuplex' GroupSubRoute
groupSubRouteCodec = sum
  { "GroupOverview": params { tab: optional <<< string }
  , "AddMember": "members" / "add" / noArgs
  }

-- | The codec for the routes. It defines how to parse and serialize the routes.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Editor": "editor" ? { docID: int }
  , "Login": "login" / noArgs
  , "PasswordReset": "reset-password" ? { token: optional <<< string }
  , "Administration": "administration" ? { tab: optional <<< string }
  , "CreateUser": "administration" / "create-user" / noArgs
  , "CreateGroup": "administration" / "create-group" / noArgs
  , "GroupRoute": "administration" / "groups" / int segment / groupSubRouteCodec
  , "Page404": "404" / noArgs
  , "Profile": "profile" ?
      { loginSuccessful: optional <<< boolean, userId: optional <<< string }
  }

-- | Converts a route to a string representation.
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Editor docID -> "Editor:" <> show docID
  Login -> "Login"
  PasswordReset { token } -> "PasswordReset:" <> (show token)
  Administration { tab } -> "Administration:" <> show tab
  CreateUser -> "CreateUser"
  CreateGroup -> "CreateGroup"
  GroupRoute groupID subRoute -> "GroupRoute:" <> show groupID <> " " <> showSubRoute subRoute
  Page404 -> "Page404"
  Profile { loginSuccessful } -> "Profile" <>
    ( if loginSuccessful == Nothing then ""
      else " (loginSuccessful: " <> (show $ fromMaybe false loginSuccessful) <> ")"
    )

showSubRoute :: GroupSubRoute -> String
showSubRoute = case _ of
  GroupOverview { tab } -> "GroupOverview tab:" <> show tab
  AddMember -> "AddMember"

urlToRoute :: String -> Maybe Route
urlToRoute url = case parse routeCodec url of
  Left _ -> Nothing
  Right route -> Just route

-- | Helper to create a GroupOverview route
groupOverview :: GroupID -> Maybe String -> Route
groupOverview gid tab = GroupRoute gid (GroupOverview { tab })

-- | Helper to create an AddMember route
addGroupMember :: GroupID -> Route
addGroupMember gid = GroupRoute gid AddMember
