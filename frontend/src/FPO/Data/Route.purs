-- | This module defines the routing for the application.

module FPO.Data.Route where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.GroupDto (GroupID)
import Routing.Duplex
  ( RouteDuplex'
  , int
  , optional
  , parse
  , root
  , segment
  , string
  )
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

-- | Sub-routes for group management under /administration/groups/:groupID
data GroupSubRoute
  = GroupDocuments
  | GroupMembers

derive instance genericGroupSubRoute :: Generic GroupSubRoute _
derive instance eqGroupSubRoute :: Eq GroupSubRoute
derive instance ordGroupSubRoute :: Ord GroupSubRoute

-- | Represents all available routes in the application.
data Route
  = AdminGroups
  | Administration
  | AdminUsers
  | CreateGroup
  | CreateUser
  | Editor DocumentID
  | GroupRoute GroupID GroupSubRoute
  | Home
  | Login
  | Page404
  | PasswordReset { token :: Maybe String }
  | Profile
  | UserProfile String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where
  show = routeToString

-- | Codec for group sub-routes
groupSubRouteCodec :: RouteDuplex' GroupSubRoute
groupSubRouteCodec = sum
  { "GroupDocuments": noArgs
  , "GroupMembers": "members" / noArgs
  }

-- | The codec for the routes. It defines how to parse and serialize the routes.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "AdminGroups": "administration" / "groups" / noArgs
  , "Administration": "administration" / noArgs
  , "AdminUsers": "administration" / "users" / noArgs
  , "CreateGroup": "administration" / "groups" / "new" / noArgs
  , "CreateUser": "administration" / "users" / "new" / noArgs
  , "Editor": "editor" / int segment
  , "GroupRoute": "administration" / "groups" / int segment / groupSubRouteCodec
  , "Home": noArgs
  , "Login": "login" / noArgs
  , "Page404": "404" / noArgs
  , "PasswordReset": "reset-password" ? { token: optional <<< string }
  , "Profile": "me" / noArgs
  , "UserProfile": "profile" / string segment
  }

-- | Converts a route to a string representation.
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Editor docID -> "Editor:" <> show docID
  Login -> "Login"
  PasswordReset { token } -> "PasswordReset:" <> show token
  Administration -> "Administration"
  AdminUsers -> "AdminUsers"
  CreateUser -> "CreateUser"
  AdminGroups -> "AdminGroups"
  CreateGroup -> "CreateGroup"
  GroupRoute groupID subRoute -> "GroupRoute:" <> show groupID <> " " <> showSubRoute
    subRoute
  Page404 -> "Page404"
  Profile -> "Profile"
  UserProfile userId -> "UserProfile:" <> userId

showSubRoute :: GroupSubRoute -> String
showSubRoute = case _ of
  GroupDocuments -> "GroupDocuments"
  GroupMembers -> "GroupMembers"

urlToRoute :: String -> Maybe Route
urlToRoute url = case parse routeCodec url of
  Left _ -> Nothing
  Right route -> Just route

-- | Helper: is this an administration sub-route?
isAdminRoute :: Route -> Boolean
isAdminRoute = case _ of
  Administration -> true
  AdminUsers -> true
  CreateUser -> true
  AdminGroups -> true
  CreateGroup -> true
  GroupRoute _ _ -> true
  _ -> false
