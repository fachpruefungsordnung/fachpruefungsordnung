-- | This module defines the routing for the application.

module FPO.Data.Route where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.GroupDto (GroupID)
import Routing.Duplex
  ( RouteDuplex'
  , int
  , optional
  , parse
  , print
  , root
  , segment
  , string
  )
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window

-- | Sub-routes for group management under /administration/groups/:groupID
data GroupSubRoute
  = GroupDocuments
  | GroupMembers
  | GroupSettings

derive instance genericGroupSubRoute :: Generic GroupSubRoute _
derive instance eqGroupSubRoute :: Eq GroupSubRoute
derive instance ordGroupSubRoute :: Ord GroupSubRoute

-- | Query parameters for the editor route, controlling which paragraph,
-- | revision, and view mode are active.
type EditorParams =
  { revision :: Maybe Int
  , paragraph :: Maybe Int
  , splitview :: Maybe String
  }

-- | Default (empty) editor params — no paragraph selected, no revision pinned,
-- | no split view.
defaultEditorParams :: EditorParams
defaultEditorParams =
  { revision: Nothing
  , paragraph: Nothing
  , splitview: Nothing
  }

-- | Query parameters for the login route, optionally carrying a redirect URI
-- | so that the user is returned to the page they were on before being logged out.
type LoginParams =
  { redirect :: Maybe String
  }

-- | Represents all available routes in the application.
data Route
  = AdminGroups
  | AdminUsers
  | CreateGroup
  | CreateUser
  | Editor DocumentID EditorParams
  | GroupRoute GroupID GroupSubRoute
  | Home
  | Login LoginParams
  | Page404
  | PasswordReset { token :: Maybe String }
  | Profile
  | Unauthorized
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
  , "GroupSettings": "settings" / noArgs
  }

-- | The codec for the routes. It defines how to parse and serialize the routes.
-- |
-- | The `Editor` route uses a path segment for the document ID and query
-- | parameters for `revision`, `paragraph`, and `splitview`:
-- |   /documents/:id?revision=...&paragraph=...&splitview=...
-- |
-- | The `Login` route uses an optional `redirect` query parameter that stores
-- | the URI the user should be sent to after a successful login:
-- |   /login?redirect=...
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "AdminGroups": "administration" / "groups" / noArgs
  , "AdminUsers": "administration" / "users" / noArgs
  , "CreateGroup": "administration" / "groups" / "new" / noArgs
  , "CreateUser": "administration" / "users" / "new" / noArgs
  , "Editor": "documents" / int segment ?
      { revision: optional <<< int
      , paragraph: optional <<< int
      , splitview: optional <<< string
      }
  , "GroupRoute": "administration" / "groups" / int segment / groupSubRouteCodec
  , "Home": noArgs
  , "Login": "login" ? { redirect: optional <<< string }
  , "Page404": "404" / noArgs
  , "PasswordReset": "reset-password" ? { token: optional <<< string }
  , "Profile": "me" / noArgs
  , "Unauthorized": "unauthorized" / noArgs
  , "UserProfile": "profile" / string segment
  }

-- | Converts a route to a string representation (for debugging / Show).
routeToString :: Route -> String
routeToString = case _ of
  Home -> "Home"
  Editor docID params -> "Editor:" <> show docID <> showEditorParams params
  Login { redirect } -> "Login" <> case redirect of
    Nothing -> ""
    Just uri -> "?redirect=" <> uri
  PasswordReset { token } -> "PasswordReset:" <> show token
  AdminUsers -> "AdminUsers"
  CreateUser -> "CreateUser"
  AdminGroups -> "AdminGroups"
  CreateGroup -> "CreateGroup"
  GroupRoute groupID subRoute -> "GroupRoute:" <> show groupID <> " " <> showSubRoute
    subRoute
  Page404 -> "Page404"
  Profile -> "Profile"
  Unauthorized -> "Unauthorized"
  UserProfile userId -> "UserProfile:" <> userId

showEditorParams :: EditorParams -> String
showEditorParams p =
  let
    parts =
      ( case p.paragraph of
          Nothing -> ""
          Just para -> " paragraph=" <> show para
      )
        <>
          ( case p.revision of
              Nothing -> ""
              Just rev -> " revision=" <> show rev
          )
        <>
          ( case p.splitview of
              Nothing -> ""
              Just sv -> " splitview=" <> sv
          )
  in
    parts

showSubRoute :: GroupSubRoute -> String
showSubRoute = case _ of
  GroupDocuments -> "GroupDocuments"
  GroupMembers -> "GroupMembers"
  GroupSettings -> "GroupSettings"

urlToRoute :: String -> Maybe Route
urlToRoute url = case parse routeCodec url of
  Left _ -> Nothing
  Right route -> Just route

-- | Parse a URL to a Route, falling back to Page404 for unknown paths.
-- | Unlike `urlToRoute`, this always returns a Route and never fails.
parseRoute :: String -> Route
parseRoute url = case parse routeCodec url of
  Left _ -> Page404
  Right route -> route

-- | Read the current browser path (pathname + search) as an `Effect String`.
-- | This is the path-based equivalent of the old `Routing.Hash.getHash`.
currentPath :: Effect String
currentPath = do
  loc <- window >>= Window.location
  p <- Location.pathname loc
  s <- Location.search loc
  pure (p <> s)

-- | Print a route as a URI string (the path portion of the URL).
-- | Useful for encoding routes as redirect parameters, etc.
routeToUri :: Route -> String
routeToUri = print routeCodec

-- | Helper: is this an administration sub-route?
isAdminRoute :: Route -> Boolean
isAdminRoute = case _ of
  AdminUsers -> true
  CreateUser -> true
  AdminGroups -> true
  CreateGroup -> true
  GroupRoute _ _ -> true
  _ -> false

isUnauthorizedRoute :: Route -> Boolean
isUnauthorizedRoute = case _ of
  Unauthorized -> true
  _ -> false

-- | Helper: is this a Login route (regardless of params)?
isLoginRoute :: Route -> Boolean
isLoginRoute = case _ of
  Login _ -> true
  _ -> false

-- | Convenience constructor for the Editor route with default (empty) params.
editorRoute :: DocumentID -> Route
editorRoute docId = Editor docId defaultEditorParams

-- | Convenience constructor for the Login route without a redirect.
loginRoute :: Route
loginRoute = Login { redirect: Nothing }

-- | Convenience constructor for the Login route with a redirect to the given route.
loginRouteWithRedirect :: Route -> Route
loginRouteWithRedirect from = Login
  { redirect: if from /= Home then Just $ routeToUri from else Nothing }
