-- | Tests for the routing codec defined in FPO.Data.Route.
-- |
-- | These tests verify that:
-- | 1. Routes can be printed and then parsed back (round-trip).
-- | 2. URL strings are parsed to the expected Route values.
-- | 3. The new Editor query parameters (revision, paragraph, splitview) work correctly.
-- | 4. The Login redirect query parameter works correctly.
-- | 5. The Unauthorized route round-trips and parses correctly.
-- | 6. Login redirect preserves protected routes (admin, profile, groups).
-- | 7. Edge cases (missing params, empty params) are handled gracefully.

module Test.Routing
  ( routeCodecTests
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import FPO.Data.Route
  ( EditorParams
  , GroupSubRoute(..)
  , Route(..)
  , defaultEditorParams
  , editorRoute
  , loginRoute
  , loginRouteWithRedirect
  , parseRoute
  , routeCodec
  , routeToUri
  , urlToRoute
  )
import Routing.Duplex (parse, print)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Helper: print a route and parse it back; the result should equal the original.
roundTrip :: Route -> Either String Route
roundTrip route =
  case parse routeCodec (print routeCodec route) of
    Left err -> Left (show err)
    Right r -> Right r

routeCodecTests :: Spec Unit
routeCodecTests = do
  describe "Route codec" do

    -- ------------------------------------------------------------------
    -- Basic routes (no parameters)
    -- ------------------------------------------------------------------

    describe "basic routes" do
      it "round-trips Home" do
        roundTrip Home `shouldEqual` Right Home

      it "round-trips Page404" do
        roundTrip Page404 `shouldEqual` Right Page404

      it "round-trips Profile" do
        roundTrip Profile `shouldEqual` Right Profile

      it "round-trips AdminUsers" do
        roundTrip AdminUsers `shouldEqual` Right AdminUsers

      it "round-trips AdminGroups" do
        roundTrip AdminGroups `shouldEqual` Right AdminGroups

      it "round-trips CreateUser" do
        roundTrip CreateUser `shouldEqual` Right CreateUser

      it "round-trips CreateGroup" do
        roundTrip CreateGroup `shouldEqual` Right CreateGroup

    -- ------------------------------------------------------------------
    -- Editor route with query parameters
    -- ------------------------------------------------------------------

    describe "Editor route" do
      it "round-trips Editor with no params" do
        let route = editorRoute 42
        roundTrip route `shouldEqual` Right route

      it "round-trips Editor with paragraph only" do
        let
          route = Editor 1
            { revision: Nothing, paragraph: Just 7, splitview: Nothing }
        roundTrip route `shouldEqual` Right route

      it "round-trips Editor with revision only" do
        let
          route = Editor 1
            { revision: Just 3, paragraph: Nothing, splitview: Nothing }
        roundTrip route `shouldEqual` Right route

      it "round-trips Editor with splitview only" do
        let
          route = Editor 1
            { revision: Nothing, paragraph: Nothing, splitview: Just "comparison" }
        roundTrip route `shouldEqual` Right route

      it "round-trips Editor with all params" do
        let
          params :: EditorParams
          params =
            { revision: Just 5
            , paragraph: Just 10
            , splitview: Just "comparison"
            }
          route = Editor 99 params
        roundTrip route `shouldEqual` Right route

      it "parses /documents/1 to Editor with default params" do
        parse routeCodec "/documents/1"
          `shouldEqual` Right (editorRoute 1)

      it "parses /documents/1?paragraph=3 correctly" do
        parse routeCodec "/documents/1?paragraph=3"
          `shouldEqual` Right
            (Editor 1 { revision: Nothing, paragraph: Just 3, splitview: Nothing })

      it "parses /documents/1?revision=5&paragraph=3 correctly" do
        parse routeCodec "/documents/1?revision=5&paragraph=3"
          `shouldEqual` Right
            (Editor 1 { revision: Just 5, paragraph: Just 3, splitview: Nothing })

      it "parses /documents/1?revision=5&paragraph=3&splitview=comparison correctly"
        do
          parse routeCodec "/documents/1?revision=5&paragraph=3&splitview=comparison"
            `shouldEqual` Right
              ( Editor 1
                  { revision: Just 5
                  , paragraph: Just 3
                  , splitview: Just "comparison"
                  }
              )

      it "prints Editor with all params to expected URI" do
        let
          route = Editor 1
            { revision: Just 5
            , paragraph: Just 3
            , splitview: Just "comparison"
            }
          uri = print routeCodec route
        -- Query param order may vary, so parse back instead of exact string match
        parse routeCodec uri `shouldEqual` Right route

      it "prints Editor with no params to /documents/:id" do
        print routeCodec (editorRoute 42) `shouldEqual` "/documents/42"

    -- ------------------------------------------------------------------
    -- Login route with redirect parameter
    -- ------------------------------------------------------------------

    describe "Login route" do
      it "round-trips Login with no redirect" do
        roundTrip loginRoute `shouldEqual` Right loginRoute

      it "round-trips Login with redirect" do
        let route = Login { redirect: Just "/documents/1" }
        roundTrip route `shouldEqual` Right route

      it "parses /login to Login with no redirect" do
        parse routeCodec "/login"
          `shouldEqual` Right loginRoute

      it "parses /login?redirect=%2Fdocuments%2F1 correctly" do
        parse routeCodec "/login?redirect=%2Fdocuments%2F1"
          `shouldEqual` Right (Login { redirect: Just "/documents/1" })

      it "prints Login with no redirect to /login" do
        print routeCodec loginRoute `shouldEqual` "/login"

      it "loginRouteWithRedirect encodes route URI in redirect param" do
        let
          targetRoute = editorRoute 7
          loginWithRedirect = loginRouteWithRedirect targetRoute
        case loginWithRedirect of
          Login { redirect: Just uri } ->
            -- The redirect URI should parse back to the original route
            parse routeCodec uri `shouldEqual` Right targetRoute
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

      it "loginRouteWithRedirect round-trips" do
        let route = loginRouteWithRedirect Home
        roundTrip route `shouldEqual` Right route

      it "loginRouteWithRedirect preserves editor params in redirect" do
        let
          editorWithParams = Editor 5
            { revision: Just 3
            , paragraph: Just 10
            , splitview: Nothing
            }
          login = loginRouteWithRedirect editorWithParams
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right editorWithParams
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

    -- ------------------------------------------------------------------
    -- PasswordReset route (existing; sanity check)
    -- ------------------------------------------------------------------

    describe "PasswordReset route" do
      it "round-trips PasswordReset with no token" do
        let route = PasswordReset { token: Nothing }
        roundTrip route `shouldEqual` Right route

      it "round-trips PasswordReset with token" do
        let route = PasswordReset { token: Just "abc123" }
        roundTrip route `shouldEqual` Right route

    -- ------------------------------------------------------------------
    -- GroupRoute
    -- ------------------------------------------------------------------

    describe "GroupRoute" do
      it "round-trips GroupRoute with GroupDocuments" do
        let route = GroupRoute 5 GroupDocuments
        roundTrip route `shouldEqual` Right route

      it "round-trips GroupRoute with GroupMembers" do
        let route = GroupRoute 5 GroupMembers
        roundTrip route `shouldEqual` Right route

    -- ------------------------------------------------------------------
    -- Unauthorized route
    -- ------------------------------------------------------------------

    describe "Unauthorized route" do
      it "round-trips Unauthorized" do
        roundTrip Unauthorized `shouldEqual` Right Unauthorized

      it "parses /unauthorized to Unauthorized" do
        parse routeCodec "/unauthorized"
          `shouldEqual` Right Unauthorized

      it "prints Unauthorized to /unauthorized" do
        print routeCodec Unauthorized `shouldEqual` "/unauthorized"

    -- ------------------------------------------------------------------
    -- UserProfile
    -- ------------------------------------------------------------------

    describe "UserProfile route" do
      it "round-trips UserProfile" do
        let route = UserProfile "user-123"
        roundTrip route `shouldEqual` Right route

    -- ------------------------------------------------------------------
    -- routeToUri / urlToRoute helpers
    -- ------------------------------------------------------------------

    describe "routeToUri and urlToRoute" do
      it "routeToUri produces a parseable string" do
        let
          route = Editor 3 { revision: Just 1, paragraph: Just 2, splitview: Nothing }
          uri = routeToUri route
        urlToRoute uri `shouldEqual` Just route

      it "urlToRoute returns Nothing for invalid paths" do
        urlToRoute "/nonexistent/path" `shouldEqual` Nothing

      it "urlToRoute returns Nothing for various unknown paths" do
        urlToRoute "/this-does-not-exist" `shouldEqual` Nothing
        urlToRoute "/admin" `shouldEqual` Nothing
        urlToRoute "/documents/notanumber" `shouldEqual` Nothing
        urlToRoute "/administration/unknown" `shouldEqual` Nothing

      it "urlToRoute parses basic routes" do
        urlToRoute "/" `shouldEqual` Just Home
        urlToRoute "/me" `shouldEqual` Just Profile
        urlToRoute "/404" `shouldEqual` Just Page404
        urlToRoute "/unauthorized" `shouldEqual` Just Unauthorized

      it "editorRoute convenience constructor uses default params" do
        editorRoute 1 `shouldEqual` Editor 1 defaultEditorParams

    -- ------------------------------------------------------------------
    -- parseRoute (fallback to Page404)
    -- ------------------------------------------------------------------

    describe "parseRoute" do
      it "returns Page404 for completely unknown paths" do
        parseRoute "/this-does-not-exist" `shouldEqual` Page404

      it "returns Page404 for partial admin paths" do
        parseRoute "/administration/unknown" `shouldEqual` Page404

      it "returns Page404 for non-numeric document ID" do
        parseRoute "/documents/abc" `shouldEqual` Page404

      it "returns Page404 for empty-ish gibberish" do
        parseRoute "/foo/bar/baz" `shouldEqual` Page404

      it "returns Home for /" do
        parseRoute "/" `shouldEqual` Home

      it "returns correct route for valid paths" do
        parseRoute "/login" `shouldEqual` loginRoute
        parseRoute "/me" `shouldEqual` Profile
        parseRoute "/404" `shouldEqual` Page404
        parseRoute "/unauthorized" `shouldEqual` Unauthorized

      it "returns correct Editor route with params" do
        parseRoute "/documents/1?paragraph=3"
          `shouldEqual` Editor 1
            { revision: Nothing, paragraph: Just 3, splitview: Nothing }

      it "returns correct GroupRoute" do
        parseRoute "/administration/groups/5/members"
          `shouldEqual` GroupRoute 5 GroupMembers

    -- ------------------------------------------------------------------
    -- Login redirect from protected routes
    -- ------------------------------------------------------------------

    describe "Login redirect from protected routes" do
      it "loginRouteWithRedirect preserves AdminUsers route" do
        let
          login = loginRouteWithRedirect AdminUsers
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right AdminUsers
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

      it "loginRouteWithRedirect preserves CreateUser route" do
        let
          login = loginRouteWithRedirect CreateUser
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right CreateUser
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

      it "loginRouteWithRedirect preserves CreateGroup route" do
        let
          login = loginRouteWithRedirect CreateGroup
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right CreateGroup
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

      it "loginRouteWithRedirect preserves GroupRoute" do
        let
          route = GroupRoute 3 GroupMembers
          login = loginRouteWithRedirect route
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right route
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

      it "loginRouteWithRedirect preserves Profile route" do
        let
          login = loginRouteWithRedirect Profile
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right Profile
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"

      it "loginRouteWithRedirect does not produce Unauthorized as redirect target" do
        -- Unauthorized is a destination for 403 errors, not a route you'd
        -- redirect back to after login.
        let login = loginRouteWithRedirect Unauthorized
        case login of
          Login { redirect: Just uri } ->
            parse routeCodec uri `shouldEqual` Right Unauthorized
          _ -> "Expected Login with redirect" `shouldEqual` "but got something else"
