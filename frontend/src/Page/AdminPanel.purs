-- | Simple admin panel page. Right now, this is just a placeholder.
-- |
-- | This page is only accessible to users with admin privileges.
-- |
-- | TODO: Implement the actual admin panel functionality, see mockups.
-- |       For a start, this page connects with the backend
-- |       and checks if we're even allowed to access this page, then
-- |       handles the response accordingly.
-- |       Also, this page implements a very simple dummy/mockup user
-- |       management system, which allows us to filter and create users
-- |       (not connected to the backend yet).

module FPO.Page.AdminPanel (component) where

import Prelude

import Data.Array (filter, length, slice, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.HTML (addButton, addColumn, createCard)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | DoNothing -- Placeholder for future actions
  -- TODO: If we want/have to use pagination,
  --       we should move this to a separate module (component).
  | SetPage Int
  -- TODO: Of course, we should add components for the filtering and creation
  --       of users, but for now, we just use these actions to
  --       demonstrate the functionality (mockup!).
  | ChangeFilterUsername String
  | ChangeCreateUsername String
  | Filter
  | CreateUser

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , users :: Array String
  , filteredUsers :: Array String
  , filterUsername :: String
  , createUsername :: String
  )

-- | Admin panel page component.
component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Unit output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { translator: fromFpoTranslator context
    , error: Nothing
    , page: 0
    , users: []
    , filteredUsers: []
    , filterUsername: ""
    , createUsername: ""
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ renderAdminPanel state
      , HH.div [ HP.classes [ HB.textCenter ] ]
          [ case state.error of
              Just err -> HH.div [ HP.classes [ HB.alert, HB.alertDanger, HB.mt5 ] ]
                [ HH.text err ]
              Nothing -> HH.text ""
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      -- TODO: Usually, we would fetch some data here (and handle
      --       the error of missing credentials), but for now,
      --       we just check if the user is an admin and redirect
      --       to a 404 page if not.
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $
        navigate Page404

      let users = map (\i -> if i == 23 then "test" else "User " <> show i) (1 .. 30)
      H.modify_ _ { users = users, filteredUsers = users }
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    DoNothing -> do
      pure unit
    SetPage p -> do
      H.modify_ _ { page = p }
    ChangeFilterUsername username -> do
      H.modify_ _ { filterUsername = username }
    ChangeCreateUsername username -> do
      H.modify_ _ { createUsername = username }
    Filter -> do
      s <- H.get
      let
        filteredUsers = filter (\u -> contains (Pattern s.filterUsername) u) s.users
      H.modify_ _ { filteredUsers = filteredUsers, page = 0 }
    CreateUser -> do
      newUser <- H.gets _.createUsername
      if newUser == "" then H.modify_ _ { error = Just "Username cannot be empty." }
      else do
        H.modify_ \s -> s
          { error = Nothing
          , users = newUser : s.users
          , filteredUsers = newUser : s.users
          , createUsername = ""
          }

  renderAdminPanel :: forall w. State -> HH.HTML w Action
  renderAdminPanel state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.colSm12, HB.colMd10, HB.colLg9 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "ap_adminPanel") state.translator ]
          , renderUserListView state
          ]
      ]

  renderUserListView :: forall w. State -> HH.HTML w Action
  renderUserListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ renderFilterBy state
      , renderUserList state
      , renderNewUserForm state
      ]

  renderFilterBy :: forall w. State -> HH.HTML w Action
  renderFilterBy state =
    createCard "Filter by" [ HP.classes [ HB.col3 ] ] $ HH.div
      [ HP.classes [ HB.row ] ]
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.filterUsername
              "Username:"
              "Username"
              "bi-person"
              HP.InputText
              ChangeFilterUsername
          , addColumn
              ""
              "Email:"
              "Email"
              "bi-envelope-fill"
              HP.InputEmail
              (const DoNothing)
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  "Filter"
                  (Just "bi-funnel")
                  (const Filter)
              ]
          ]
      ]

  -- Creates a list of (dummy) users with pagination.
  renderUserList :: forall w. State -> HH.HTML w Action
  renderUserList state =
    createCard "List of Users" [ HP.classes [ HB.col4 ] ] $ HH.div_
      [ HH.ul [ HP.classes [ HB.listGroup ] ]
          $ map createUserEntry
              (slice (state.page * 10) ((state.page + 1) * 10) state.filteredUsers)
      , HH.ul [ HP.classes [ HB.pagination, HB.mt3, HB.justifyContentCenter ] ]
          $ map (\i -> createPageItem i (i == state.page + 1))
              ( range' 1
                  ( length state.filteredUsers `div` 10 +
                      if length state.filteredUsers `mod` 10 > 0 then 1 else 0
                  )
              )
      ]

  -- Creates a form to create a new (dummy) user.
  renderNewUserForm :: forall w. State -> HH.HTML w Action
  renderNewUserForm state =
    createCard "Create New User" [ HP.classes [ HB.col3 ] ] $ HH.div_
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.createUsername
              "Username"
              "Username"
              "bi-person"
              HP.InputText
              ChangeCreateUsername
          , addColumn
              ""
              "Email"
              "Email"
              "bi-envelope-fill"
              HP.InputEmail
              (const DoNothing)
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  "Create"
                  (Just "bi-plus-circle")
                  (const CreateUser)
              ]
          ]
      ]

  -- Creates a (dummy) user entry for the list.
  createUserEntry :: forall w. String -> HH.HTML w Action
  createUserEntry userName =
    HH.li [ HP.classes [ HB.listGroupItem ] ]
      [ HH.text userName ]

  -- Creates a page item for pagination.
  createPageItem :: forall w. Int -> Boolean -> HH.HTML w Action
  createPageItem pageNumber enabled =
    HH.li
      [ HP.classes $ [ HB.pageItem ] <> if enabled then [ HB.active ] else [] ]
      [ HH.a
          [ HP.classes [ HB.pageLink ]
          , HP.href "#"
          , HE.onClick $ const (SetPage (pageNumber - 1))
          ]
          [ HH.text $ show pageNumber
          ]
      ]

  -- A range function that is not completely weird, compared to the one in `Data.Array`.
  -- Spans from start to end, inclusive. As you would expect, if the end is smaller
  -- than the start, it returns an **empty** array!
  range' :: Int -> Int -> Array Int
  range' 1 0 = []
  range' start end = start .. end
