-- | Admin user overview and management page.
-- |
-- | TODO: Implement the actual admin panel functionality, see mockups.
-- |       For a start, this page connects with the backend
-- |       and checks if we're even allowed to access this page, then
-- |       handles the response accordingly.
-- |       Also, this page implements a very simple dummy/mockup user
-- |       management system, which allows us to filter and create users
-- |       (not connected to the backend yet).

module FPO.Page.Admin.Users (component) where

import Prelude

import Affjax (printError)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (filter, length, replicate, slice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains, null)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.Pagination as P
import FPO.Data.Email as Email
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( LoadState(..)
  , deleteIgnore
  , getFromJSONEndpoint
  , getUser
  , postJson
  )
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.CreateUserDto
  ( CreateUserDto
  , getEmail
  , getName
  , getPassword
  , withEmail
  , withName
  , withPassword
  )
import FPO.Dto.CreateUserDto as CreateUserDto
import FPO.Dto.UserOverviewDto (UserOverviewDto)
import FPO.Dto.UserOverviewDto as UserOverviewDto
import FPO.Page.HTML
  ( addButton
  , addCard
  , addColumn
  , addError
  , deleteButton
  , emptyEntryText
  )
import FPO.Translations.Labels (Labels)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | DoNothing -- Placeholder for future actions
  | SetPage P.Output
  -- TODO: Of course, we should add dedicated components for the filtering
  --        and creation of users, but for now, we just use these actions to
  --        demonstrate the functionality (mockup!). Or, might be even better,
  --        to add a general component that allows us to create simple forms
  --        with a label, input field(s), and a button. This way, we dont have to
  --        repeat ourselves over and over again.
  | ChangeFilterUsername String
  | ChangeFilterEmail String
  | ChangeCreateUsername String
  | ChangeCreateEmail String
  | ChangeCreatePassword String
  | RequestDeleteUser UserOverviewDto
  | PerformDeleteUser String
  | CloseDeleteModal
  | GetUser String
  | Filter
  | CreateUser

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , users :: LoadState (Array UserOverviewDto)
  , filteredUsers :: Array UserOverviewDto
  , filterUsername :: String
  , filterEmail :: String
  , createUserDto :: CreateUserDto
  , createUserError :: Maybe String
  , createUserSuccess :: Maybe String
  , requestDeleteUser :: Maybe UserOverviewDto
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
    , users: Loading
    , filteredUsers: []
    , filterUsername: ""
    , filterEmail: ""
    , createUserDto: CreateUserDto.empty
    , createUserError: Nothing
    , createUserSuccess: Nothing
    , requestDeleteUser: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.dFlex, HB.justifyContentCenter, HB.my5 ]
      ]
      [ renderDeleteModal state
      , renderUserManagement state
      , addError state.error
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- H.liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $ navigate Page404
      fetchAndLoadUsers

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    DoNothing -> do
      pure unit
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    ChangeFilterUsername username -> do H.modify_ _ { filterUsername = username }
    ChangeFilterEmail email -> do H.modify_ _ { filterEmail = email }
    ChangeCreateUsername username -> do
      state <- H.get
      H.modify_ _ { createUserDto = withName username state.createUserDto }
    ChangeCreateEmail email -> do
      state <- H.get
      H.modify_ _ { createUserDto = withEmail email state.createUserDto }
    ChangeCreatePassword password -> do
      state <- H.get
      H.modify_ _ { createUserDto = withPassword password state.createUserDto }
    RequestDeleteUser userOverviewDto -> H.modify_ _
      { requestDeleteUser = Just userOverviewDto }
    PerformDeleteUser userId -> do
      response <- H.liftAff $ deleteIgnore ("/users/" <> userId)
      case response of
        Left err -> do
          s <- H.get
          H.modify_ _
            { error = Just
                ( translate (label :: _ "admin_users_failedToDeleteUser")
                    s.translator <> ": " <> (printError err)
                )
            , requestDeleteUser = Nothing
            }
        Right _ -> do
          H.modify_ _ { error = Nothing, requestDeleteUser = Nothing }
          fetchAndLoadUsers
    CloseDeleteModal -> do H.modify_ _ { requestDeleteUser = Nothing }
    GetUser _ -> navigate (Profile { loginSuccessful: Nothing })
    Filter -> do
      state <- H.get
      filteredUsers <- case state.users of
        Loading -> pure []
        Loaded userList ->
          pure $ filter
            ( \user ->
                ( if null state.filterUsername then false
                  else
                    contains (Pattern state.filterUsername)
                      (UserOverviewDto.getName user)
                )
                  ||
                    ( if null state.filterEmail then false
                      else
                        contains
                          (Pattern state.filterEmail)
                          (UserOverviewDto.getEmail user)
                    )
            )
            userList
      log $ show (length filteredUsers)
      H.modify_ _ { filteredUsers = filteredUsers }
    CreateUser -> do
      state <- H.get
      response <- H.liftAff $ postJson "/register" (encodeJson state.createUserDto)
      case response of
        Left err -> do
          H.modify_ _
            { createUserError = Just $
                ( translate (label :: _ "admin_users_failedToCreateUser")
                    state.translator
                ) <> ": " <> printError err
            }
        Right _ -> do
          H.modify_ _
            { createUserError = Nothing
            , createUserSuccess = Just
                ( translate (label :: _ "admin_users_successfullyCreatedUser")
                    state.translator
                )
            , createUserDto = CreateUserDto.empty
            }
          fetchAndLoadUsers

  renderUserManagement :: State -> H.ComponentHTML Action Slots m
  renderUserManagement state =
    HH.div [ HP.classes [ HB.w100, HB.col12 ] ]
      [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $ translate (label :: _ "au_userManagement") state.translator
          ]
      , case state.users of
          Loading ->
            HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
              [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
          Loaded _ -> renderUserListView state
      ]

  renderUserListView :: State -> H.ComponentHTML Action Slots m
  renderUserListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentAround ] ]
      [ renderFilterBy state
      , renderUserList state
      , renderNewUserForm state
      ]

  renderFilterBy :: State -> H.ComponentHTML Action Slots m
  renderFilterBy state =
    addCard (translate (label :: _ "common_filterBy") state.translator)
      [ HP.classes [ HB.col3 ] ] $ HH.div
      [ HP.classes [ HB.row ] ]
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              state.filterUsername
              (translate (label :: _ "common_userName") state.translator)
              (translate (label :: _ "common_userName") state.translator)
              "bi-person"
              HP.InputText
              ChangeFilterUsername
          , addColumn
              ""
              (translate (label :: _ "common_email") state.translator)
              (translate (label :: _ "common_email") state.translator)
              "bi-envelope-fill"
              HP.InputEmail
              ChangeFilterEmail
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  true
                  "Filter"
                  (Just "bi-funnel")
                  (const Filter)
              ]
          ]
      ]

  -- Creates a list of (dummy) users with pagination.
  renderUserList :: State -> H.ComponentHTML Action Slots m
  renderUserList state =
    addCard (translate (label :: _ "admin_users_listOfUsers") state.translator)
      [ HP.classes [ HB.col6 ] ] $ HH.div_
      [ HH.ul [ HP.classes [ HB.listGroup ] ]
          $ map (createUserEntry state.translator) usrs
              <> replicate (10 - length usrs)
                emptyEntryText
      -- TODO: ^ Artificially inflating the list to 10 entries
      --         allows for a fixed overall height of the list,
      --         but it's not a clean solution at all.
      , HH.slot _pagination unit P.component ps SetPage
      ]
    where
    usrs = slice (state.page * 10) ((state.page + 1) * 10) state.filteredUsers
    ps =
      { pages: P.calculatePageCount (length state.filteredUsers) 10
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  -- Creates a form to create a new (dummy) user.
  renderNewUserForm :: forall w. State -> HH.HTML w Action
  renderNewUserForm state =
    addCard (translate (label :: _ "admin_users_createNewUser") state.translator)
      [ HP.classes [ HB.col3 ] ] $ HH.div_
      [ HH.div [ HP.classes [ HB.col ] ]
          [ addColumn
              (getName state.createUserDto)
              (translate (label :: _ "common_userName") state.translator)
              (translate (label :: _ "common_userName") state.translator)
              "bi-person"
              HP.InputText
              ChangeCreateUsername
          , addColumn
              (getEmail state.createUserDto)
              (translate (label :: _ "common_email") state.translator)
              (translate (label :: _ "common_email") state.translator)
              "bi-envelope-fill"
              HP.InputEmail
              ChangeCreateEmail
          , addColumn
              (getPassword state.createUserDto)
              (translate (label :: _ "common_password") state.translator)
              (translate (label :: _ "common_password") state.translator)
              "bi-lock-fill"
              HP.InputPassword
              ChangeCreatePassword
          ]
      , HH.div [ HP.classes [ HB.col12, HB.textCenter ] ]
          [ HH.div [ HP.classes [ HB.dInlineBlock ] ]
              [ addButton
                  (isCreateUserFormValid state.createUserDto)
                  (translate (label :: _ "admin_users_create") state.translator)
                  (Just "bi-plus-circle")
                  (const CreateUser)
              ]
          ]
      , case state.createUserError of
          Just err -> HH.div
            [ HP.classes [ HB.alert, HB.alertDanger, HB.textCenter, HB.mt3 ] ]
            [ HH.text err ]
          Nothing -> HH.text ""
      , case state.createUserSuccess of
          Just err -> HH.div
            [ HP.classes [ HB.alert, HB.alertSuccess, HB.textCenter, HB.mt3 ] ]
            [ HH.text err ]
          Nothing -> HH.text ""
      ]

  -- Creates a (dummy) user entry for the list.
  createUserEntry
    :: forall w. Translator Labels -> UserOverviewDto -> HH.HTML w Action
  createUserEntry translator userOverviewDto =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.span [ HP.classes [ HB.col5 ] ]
          [ HH.text $ UserOverviewDto.getName userOverviewDto ]
      , HH.span [ HP.classes [ HB.col5 ] ]
          [ HH.text $ UserOverviewDto.getEmail userOverviewDto ]
      , HH.div [ HP.classes [ HB.col2, HB.dFlex, HB.justifyContentEnd, HB.gap1 ] ]
          [ deleteButton (const $ RequestDeleteUser userOverviewDto)
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
              , HE.onClick $ const $ GetUser (UserOverviewDto.getID userOverviewDto)
              , HP.title
                  (translate (label :: _ "admin_users_goToProfilePage") translator)
              ]
              [ HH.i [ HP.classes [ HB.bi, (H.ClassName "bi-person-fill") ] ] []

              ]
          ]
      ]

renderDeleteModal :: forall m. State -> HH.HTML m Action
renderDeleteModal state =
  case state.requestDeleteUser of
    Nothing -> HH.div [ HP.classes [ HB.dNone ] ] []
    Just userOverviewDto -> deleteConfirmationModal
      state.translator
      userOverviewDto
      UserOverviewDto.getName
      CloseDeleteModal
      (PerformDeleteUser <<< UserOverviewDto.getID)
      (translate (label :: _ "admin_users_theUser") state.translator)

isCreateUserFormValid :: CreateUserDto -> Boolean
isCreateUserFormValid createUserDto =
  not (null $ getName createUserDto)
    && not (null $ getEmail createUserDto)
    && not (null $ getPassword createUserDto)
    && Email.isValidEmailStrict (getEmail createUserDto)

fetchAndLoadUsers
  :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
fetchAndLoadUsers = do
  maybeUsers <- H.liftAff $ getFromJSONEndpoint decodeJson "/users"
  case maybeUsers of
    Nothing -> do
      state <- H.get
      H.modify_ _
        { error = Just $ translate (label :: _ "admin_users_failedToLoadUsers")
            state.translator
        }
    Just users -> do
      H.modify_ _ { users = Loaded users, filteredUsers = users }
