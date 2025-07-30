-- | Admin user overview and management page.
-- |
-- | TODO:
-- | - Implement the `goToProfilePage` funcionality
-- |   (for users other than the one logged in).
-- | - Trying to delete oneself fails in the backend,
-- |   but no error is shown in the UI. Perhaps, we should
-- |   simply prohibit deleting oneself in the UI?

module FPO.Page.Admin.Users (component) where

import Prelude

import Affjax (printError)
import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.UI.UserFilter as Filter
import FPO.Components.UI.UserList as UserList
import FPO.Data.Email as Email
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (deleteIgnore, getUser, postJson)
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
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addButton, addCard, addColumn, addError)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_filter = Proxy :: Proxy "filter"
_userlist = Proxy :: Proxy "userlist"

type Slots =
  ( filter :: forall q. H.Slot q Filter.Output Unit
  , userlist :: H.Slot UserList.Query UserList.Output Unit
  )

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | ChangeCreateUsername String
  | ChangeCreateEmail String
  | ChangeCreatePassword String
  | RequestDeleteUser UserOverviewDto
  | PerformDeleteUser String
  | CloseDeleteModal
  | GetUser String
  | HandleFilter Filter.Output
  | HandleUserList UserList.Output
  | CreateUser

type State = FPOState
  ( error :: Maybe String
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
    , createUserDto: CreateUserDto.empty
    , createUserError: Nothing
    , createUserSuccess: Nothing
    , requestDeleteUser: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ]
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
      H.tell _userlist unit UserList.ReloadUsersQ

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
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
          H.tell _userlist unit UserList.ReloadUsersQ
    CloseDeleteModal -> do H.modify_ _ { requestDeleteUser = Nothing }
    GetUser _ -> navigate (Profile { loginSuccessful: Nothing })
    HandleFilter f -> do
      H.tell _userlist unit (UserList.HandleFilterQ f)
    HandleUserList (UserList.Loading b) -> do
      -- We do not care about the loading state here,
      -- as the user list component handles it itself.
      -- Would be nice to have a way to either render the user
      -- list component (and the whole scene), or show a loading spinner
      -- instead, but this doesnt work as the user list component
      -- must be rendered in the scene in order to exist and do it's work :)
      pure unit
    HandleUserList (UserList.Error err) -> do
      H.modify_ _ { error = Just err }
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
          H.tell _userlist unit UserList.ReloadUsersQ

  renderUserManagement :: State -> H.ComponentHTML Action Slots m
  renderUserManagement state =
    HH.div_ $
      [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $ translate (label :: _ "au_userManagement") state.translator
          ]
      , renderUserListView state
      ]

  renderUserListView :: State -> H.ComponentHTML Action Slots m
  renderUserListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentAround ] ]
      [ renderFilterBy
      , renderUserList
      , renderNewUserForm state
      ]

  renderFilterBy :: H.ComponentHTML Action Slots m
  renderFilterBy =
    HH.slot _filter unit Filter.component unit HandleFilter

  renderUserList :: H.ComponentHTML Action Slots m
  renderUserList =
    HH.slot _userlist unit UserList.component unit HandleUserList

  -- Creates a form to create a new (dummy) user.
  renderNewUserForm :: forall w. State -> HH.HTML w Action
  renderNewUserForm state =
    addCard (translate (label :: _ "admin_users_createNewUser") state.translator)
      [ HP.classes [ HB.col12, HB.colMd3, HB.colLg3 ] ] $ HH.div_
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
