-- | Admin user overview and management page.
-- |
-- | TODO:
-- | - Implement the `goToProfilePage` funcionality
-- |   (for users other than the one logged in).

module FPO.Page.Admin.Users (component) where

import Prelude

import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.UI.UserFilter as Filter
import FPO.Components.UI.UserList as UserList
import FPO.Data.AppError (AppError(..))
import FPO.Data.Email as Email
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (deleteIgnore, getUser, postString)
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
import FPO.Dto.UserDto (UserID, getUserID, isUserSuperadmin)
import FPO.Dto.UserOverviewDto as UOD
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addButton, addCard, addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_filter = Proxy :: Proxy "filter"
_userlist = Proxy :: Proxy "userlist"

data ButtonEvent
  = EffectDeleteUser
  | EffectGoToProfilePage

type Slots =
  ( filter :: forall q. H.Slot q Filter.Output Unit
  , userlist :: H.Slot UserList.Query (UserList.Output ButtonEvent) Unit
  )

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | ChangeCreateUsername String
  | ChangeCreateEmail String
  | ChangeCreatePassword String
  | RequestDeleteUser UOD.UserOverviewDto
  | PerformDeleteUser String
  | CloseDeleteModal
  | GetUser String
  | HandleFilter Filter.Output
  | HandleUserList (UserList.Output ButtonEvent)
  | CreateUser

type State = FPOState
  ( createUserDto :: CreateUserDto
  , requestDeleteUser :: Maybe UOD.UserOverviewDto
  -- | The ID of the user that is currently viewing the page.
  , userID :: Maybe UserID
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
    , createUserDto: CreateUserDto.empty
    , requestDeleteUser: Nothing
    , userID: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.containerXl, HB.my5 ]
      ]
      [ renderDeleteModal state
      , renderUserManagement state
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      userResult <- getUser
      case userResult of
        Left _ -> pure unit -- TODO Ignore error, redirect to 404
        Right user -> do
          when (not $ isUserSuperadmin user) $ navigate Page404
          H.modify_ _ { userID = Just $ getUserID user }

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
      response <- deleteIgnore ("/users/" <> userId)
      case response of
        Left _ -> do
          H.modify_ _
            { requestDeleteUser = Nothing
            }
        Right _ -> do
          H.modify_ _ { requestDeleteUser = Nothing }
          H.tell _userlist unit UserList.ReloadUsersQ
    CloseDeleteModal -> do H.modify_ _ { requestDeleteUser = Nothing }
    GetUser userId -> do
      userResult <- getUser
      case userResult of
        Left _ -> pure unit -- Ignore error like in editor
        Right user -> navigate
          ( Profile
              { loginSuccessful: Nothing
              , userId: if getUserID user == userId then Nothing else Just userId
              }
          )
    HandleFilter f -> do
      H.tell _userlist unit (UserList.HandleFilterQ f)
    HandleUserList (UserList.Loading _) -> do
      -- We do not care about the loading state here,
      -- as the user list component handles it itself.
      -- Would be nice to have a way to either render the user
      -- list component (and the whole scene), or show a loading spinner
      -- instead, but this doesnt work as the user list component
      -- must be rendered in the scene in order to exist and do it's work :)
      pure unit
    HandleUserList (UserList.Error _) -> pure unit
    CreateUser -> do
      state <- H.get
      response <- postString "/register" (encodeJson state.createUserDto) -- could be a postIgnore
      case response of
        Left err -> do
          updateStore $ Store.AddError $ ServerError
            ( ( translate (label :: _ "admin_users_failedToCreateUser")
                  state.translator
              ) <> ": " <> (show err)
            )
        Right _ -> do
          updateStore $ Store.AddSuccess
            ( translate (label :: _ "admin_users_successfullyCreatedUser")
                state.translator
            )
          H.modify_ _
            { createUserDto = CreateUserDto.empty
            }
          H.tell _userlist unit UserList.ReloadUsersQ
    HandleUserList (UserList.ButtonPressed userOverviewDto effect) -> do
      case effect of
        EffectDeleteUser -> H.modify_ _ { requestDeleteUser = Just userOverviewDto }
        EffectGoToProfilePage -> do
          handleAction $ GetUser $ UOD.getID userOverviewDto

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
      , renderUserList state
      , renderNewUserForm state
      ]

  renderFilterBy :: H.ComponentHTML Action Slots m
  renderFilterBy =
    HH.slot _filter unit Filter.component unit HandleFilter

  renderUserList :: State -> H.ComponentHTML Action Slots m
  renderUserList state =
    HH.slot _userlist unit UserList.component events HandleUserList
    where
    events = \u ->
      let
        isMe = state.userID == Just (UOD.getID u)
      in
        [ { popover: translate (label :: _ "admin_users_deleteUser") state.translator
          , effect: EffectDeleteUser
          , icon: "bi-trash"
          , classes: [ HB.btn, HB.btnOutlineDanger, HB.btnSm, HB.me2 ]
              <> (if isMe then [ HB.opacity25 ] else [])
          , disabled: isMe
          }
        , { popover: translate (label :: _ "admin_users_goToProfilePage")
              state.translator
          , effect: EffectGoToProfilePage
          , icon: "bi-person-fill"
          , classes: [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
          , disabled: false
          }
        ]

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
      ]

renderDeleteModal :: forall m. State -> HH.HTML m Action
renderDeleteModal state =
  case state.requestDeleteUser of
    Nothing -> HH.div [ HP.classes [ HB.dNone ] ] []
    Just userOverviewDto -> deleteConfirmationModal
      state.translator
      userOverviewDto
      UOD.getName
      CloseDeleteModal
      (PerformDeleteUser <<< UOD.getID)
      (translate (label :: _ "admin_users_theUser") state.translator)

isCreateUserFormValid :: CreateUserDto -> Boolean
isCreateUserFormValid createUserDto =
  not (null $ getName createUserDto)
    && not (null $ getEmail createUserDto)
    && not (null $ getPassword createUserDto)
    && Email.isValidEmailStrict (getEmail createUserDto)
