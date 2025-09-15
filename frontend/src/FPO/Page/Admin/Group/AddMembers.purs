-- | Page used to assign users to a specific group.

module FPO.Page.Admin.Group.AddMembers (component) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.UI.UserFilter as Filter
import FPO.Components.UI.UserList as UserList
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( changeRole
  , getAuthorizedUser
  , getGroup
  , removeUser
  )
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.GroupDto (GroupDto, GroupID, getGroupName, isUserInGroup)
import FPO.Dto.UserOverviewDto (getID)
import FPO.Dto.UserRoleDto (Role(..))
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
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

data ButtonEvent
  = EffectAddUser
  | EffectRemoveUser

type Slots =
  ( filter :: forall q. H.Slot q Filter.Output Unit
  , userlist :: H.Slot UserList.Query (UserList.Output ButtonEvent) Unit
  )

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | HandleFilter Filter.Output
  | HandleUserList (UserList.Output ButtonEvent)
  | ReloadGroup

type State = FPOState
  ( group :: Maybe GroupDto
  , groupID :: GroupID
  )

type Input = GroupID

-- | Admin panel page component.
component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Input output m
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
  initialState :: Connected FPOTranslator Input -> State
  initialState { context, input } =
    { translator: fromFpoTranslator context
    , group: Nothing
    , groupID: input
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    case state.group of
      Just g -> HH.div
        [ HP.classes [ HB.containerXl, HB.my5 ]
        ]
        [ renderMemberManagement state g
        ]
      Nothing -> HH.div [ HP.classes [ HB.textCenter, HB.my5 ] ]
        [ HH.h1 []
            [ HH.text $ translate (label :: _ "gmam_loadingGroup") state.translator ]
        , HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] []
        ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      userWithError <- getAuthorizedUser state.groupID
      case userWithError of
        Left _ -> pure unit
        Right maybeUser ->
          case maybeUser of
            Nothing -> do
              navigate Page404
            Just _ -> do
              handleAction ReloadGroup
              -- User is either a superadmin or an admin of the group,
              -- so we can proceed to load the group and user list.
              H.tell _userlist unit UserList.ReloadUsersQ
    Receive { context, input } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

      s <- H.get
      when (s.groupID /= input) $ do
        H.modify_ _ { groupID = input }
        handleAction ReloadGroup
    HandleFilter f -> do
      H.tell _userlist unit (UserList.HandleFilterQ f)
    HandleUserList (UserList.Loading _) -> do
      pure unit
    HandleUserList (UserList.Error _) -> pure unit
    HandleUserList (UserList.ButtonPressed userOverviewDto effect) -> do
      s <- H.get
      case effect of
        EffectAddUser -> do
          response <- changeRole s.groupID (getID userOverviewDto) Member
          case response of
            Left _ -> pure unit
            Right _ -> handleAction ReloadGroup
        EffectRemoveUser -> do
          response <- removeUser s.groupID (getID userOverviewDto)
          case response of
            Left _ -> pure unit
            Right _ -> handleAction ReloadGroup
    ReloadGroup -> do
      s <- H.get
      g <- getGroup s.groupID
      case g of
        Right group -> do
          H.modify_ _
            { group = Just group
            }
        Left _ -> pure unit

  renderMemberManagement :: State -> GroupDto -> H.ComponentHTML Action Slots m
  renderMemberManagement state group =
    HH.div_ $
      [ HH.h2 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $
              translate (label :: _ "gmam_assignMembers")
                state.translator <> " "
          , HH.span
              [ HP.classes
                  [ HB.textSecondary, HB.fwBolder, HB.dInlineBlock, HB.textWrap ]
              ]
              [ HH.text $ getGroupName group ]
          ]
      , renderUserListView state group
      ]

  renderUserListView :: State -> GroupDto -> H.ComponentHTML Action Slots m
  renderUserListView state group =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ renderFilterBy
      , renderUserList state group
      ]

  renderFilterBy :: H.ComponentHTML Action Slots m
  renderFilterBy =
    HH.slot _filter unit Filter.component unit HandleFilter

  renderUserList :: State -> GroupDto -> H.ComponentHTML Action Slots m
  renderUserList state group =
    HH.slot _userlist unit UserList.component events HandleUserList
    where
    events = \u ->
      [ if isUserInGroup group (getID u) then
          { popover: translate (label :: _ "gmam_removeMember") state.translator
          , effect: EffectRemoveUser
          , icon: "bi-person-dash-fill"
          , classes: [ HB.btn, HB.btnDanger, HB.btnSm ]
          , disabled: false
          }
        else
          { popover: translate (label :: _ "gmam_addMember") state.translator
          , effect: EffectAddUser
          , icon: "bi-person-plus-fill"
          , classes: [ HB.btn, HB.btnPrimary, HB.btnSm ]
          , disabled: false
          }
      ]
