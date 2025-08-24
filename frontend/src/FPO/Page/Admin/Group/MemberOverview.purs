-- | Overview of Members belonging to a Group.
-- |
-- | TODO: We distinct between group-/superadmins and normal members here, but
-- |       normal members do not have any access to this page and get 404 immediately.
-- |       The mockup explicitly shows that normal members can see the this page, but
-- |       no dropdown to change member roles (reasonable, since they might want to
-- |       view the members of their group, or whatever). Though, there is no way
-- |       to access this page for normal members. We may want to allow normal group
-- |       members to see the list of groups they are in, and thus allow them to
-- |       access this page.

module FPO.Page.Admin.Group.MemberOverview (component) where

import Prelude

import Data.Array (filter, length, null, replicate, slice)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Components.Pagination as P
import FPO.Components.Table.Head as TH
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request
  ( changeRole
  , deleteIgnore
  , getGroup
  , getUser
  )
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.GroupDto
  ( GroupDto
  , GroupID
  , GroupMemberDto
  , getGroupMembers
  , getGroupName
  , getUserInfoID
  , getUserInfoName
  , getUserInfoRole
  , lookupUser
  )
import FPO.Dto.UserDto (UserID, isAdminOf, isUserSuperadmin)
import FPO.Dto.UserRoleDto (Role(..))
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn)
import FPO.UI.Style as Style
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Halogen.Themes.Bootstrap5 as Hb
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_tablehead = Proxy :: Proxy "tablehead"
_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( tablehead :: forall q. H.Slot q TH.Output Unit
  , pagination :: H.Slot P.Query P.Output Unit
  )

type Input = GroupID

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | SetPage P.Output
  | FilterForMember String
  | ChangeSorting TH.Output
  -- | Actions regarding deletion of members.
  | RequestRemoveMember UserID
  | ConfirmRemoveMember UserID
  | CancelModal
  | ReloadGroupMembers
  | NavigateToDocuments
  | SetUserRole GroupMemberDto Role
  | NavigateToUserAdder

-- | Simple "state machine" for the modal system.
data ModalState
  = NoModal
  | RemoveMemberModal UserID

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , groupID :: GroupID
  , group :: Maybe GroupDto
  , filteredMembers :: Array GroupMemberDto
  , modalState :: ModalState
  , isAdmin :: Boolean
  , memberNameFilter :: String
  )

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
    , page: 0
    , groupID: input
    , filteredMembers: []
    , modalState: NoModal
    , error: Nothing
    , group: Nothing
    , isAdmin: false
    , memberNameFilter: ""
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      $
        ( case state.modalState of
            RemoveMemberModal userID ->
              fromMaybe [] do
                group <- state.group
                member <- lookupUser group userID
                pure
                  [ deleteConfirmationModal state.translator userID
                      (const $ getUserInfoName member)
                      CancelModal
                      ConfirmRemoveMember
                      (translate (label :: _ "common_member") state.translator)
                  ]
            _ -> []
        ) <>
          [ renderMemberManagement state
          , HH.div [ HP.classes [ HB.textCenter ] ]
              [ case state.error of
                  Just err -> HH.div
                    [ HP.classes [ HB.alert, HB.alertDanger, HB.mt5 ] ]
                    [ HH.text err ]
                  Nothing -> HH.text ""
              ]
          ]

  renderMemberManagement :: State -> H.ComponentHTML Action Slots m
  renderMemberManagement state =
    HH.div_
      [ HH.h2 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $
              translate (label :: _ "gm_membersOfGroup")
                state.translator <> " "
          , HH.span
              [ HP.classes
                  [ HB.textSecondary, HB.fwBolder, HB.dInlineBlock, HB.textWrap ]
              ]
              [ HH.text $ fromMaybe "" $ getGroupName <$> state.group ]
          ]
      , renderMemberListView state
      ]

  renderMemberListView :: State -> H.ComponentHTML Action Slots m
  renderMemberListView state =
    case state.group of
      Nothing -> HH.div [ HP.classes [ HB.my3, HB.textCenter ] ]
        [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
      Just _ -> HH.div [ HP.classes [ HB.row ] ]
        [ renderSideButtons state
        , renderMembersOverview state
        ]

  -- Renders the overview of members of this group.
  renderMembersOverview :: State -> H.ComponentHTML Action Slots m
  renderMembersOverview state =
    HH.div [ HP.classes [ HB.col12, HB.colMd9, HB.colLg8 ] ]
      [ HH.div [ HP.classes [ HB.card, HB.bgLightSubtle ] ]
          [ HH.div [ HP.class_ HB.cardBody ] [ renderMemberOverview state ] ]
      ]

  -- Search bar and list of members.
  renderMemberOverview :: State -> H.ComponentHTML Action Slots m
  renderMemberOverview state =
    HH.div [ HP.classes [ HB.container ] ]
      [ HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
          [ addColumn
              state.memberNameFilter
              ""
              (translate (label :: _ "gm_searchMembers") state.translator)
              "bi-search"
              HP.InputText
              FilterForMember
          , HH.div [ HP.classes [ HB.col12 ] ]
              [ renderMemberList docs state ]
          , HH.slot _pagination unit P.component ps SetPage
          ]
      ]
    where
    docs = slice (state.page * 10) ((state.page + 1) * 10) state.filteredMembers
    ps =
      { pages: P.calculatePageCount (length state.filteredMembers) 10
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  -- Renders the list of projects.
  renderMemberList :: Array GroupMemberDto -> State -> H.ComponentHTML Action Slots m
  renderMemberList docs state =
    HH.table
      [ HP.classes [ HB.table, HB.tableBordered ] ]
      [ HH.colgroup_
          [ HH.col [ HP.style "width: 55%;" ]
          , HH.col [ HP.style "width: 35%;" ]
          , HH.col [ HP.style "width: 10%;" ]
          ]
      , HH.slot _tablehead unit TH.component
          { columns: tableCols
          , sortedBy: translate (label :: _ "common_userName") state.translator
          }
          ChangeSorting
      , HH.tbody_ $
          if null docs then
            [ HH.tr []
                [ HH.td
                    [ HP.colSpan 3
                    , HP.classes [ HB.textCenter ]
                    ]
                    [ HH.i_ [ HH.text "No members found" ] ]
                ]
            ]
          else
            ( map (renderMemberEntry state) docs
                <> replicate (10 - length docs) (emptyMemberEntry state)
            )
      ]
    where
    tableCols = TH.createTableColumns
      [ { title: translate (label :: _ "common_userName") state.translator
        , style: Nothing
        }
      , { title: translate (label :: _ "gm_role") state.translator
        , style: Nothing
        }
      , { title: ""
        , style: Nothing
        }
      ]

  -- Renders a single project row in the table.
  renderMemberEntry :: forall w. State -> GroupMemberDto -> HH.HTML w Action
  renderMemberEntry state member =
    HH.tr
      []
      [ HH.td [ HP.classes [ HB.textCenter ] ]
          [ HH.text $ getUserInfoName member ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ memberRole ]
      , HH.td [ HP.classes [ HB.textCenter ] ]
          [ buttonRemoveMember state $ getUserInfoID member ]
      ]
    where
    -- Admins have a dropdown to change the role of the member.
    memberRole
      | state.isAdmin =
          HH.select
            [ HE.onValueChange setRole
            , HP.classes [ HB.formSelect, Hb.formSelectSm ]
            ]
            [ HH.option
                [ HP.value adminStr
                , HP.selected (getUserInfoRole member == Admin)
                ]
                [ HH.text adminStr ]
            , HH.option
                [ HP.value memberStr
                , HP.selected (getUserInfoRole member == Member)
                ]
                [ HH.text memberStr ]
            ]
      | otherwise = HH.text userRoleString

    userRoleString = case getUserInfoRole member of
      Admin -> adminStr
      Member -> memberStr

    adminStr :: String
    adminStr = "Admin"

    memberStr :: String
    memberStr = translate (label :: _ "common_member") state.translator

    setRole :: String -> Action
    setRole roleStr =
      if roleStr == adminStr then
        SetUserRole member Admin
      else
        SetUserRole member Member

  -- Renders an empty project row for padding.
  emptyMemberEntry :: forall w. State -> HH.HTML w Action
  emptyMemberEntry state =
    HH.tr []
      [ HH.td
          [ HP.colSpan 3
          , HP.classes [ HB.textCenter ]
          ]
          [ HH.div [ HP.class_ HB.invisible ]
              [ HH.text $ "Empty Row", buttonRemoveMember state "" ]
          ]
      ]

  renderSideButtons :: forall w. State -> HH.HTML w Action
  renderSideButtons state =
    HH.div [ HP.classes [ HB.colMd3, HB.colLg2, HB.col12, HB.mb3 ] ]
      [ HH.div
          [ HP.classes [ HB.dFlex, HB.dMdGrid, HB.justifyContentCenter, HB.gap2 ] ]
          [ renderToDocumentsButton state
          , renderAddMemberButton state
          ]
      ]

  renderToDocumentsButton :: forall w. State -> HH.HTML w Action
  renderToDocumentsButton state =
    HH.button
      [ Style.cyanStyle
      , HE.onClick (const NavigateToDocuments)
      ]
      [ HH.text $ translate (label :: _ "common_projects") state.translator ]

  renderAddMemberButton :: forall w. State -> HH.HTML w Action
  renderAddMemberButton state =
    HH.button
      [ Style.cyanStyle
      , HE.onClick (const $ NavigateToUserAdder)
      ]
      [ HH.text $ translate (label :: _ "gm_addMember") state.translator ]

  buttonRemoveMember :: forall w. State -> String -> HH.HTML w Action
  buttonRemoveMember state memberID =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HE.onClick (const $ RequestRemoveMember memberID)
      , Style.popover
          ( translate (label :: _ "gm_removeMember") state.translator
          )
      ]
      [ HH.i
          [ HP.class_ $ HH.ClassName "bi-door-open" ]
          []
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      s <- H.get
      userResult <- getUser
      case userResult of
        Left _ -> H.modify_ _ { isAdmin = false } -- Ignore error, set not admin
        Right user -> do
          -- Superadmins are considered admins of all groups. We could also change this
          -- such that superadmins (notice that only they can create groups) are admins
          -- of any group they created, and they can demote themselves to members (forever
          -- losing the admin role, until someone else promotes them again). Not sure if
          -- this is useful.
          H.modify_ _
            { isAdmin = user `isAdminOf` s.groupID || isUserSuperadmin user
            }
      handleAction ReloadGroupMembers
      handleAction $ FilterForMember ""
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    FilterForMember mn -> do
      s <- H.get
      let allMembers = fromMaybe [] $ getGroupMembers <$> s.group
      let
        filteredMembers =
          filter
            (contains (Pattern mn) <<< getUserInfoName)
            allMembers
      H.modify_ _
        { filteredMembers = filteredMembers, page = 0, memberNameFilter = mn }
    RequestRemoveMember memberID -> do
      H.modify_ _ { modalState = RemoveMemberModal memberID }
    CancelModal -> do
      H.modify_ \s -> s
        { error = Nothing
        , modalState = NoModal
        }
    ConfirmRemoveMember memberID -> do
      s <- H.get
      deleteResponse <- deleteIgnore
        ("/roles/" <> show s.groupID <> "/" <> memberID)
      case deleteResponse of
        Left err -> do
          H.modify_ _
            { error = Just (show err)
            , modalState = NoModal
            }
        Right _ -> do
          log "Removed member successfully"
          H.modify_ _
            { error = Nothing
            , modalState = NoModal
            }

      handleAction ReloadGroupMembers
      handleAction (FilterForMember "")
    ChangeSorting (TH.Clicked _ _) -> do
      -- TODO: enable filtering using the columns
      --       For this, we need to either change the group or additionally store
      --       all members in the state.

      H.tell _pagination unit $ P.SetPageQ 0
    ReloadGroupMembers -> do
      state <- H.get
      groupWithError <- getGroup state.groupID
      case groupWithError of
        Left _ -> pure unit -- TODO 
        Right group ->
          H.modify_ _
            { group = Just group
            , filteredMembers = getGroupMembers group
            , page = 0
            }
    NavigateToDocuments -> do
      log "Routing to document overview"
      s <- H.get
      navigate (ViewGroupDocuments { groupID: s.groupID })
    SetUserRole member role -> do
      -- TODO: Instead of writing code like this, we should resort to using only high-level
      --       requests in components. These high-level requests must then be implemented
      --       in another module, and the component should only call them and easily handle the
      --       result.
      s <- H.get
      let userID = getUserInfoID member
      if getUserInfoRole member == role then
        log "User already has this role, ignoring"
      else do
        response <- changeRole s.groupID userID role
        case response of
          Left err -> do
            H.modify_ _
              { error = Just (show err)
              , modalState = NoModal
              }
          Right _ -> do
            log "Changed user role successfully"
            handleAction ReloadGroupMembers
            handleAction (FilterForMember "")
      handleAction ReloadGroupMembers
      handleAction (FilterForMember "")
    NavigateToUserAdder -> do
      s <- H.get
      navigate (GroupAddMembers { groupID: s.groupID })
