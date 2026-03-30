-- | Page for creating a new group.
-- |
-- | This is a sub-page of the Administration page, accessed via the "Add" button
-- | on the Groups tab. It allows the user to specify a group name, description,
-- | and add members (with role selection) during group creation.
-- |
-- | The member search bar shows a dropdown popover of addable users only when the
-- | input has focus (like a combobox / autocomplete). The first result is
-- | highlighted by default; pressing Enter adds it. Clicking a user also adds
-- | them. In both cases the popover dismisses and re-appears when the user types
-- | or clicks the input again. Clicking outside dismisses via a transparent overlay.
-- |
-- | The current user (creator) is always added as Admin and shown with a minimal
-- | "(You · Admin)" label — no role toggle or remove button.

module FPO.Page.Admin.CreateGroup
  ( component
  ) where

import Prelude

import Data.Array (filter, head, length, mapWithIndex, notElem, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, toLower)
import Data.String (null) as String
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (addGroup, changeRole, getUser, getUsers)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.GroupDto (GroupCreate(..))
import FPO.Dto.UserDto
  ( FullUserDto
  , UserID
  , getUserEmail
  , getUserID
  , getUserName
  , isAdmin
  )
import FPO.Dto.UserOverviewDto (UserOverviewDto)
import FPO.Dto.UserOverviewDto as UOD
import FPO.Dto.UserRoleDto (Role(..))
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.Css as HB
import FPO.UI.HTML (addColumn)
import FPO.UI.Style as Style
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.KeyboardEvent as KE

-- | A member added to the group during creation.
type AddedMember =
  { userId :: UserID
  , userName :: String
  , userEmail :: String
  , role :: Role
  , isCreator :: Boolean
  }

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | DoNothing
  | ChangeGroupName String
  | ChangeGroupDescription String
  | SubmitCreateGroup Event
  | Cancel
  -- User search actions
  | SearchUsers String
  | AddUser UserOverviewDto
  | FocusUserSearch
  | CloseUserSearch
  | SearchKeyDown KE.KeyboardEvent
  -- Member list actions
  | SetMemberRole AddedMember Role
  | RemoveMember UserID

type State = FPOState
  ( groupName :: String
  , groupDescription :: String
  , waiting :: Boolean
  -- Current user
  , currentUser :: Maybe FullUserDto
  -- All users (loaded once on init)
  , allUsers :: Array UserOverviewDto
  , loadingUsers :: Boolean
  -- Member list state
  , addedMembers :: Array AddedMember
  -- User search
  , userSearchFilter :: String
  , searchFocused :: Boolean
  , searchSubmitGuard :: Boolean
  )

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
    , groupName: ""
    , groupDescription: ""
    , waiting: false
    , currentUser: Nothing
    , allUsers: []
    , loadingUsers: false
    , addedMembers: []
    , userSearchFilter: ""
    , searchFocused: false
    , searchSubmitGuard: false
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      [ -- Transparent overlay to dismiss the user-search dropdown on outside click
        if state.searchFocused then
          HH.div
            [ HP.style
                "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; z-index: 1040; background: transparent;"
            , HE.onClick $ const CloseUserSearch
            ]
            []
        else HH.text ""
      , HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
          [ HH.div [ HP.classes [ HB.col12, HB.colMd8, HB.colLg6 ] ]
              [ HH.div [ HP.classes [ HB.card ] ]
                  [ HH.div [ HP.classes [ HB.cardHeader ] ]
                      [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                          [ HH.i
                              [ HP.classes [ H.ClassName "bi-folder-plus", HB.me2 ] ]
                              []
                          , HH.text $ translate
                              (label :: _ "admin_groups_createNewGroup")
                              state.translator
                          ]
                      ]
                  , HH.div [ HP.classes [ HB.cardBody ] ]
                      [ renderForm state ]
                  ]
              ]
          ]
      ]

  renderForm :: State -> H.ComponentHTML Action () m
  renderForm state =
    HH.form
      [ HE.onSubmit SubmitCreateGroup ]
      [ addColumn
          state.groupName
          (translate (label :: _ "admin_groups_groupName") state.translator)
          (translate (label :: _ "admin_groups_enterGroupName") state.translator)
          "bi-people"
          HP.InputText
          ChangeGroupName
      , HH.div [ HP.classes [ HB.mb4 ] ]
          [ HH.label [ HP.classes [ HB.formLabel ] ]
              [ HH.text $ translate (label :: _ "admin_groups_desc") state.translator
              ]
          , HH.div [ HP.classes [ HB.inputGroup ] ]
              [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                  [ HH.i [ HP.classes [ H.ClassName "bi-card-text" ] ] [] ]
              , HH.textarea
                  [ HP.classes [ HB.formControl ]
                  , HP.placeholder $ translate
                      (label :: _ "admin_groups_enterGroupDesc")
                      state.translator
                  , HP.value state.groupDescription
                  , HP.rows 3
                  , HE.onValueInput ChangeGroupDescription
                  ]
              ]
          ]
      -- Members section
      , renderMembersSection state
      -- Buttons
      , HH.div [ HP.classes [ HB.dFlex, HB.justifyContentBetween, HB.mt4 ] ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnSecondary ]
              , HE.onClick $ const Cancel
              , HP.disabled state.waiting
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-arrow-left", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "common_cancel") state.translator
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit
              , HP.classes [ HB.btn, HB.btnPrimary ]
              , HP.disabled $ String.null state.groupName || state.waiting
              ]
              [ if state.waiting then HH.span
                  [ HP.classes [ HB.spinnerBorderSm, HB.me2 ] ]
                  []
                else HH.i [ HP.classes [ H.ClassName "bi-plus-circle", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "common_create") state.translator
              ]
          ]
      ]

  -- | The members section: a card containing the user search bar with its
  -- | dropdown popover of addable users, followed by the list of already-added members.
  renderMembersSection :: State -> H.ComponentHTML Action () m
  renderMembersSection state =
    let
      nonCreatorMembers = filter (not <<< _.isCreator) state.addedMembers
      memberCount = length nonCreatorMembers
    in
      HH.div [ HP.classes [ HB.card, HB.mb4 ] ]
        [ HH.div
            [ HP.classes
                [ HB.cardHeader
                , HB.dFlex
                , HB.justifyContentBetween
                , HB.alignItemsCenter
                ]
            ]
            [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                [ HH.text $ translate (label :: _ "common_members") state.translator
                ]
            , if memberCount > 0 then
                HH.span [ HP.classes [ HB.badge, HB.bgPrimary ] ]
                  [ HH.text $ show memberCount ]
              else HH.text ""
            ]
        , HH.div [ HP.classes [ HB.cardBody ] ]
            [ -- Search bar with anchored dropdown popover
              renderUserSearch state
            -- Added members list
            , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush, HB.mt3 ] ]
                $ map (renderMemberEntry state) state.addedMembers
            ]
        ]

  -- | The search bar and its anchored dropdown popover of addable users.
  -- | The dropdown is only visible when the search input has focus.
  renderUserSearch :: State -> H.ComponentHTML Action () m
  renderUserSearch state =
    HH.div [ HP.style "z-index: 1050;" ]
      [ HH.div [ HP.classes [ HB.inputGroup ] ]
          [ HH.span [ HP.classes [ HB.inputGroupText ] ]
              [ HH.i [ HP.classes [ H.ClassName "bi-search" ] ] [] ]
          , HH.div
              [ HP.style "position: relative; flex: 1 1 auto; min-width: 0;" ]
              [ HH.input
                  [ HP.type_ HP.InputText
                  , HP.classes [ HB.formControl ]
                  , HP.style
                      "border-top-left-radius: 0; border-bottom-left-radius: 0;"
                  , HP.placeholder $ translate
                      (label :: _ "admin_groups_searchUsersToAdd")
                      state.translator
                  , HP.value state.userSearchFilter
                  , HE.onValueInput SearchUsers
                  , HE.onFocus $ const FocusUserSearch
                  , HE.onKeyDown SearchKeyDown
                  ]
              , if state.searchFocused then renderUserDropdown state
                else HH.text ""
              ]
          ]
      ]

  -- | The dropdown popover anchored below the search bar.
  -- | Only rendered when `searchFocused` is true.
  -- | Shows a loading spinner, an empty state, or the list of addable users.
  renderUserDropdown :: State -> H.ComponentHTML Action () m
  renderUserDropdown state =
    let
      available = availableUsers state
    in
      HH.div
        [ HP.classes [ HB.border, HB.rounded, HB.bgWhite ]
        , HP.style
            "position: absolute; top: 100%; width: 100%; z-index: 1051; margin-top: 2px; max-height: 220px; overflow-y: auto; box-shadow: 0 0.25rem 0.5rem rgba(0,0,0,.1);"
        ]
        [ if state.loadingUsers then
            HH.div [ HP.classes [ HB.textCenter, HB.py3 ] ]
              [ HH.div
                  [ HP.classes
                      [ HB.spinnerBorder, HB.spinnerBorderSm, HB.textPrimary ]
                  ]
                  []
              ]
          else if null available then
            HH.div [ HP.classes [ HB.textCenter, HB.py3, HB.textMuted, HB.small ] ]
              [ HH.text $ translate
                  (label :: _ "admin_groups_noUsersFound")
                  state.translator
              ]
          else
            HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
              $ mapWithHighlight available
        ]

  -- | Renders the dropdown entries, highlighting the first one as the default
  -- | selection (added on Enter).
  mapWithHighlight :: Array UserOverviewDto -> Array (H.ComponentHTML Action () m)
  mapWithHighlight = mapWithIndex (\i -> renderDropdownEntry (i == 0))

  -- | A single clickable user entry in the dropdown. Uses onMouseDown so the
  -- | action fires before an input blur event could close the popover.
  renderDropdownEntry :: Boolean -> UserOverviewDto -> H.ComponentHTML Action () m
  renderDropdownEntry isHighlighted user =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          , HB.py2
          ]
      , HP.style $ "cursor: pointer;"
          <> if isHighlighted then " background-color: #e9ecef;" else ""
      , HE.onMouseDown $ const $ AddUser user
      ]
      [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn ] ]
          [ HH.span [] [ HH.text $ UOD.getName user ]
          , HH.small [ HP.classes [ HB.textMuted ] ]
              [ HH.text $ UOD.getEmail user ]
          ]
      , HH.i [ HP.classes [ H.ClassName "bi-plus-circle", HB.textPrimary ] ] []
      ]

  -- | Renders a single added member entry. The creator gets a minimal display
  -- | (name + "You · Admin" label, no buttons). Other members get role toggle + remove.
  renderMemberEntry :: State -> AddedMember -> H.ComponentHTML Action () m
  renderMemberEntry state member =
    if member.isCreator then
      HH.li
        [ HP.classes
            [ HB.listGroupItem
            , HB.dFlex
            , HB.justifyContentBetween
            , HB.alignItemsCenter
            ]
        ]
        [ HH.span [ HP.classes [ HB.fwBold ] ]
            [ HH.text member.userName ]
        , HH.small [ HP.classes [ HB.textMuted ] ]
            [ HH.text $
                translate (label :: _ "prof_you") state.translator
                  <> " · Admin"
            ]
        ]
    else
      HH.li
        [ HP.classes
            [ HB.listGroupItem
            , HB.dFlex
            , HB.justifyContentBetween
            , HB.alignItemsCenter
            ]
        ]
        [ HH.div [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
            [ HH.span [ HP.classes [ HB.fwBold ] ]
                [ HH.text member.userName ]
            , HH.small [ HP.classes [ HB.textMuted ] ]
                [ HH.text member.userEmail ]
            ]
        , HH.div [ HP.classes [ HB.dFlex, HB.gap2, HB.alignItemsCenter ] ]
            [ renderRoleToggle state member
            , HH.button
                [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
                , HE.onClick $ const $ RemoveMember member.userId
                , Style.popover $ translate (label :: _ "gm_removeMember")
                    state.translator
                ]
                [ HH.i [ HP.classes [ H.ClassName "bi-x-lg" ] ] [] ]
            ]
        ]

  -- | Renders the Admin / Member toggle button group for a member.
  renderRoleToggle :: State -> AddedMember -> H.ComponentHTML Action () m
  renderRoleToggle state member =
    HH.div [ HP.classes [ HB.btnGroup, HB.btnGroupSm ] ]
      [ HH.button
          [ HP.type_ HP.ButtonButton
          , HP.classes $ [ HB.btn ] <>
              if member.role == Admin then [ HB.btnPrimary ]
              else [ HB.btnOutlineSecondary ]
          , HE.onClick $
              if member.role /= Admin then const $ SetMemberRole member Admin
              else const DoNothing
          ]
          [ HH.text "Admin" ]
      , HH.button
          [ HP.type_ HP.ButtonButton
          , HP.classes $ [ HB.btn ] <>
              if member.role == Member then [ HB.btnPrimary ]
              else [ HB.btnOutlineSecondary ]
          , HE.onClick $
              if member.role /= Member then const $ SetMemberRole member Member
              else const DoNothing
          ]
          [ HH.text $ translate (label :: _ "common_member") state.translator ]
      ]

  -- | Computes the currently-visible list of addable users: all users that are
  -- | not yet added as members, filtered by the current search query.
  availableUsers :: State -> Array UserOverviewDto
  availableUsers state =
    let
      memberIds = map _.userId state.addedMembers
      notAdded = filter (\u -> UOD.getID u `notElem` memberIds) state.allUsers
    in
      if String.null state.userSearchFilter then notAdded
      else
        let
          q = toLower state.userSearchFilter
        in
          filter
            ( \u ->
                contains (Pattern q) (toLower $ UOD.getName u)
                  || contains (Pattern q) (toLower $ UOD.getEmail u)
            )
            notAdded

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      userResult <- getUser
      case userResult of
        -- Don't navigate to Page404 here – handleAppError already redirects
        -- to the login page for auth errors (401).  Navigating unconditionally
        -- would race with the auth redirect and corrupt the ?redirect= param.
        Left _ -> pure unit
        Right user -> do
          when (not $ isAdmin user) $ navigate Unauthorized
          let
            creatorMember =
              { userId: getUserID user
              , userName: getUserName user
              , userEmail: getUserEmail user
              , role: Admin
              , isCreator: true
              }
          H.modify_ _
            { currentUser = Just user
            , addedMembers = [ creatorMember ]
            }

      -- Load all users for search
      H.modify_ _ { loadingUsers = true }
      usersResult <- getUsers
      case usersResult of
        Left _ -> H.modify_ _ { loadingUsers = false }
        Right users -> H.modify_ _ { allUsers = users, loadingUsers = false }

    Receive { context } ->
      H.modify_ _ { translator = fromFpoTranslator context }

    DoNothing -> pure unit

    ChangeGroupName name ->
      H.modify_ _ { groupName = name }

    ChangeGroupDescription desc ->
      H.modify_ _ { groupDescription = desc }

    -- User search --

    SearchUsers searchText ->
      H.modify_ _ { userSearchFilter = searchText, searchFocused = true }

    FocusUserSearch ->
      H.modify_ _ { searchFocused = true }

    CloseUserSearch ->
      H.modify_ _ { searchFocused = false, userSearchFilter = "" }

    SearchKeyDown event
      | KE.key event == "Enter" -> do
          state <- H.get
          when state.searchFocused $ do
            H.modify_ _
              { searchSubmitGuard = true
              , searchFocused = false
              , userSearchFilter = ""
              }
            case head (availableUsers state) of
              Just user -> do
                let
                  newMember =
                    { userId: UOD.getID user
                    , userName: UOD.getName user
                    , userEmail: UOD.getEmail user
                    , role: Member
                    , isCreator: false
                    }
                H.modify_ \s -> s
                  { addedMembers = s.addedMembers <> [ newMember ] }
              Nothing -> pure unit
      | KE.key event == "Escape" ->
          H.modify_ _ { searchFocused = false, userSearchFilter = "" }
      | otherwise -> pure unit

    AddUser user -> do
      let
        newMember =
          { userId: UOD.getID user
          , userName: UOD.getName user
          , userEmail: UOD.getEmail user
          , role: Member
          , isCreator: false
          }
      H.modify_ \s -> s
        { addedMembers = s.addedMembers <> [ newMember ]
        , userSearchFilter = ""
        , searchFocused = false
        }

    -- Member list actions --

    SetMemberRole member role ->
      H.modify_ \s -> s
        { addedMembers = map
            ( \m ->
                if m.userId == member.userId && not m.isCreator then m
                  { role = role }
                else m
            )
            s.addedMembers
        }

    RemoveMember userId ->
      H.modify_ \s -> s
        { addedMembers = filter
            (\m -> m.userId /= userId || m.isCreator)
            s.addedMembers
        }

    -- Form submission --

    SubmitCreateGroup event -> do
      H.liftEffect $ preventDefault event
      state <- H.get
      -- If Enter was pressed inside the search popover, SearchKeyDown already
      -- handled it. Consume the guard and skip form submission.
      if state.searchSubmitGuard then
        H.modify_ _ { searchSubmitGuard = false }
      else if String.null state.groupName then
        updateStore $ Store.AddWarning
          (translate (label :: _ "admin_groups_notEmpty") state.translator)
      else do
        H.modify_ _ { waiting = true }

        let
          nonCreatorMembers = filter (not <<< _.isCreator) state.addedMembers
          userIds = map _.userId nonCreatorMembers
          adminMembers = filter (\m -> m.role == Admin) nonCreatorMembers

        response <- addGroup $ GroupCreate
          { groupCreateName: state.groupName
          , groupCreateDescription: state.groupDescription
          , groupCreateUsers: userIds
          }

        case response of
          Left _ -> do
            H.modify_ _ { waiting = false }
          Right groupID -> do
            _ <- traverse (\m -> changeRole groupID m.userId Admin) adminMembers
            updateStore $ Store.AddSuccess
              ( translate (label :: _ "admin_groups_successfullyCreatedGroup")
                  state.translator
              )
            navigate AdminGroups

    Cancel -> navigate AdminGroups
