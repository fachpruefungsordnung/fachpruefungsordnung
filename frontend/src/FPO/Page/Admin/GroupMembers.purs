-- | Admin group member overview and management page.
-- |
-- | TODO: Lots of thigns are still missing here and WIP, such as:
-- |  - Adding/Removing members to a group
-- |  - Actually communicating with the backend
-- |  - Using a table to display all members, similar to
-- |    the home page overview or document overview page
-- |
-- | Also, the layout should be aligned with the project management page,
-- | and the Members/Projects buttons should be aligned on both pages.

module FPO.Page.Admin.GroupMembers
  ( component
  ) where

import Prelude

import Data.Array (length, replicate, slice)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (LoadState(..), getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.HTML (addCard, addError, emptyEntryGen)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

type Input = Int

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | SetPage P.Output

type State = FPOState
  ( error :: Maybe String
  , page :: Int
  , members :: LoadState (Array Member)
  , filteredMembers :: Array Member
  , waiting :: Boolean
  , groupID :: Int
  )

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
    , page: 0
    , waiting: false
    , members: Loaded mockMembers
    , filteredMembers: []
    , error: Nothing
    , groupID: input
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      [ renderGroupManagement state
      , addError state.error
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff $ getUser
      when (fromMaybe true (not <$> _.isAdmin <$> u)) $
        navigate Page404
    SetPage (P.Clicked page) -> do
      H.modify_ _ { page = page }
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }

  renderGroupManagement :: State -> H.ComponentHTML Action Slots m
  renderGroupManagement state =
    HH.div_
      [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
          [ HH.text $ translate (label :: _ "au_groupManagement")
              state.translator
          ]
      , case state.members of
          Loading ->
            HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
              [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
          Loaded _ ->
            renderMemberListView state
      ]

  renderMemberListView :: State -> H.ComponentHTML Action Slots m
  renderMemberListView state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ renderMemberList state
      ]

  -- Creates a list of (dummy) members with pagination.
  -- TODO: Of course, this should instead be a table with sortable columns!
  renderMemberList :: State -> H.ComponentHTML Action Slots m
  renderMemberList state =
    HH.div [ HP.classes [ HB.colLg7, HB.colMd8, HB.colSm9, HB.mb4 ] ]
      [ addCard "Members of Group XY"
          [] $
          HH.div_
            [ addFilterColumn
            , HH.ul [ HP.classes [ HB.listGroup ] ]
                $ map (createListEntry state) ms
                    <> replicate (membersPerPage - length ms)
                      (emptyEntryGen [ buttonRemoveMember state "(not a member)" ])
            , HH.slot _pagination unit P.component ps SetPage
            ]
      ]
    where
    ms = slice (state.page * membersPerPage) ((state.page + 1) * membersPerPage)
      state.filteredMembers
    ps =
      { pages: P.calculatePageCount (length state.filteredMembers) membersPerPage
      , style: P.Compact 1
      , reaction: P.PreservePage
      }
    membersPerPage = 8

  addFilterColumn :: H.ComponentHTML Action Slots m
  addFilterColumn =
    HH.div [ HP.classes [ HB.dFlex, HB.gap1, HB.alignItemsCenter, HB.mb3 ] ]
      [ HH.div [ HP.classes [ HB.inputGroup ] ]
          [ HH.span [ HP.classes [ HB.inputGroupText ] ]
              [ HH.i [ HP.class_ $ HH.ClassName "bi-search" ] [] ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.classes [ HB.formControl ]
              , HP.placeholder "Name of member"
              ]
          ]
      , HH.div [ HP.classes [ HB.inputGroup ] ]
          [ HH.span [ HP.classes [ HB.inputGroupText ] ]
              [ HH.i [ HP.class_ $ HH.ClassName "bi-search" ] [] ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.classes [ HB.formControl ]
              , HP.placeholder "Email"
              ]
          ]
      , HH.select [ HP.classes [ HB.formSelect ], HP.style "max-width: 120px" ]
          [ HH.option [ HP.selected true ] [ HH.text "All Roles" ]
          , HH.option [ HP.value "admin" ] [ HH.text "Admin" ]
          , HH.option [ HP.value "user" ] [ HH.text "Editor" ]
          , HH.option [ HP.value "viewer" ] [ HH.text "Viewer" ]
          ]
      ]

  -- Creates a (dummy) group entry for the list.
  createListEntry :: forall w. State -> Member -> HH.HTML w Action
  createListEntry state m =
    HH.li
      [ HP.classes
          [ HB.listGroupItem
          , HB.dFlex
          , HB.justifyContentBetween
          , HB.alignItemsCenter
          ]
      ]
      [ HH.text m.name
      , buttonRemoveMember state m.name
      ]

  buttonRemoveMember :: forall w. State -> String -> HH.HTML w Action
  buttonRemoveMember state _ =
    HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
      , HP.disabled state.waiting
      ]
      [ HH.i [ HP.class_ $ HH.ClassName "person-x" ] [] ]

-- | TODO: Implement the actual member data structure, in a DTO.
-- |       This is a mock data structure for demonstration purposes.
-- |       A unique identifier (ID) for each member should be added, or
-- |       the email should be used as a unique identifier. Also, we should
-- |       define the roles as a separate data type, not string-based.
type Member =
  { name :: String
  , email :: String
  , role :: String
  }

-- Mock data for members.
mockMembers :: Array Member
mockMembers =
  [ { name: "Alice", email: "test1@test.com", role: "admin" }
  , { name: "Bob", email: "test2@test.com", role: "user" }
  , { name: "Charlie", email: "test3@test.com", role: "viewer" }
  ]
