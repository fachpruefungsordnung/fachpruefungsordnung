-- | Card containing a list of users. The users list is paginated and can be
-- | filtered by username and email. Furthermore, it allows the refetching of
-- | users (e.g., after a user has been created or deleted).
-- |
-- | Refer to the `Query` and `Output` types for interface documentation.

module FPO.Components.UI.UserList where

import Prelude

import Data.Argonaut (decodeJson)
import Data.Array (filter, length, null, replicate, slice)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import FPO.Components.Pagination as P
import FPO.Components.Pagination as P
import FPO.Components.UI.UserFilter as Filter
import FPO.Data.Request as R
import FPO.Data.Store as Store
import FPO.Dto.UserOverviewDto (UserOverviewDto(..))
import FPO.Dto.UserOverviewDto as UserOverviewDto
import FPO.Translations.Labels (Labels)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addButton, addCard, addColumn, deleteButton)
import FPO.UI.HTML (emptyEntryText)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (InputType(..), classes) as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 (p4)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type State = FPOState
  ( users :: Array UserOverviewDto
  , filteredUsers :: Array UserOverviewDto
  , page :: Int
  , loading :: Boolean
  )

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

data Action
  = Receive (Connected FPOTranslator Unit)
  | HandleFilter (Filter.Output)
  | SetPage P.Output
  | Initialize

data Query a
  -- | Query to reload the users list, for example,
  -- | after the parent component has changed the
  -- | user list using backend requests.
  = ReloadUsersQ a
  -- | Query to handle filter changes.
  -- | The filter is passed as an argument, usually by
  -- |   1. The parent component that received the filter change
  -- |      from the `UserFilter` child.
  -- |   2. The parent component that synthesizes the filter
  -- |      itself.
  | HandleFilterQ (Filter.Output) a

data Output
  -- | Output to indicate to the parent whether or not the user list is
  -- | ready/loaded. This can be used to disable UI elements that depend on the
  -- | user list, e.g., the "Create User" button, if wished.
  = Loading Boolean
  -- | Output to indicate an error, e.g., when the user list could not be loaded.
  | Error String

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Unit Output m
component = connect selectTranslator $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }
  where
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { users: []
    , filteredUsers: []
    , translator: fromFpoTranslator context
    , page: 0
    , loading: true
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    addCard (translate (label :: _ "admin_users_listOfUsers") state.translator)
      [ HP.classes [ HB.col12, HB.colMd6, HB.colLg6, HB.mb3 ] ] $ HH.div_
      if state.loading then
        [ HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
            [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
        ]
      else
        [ HH.ul [ HP.classes [ HB.listGroup ] ]
            $ map (createUserEntry state.translator) usrs
                <> replicate (10 - length usrs)
                  emptyEntryText
        , HH.slot _pagination unit P.component ps SetPage
        ]
    where
    usrs = slice (state.page * 10) ((state.page + 1) * 10) state.filteredUsers
    ps =
      { pages: P.calculatePageCount (length state.filteredUsers) 10
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

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
      -- , HH.span [ HP.classes [ HB.col5 ] ]
      --     [ HH.text $ UserOverviewDto.getEmail userOverviewDto ]
      -- , HH.div [ HP.classes [ HB.col2, HB.dFlex, HB.justifyContentEnd, HB.gap1 ] ]
      --     [ deleteButton (const $ RequestDeleteUser userOverviewDto)
      --     , HH.button
      --         [ HP.type_ HP.ButtonButton
      --         , HP.classes [ HB.btn, HB.btnSm, HB.btnOutlinePrimary ]
      --         , HE.onClick $ const $ GetUser (UserOverviewDto.getID userOverviewDto)
      --         , HP.title
      --             (translate (label :: _ "admin_users_goToProfilePage") translator)
      --         ]
      --         [ HH.i [ HP.classes [ HB.bi, (H.ClassName "bi-person-fill") ] ] []

      --         ]
      --     ]
      ]

  handleAction
    :: Action
    -> H.HalogenM State Action Slots Output m Unit
  handleAction action = case action of
    Initialize -> do
      setLoading true
      fetchAndLoadUsers
      setLoading false
    HandleFilter (Filter.Filter f) -> do
      state <- H.get
      let
        filteredUsers = filter
          ( \user ->
              ( if String.null f.username then false
                else
                  contains (Pattern f.username)
                    (UserOverviewDto.getName user)
              )
                ||
                  ( if String.null f.email then false
                    else
                      contains
                        (Pattern f.email)
                        (UserOverviewDto.getEmail user)
                  )
          )
          state.users
      H.modify_ _ { filteredUsers = filteredUsers }
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
  handleQuery = case _ of
    ReloadUsersQ a -> do
      liftEffect $ log "ReloadUsersQ invoked..."
      fetchAndLoadUsers
      pure $ Just a
    HandleFilterQ f a -> do
      handleAction (HandleFilter f)
      pure $ Just a

  fetchAndLoadUsers
    :: H.HalogenM State Action Slots Output m Unit
  fetchAndLoadUsers = do
    liftEffect $ log "Fetching user data, Loading=true..."
    setLoading true
    maybeUsers <- H.liftAff $ R.getFromJSONEndpoint decodeJson "/users"
    case maybeUsers of
      Nothing -> do
        state <- H.get
        H.raise $ Error $ translate (label :: _ "admin_users_failedToLoadUsers")
          state.translator
      Just users -> do
        H.modify_ _ { users = users, filteredUsers = users }

    liftEffect $ log "Done fetching, Loading=false!"
    setLoading false

  setLoading
    :: MonadAff m
    => Boolean
    -> H.HalogenM State Action Slots Output m Unit
  setLoading b = do
    H.modify_ _ { loading = b }
    H.raise $ Loading b
