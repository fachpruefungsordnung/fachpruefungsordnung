-- | Card containing a list of users. The users list is paginated and can be
-- | filtered by username and email. Furthermore, it allows the refetching of
-- | users (e.g., after a user has been created or deleted).
-- |
-- | Refer to the `Input`, `Query` and `Output` types for interface documentation.

module FPO.Components.UI.UserList where

import Prelude

import Data.Argonaut (decodeJson)
import Data.Array (filter, length, replicate, slice)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Pagination as P
import FPO.Components.UI.UserFilter as Filter
import FPO.Data.Request as R
import FPO.Data.Store as Store
import FPO.Dto.UserOverviewDto (UserOverviewDto)
import FPO.Dto.UserOverviewDto as UserOverviewDto
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addCard, emptyEntryGen)
import FPO.UI.Style as Style
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Type.Proxy (Proxy(..))

_pagination = Proxy :: Proxy "pagination"

type State ba = FPOState
  ( users :: Array UserOverviewDto
  , filteredUsers :: Array UserOverviewDto
  , page :: Int
  , loading :: Boolean
  , buttonStyles :: Input ba
  )

type ButtonStyle ba =
  { popover :: String
  , effect :: ba
  , icon :: String
  , classes :: Array HH.ClassName
  , disabled :: Boolean
  }

-- | For the input, the parent component can pass an array of
-- | `ButtonStyle` to set the button of the user list.
-- | This way, for each user in the list, buttons with specific
-- | styles and effects can be rendered. The effect is then, if
-- | the button is pressed, passed to the parent component
-- | (together with the associated user dto) via the `Output` type,
-- | and the parent component can handle the specific button effect
-- | (e.g., deleting the user, navigating to the profile page, etc.).
-- |
-- | The effect is polymorphic, i.e., it's best practice to use some
-- | kind of algebraic data type (enum) to represent the possible effects
-- | in the parent component, then do exhaustive pattern matching
-- | on the effect and define the business logic accordingly.
-- |
-- | Furthermore, the button style is parametrized over the user dto (i.e.,
-- | parametrized over each user in the list), so that the parent component
-- | can define the button styles based on the user dto. This allows the parent
-- | to, for example, disable buttons for certain users, or change the button
-- | style based on the user dto.
-- |
-- | TODO: It would be nice to not only be able to pass an array of
-- |       `ButtonStyle`, but simply pass a function that takes a `UserOverviewDto`
-- |       and returns a div or similar, i.e., a function that can be used to
-- |       generate whatever HTML is needed for the user list. Then, it would also
-- |       be nice to parametrize this component over the Action type of the
-- |       parent component, so that the parent component can define the action
-- |       type that is returned when a button is pressed and handle it directly.
-- |       This would've been a much nicer approach because the parent component
-- |       could then define all possible actions and simply use `handleAction`
-- |       to handle the button presses, with no need to define a specific data type
-- |       to represent the button effects.
type Input ba = UserOverviewDto -> Array (ButtonStyle ba)

type Slots =
  ( pagination :: H.Slot P.Query P.Output Unit
  )

data Action ba
  = Receive (Connected FPOTranslator (Input ba))
  | HandleFilter (Filter.Output)
  | SetPage P.Output
  | Initialize
  | HandleButtonPressed UserOverviewDto ba

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

data Output ba
  -- | Output to indicate to the parent whether or not the user list is
  -- | ready/loaded. This can be used to disable UI elements that depend on the
  -- | user list, e.g., the "Create User" button, if wished.
  = Loading Boolean
  -- | Output to indicate an error, e.g., when the user list could not be loaded.
  | Error String
  | ButtonPressed UserOverviewDto ba

component
  :: forall m ba
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component Query (Input ba) (Output ba) m
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
  initialState :: Connected FPOTranslator (Input ba) -> (State ba)
  initialState { context, input } =
    { users: []
    , filteredUsers: []
    , translator: fromFpoTranslator context
    , page: 0
    , loading: true
    , buttonStyles: input
    }

  render :: (State ba) -> H.ComponentHTML (Action ba) Slots m
  render state =
    addCard (translate (label :: _ "admin_users_listOfUsers") state.translator)
      [ HP.classes [ HB.col12, HB.colMd6, HB.colLg6, HB.mb3 ] ] $ HH.div_
      if state.loading then
        [ HH.div [ HP.classes [ HB.textCenter, HB.mt5 ] ]
            [ HH.div [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
        ]
      else
        [ HH.ul [ HP.classes [ HB.listGroup ] ]
            $ map (createUserEntry state) usrs
                <> replicate (10 - length usrs)
                  emptyUserEntry
        , HH.slot _pagination unit P.component ps SetPage
        ]
    where
    usrs = slice (state.page * 10) ((state.page + 1) * 10) state.filteredUsers
    ps =
      { pages: P.calculatePageCount (length state.filteredUsers) 10
      , style: P.Compact 1
      , reaction: P.PreservePage
      }

  -- Creates a user entry for the list.
  createUserEntry
    :: forall w. State ba -> UserOverviewDto -> HH.HTML w (Action ba)
  createUserEntry state userOverviewDto =
    HH.li [ HP.classes [ HB.listGroupItem, HB.p2 ] ]
      $
        [ HH.div
            [ HP.classes
                [ HB.dFlex
                , HB.alignItemsCenter
                , HB.flexNowrap
                ]
            ]
            $
              [ HH.div
                  [ HP.classes
                      [ HB.pe3
                      , HB.textTruncate
                      , Style.truncateHoverTitle
                      ]
                  , HP.style "width: 160px; min-width: 160px; max-width: 160px;"
                  ]
                  [ HH.text $ UserOverviewDto.getName userOverviewDto
                  ]
              , HH.div
                  [ HP.classes
                      [ HB.pe3
                      , HB.textTruncate
                      , Style.truncateHoverTitle
                      , HB.flexGrow1
                      ]
                  , HP.style "min-width: 60px;"
                  ]
                  [ HH.text $ UserOverviewDto.getEmail userOverviewDto
                  ]
              , HH.div [ HP.classes [ HB.dFlex, HB.gap2, HB.flexShrink0, HB.ps2 ] ]
                  ( map (addButton userOverviewDto)
                      (state.buttonStyles userOverviewDto)
                  )
              ]
        ]
    where
    addButton
      :: UserOverviewDto
      -> ButtonStyle ba
      -> HH.HTML w (Action ba)
    addButton dto style =
      HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes style.classes
        , HE.onClick $ const $ HandleButtonPressed dto style.effect
        , Style.popover $ style.popover
        , HP.disabled style.disabled
        ]
        [ HH.i [ HP.classes [ HB.bi, H.ClassName style.icon ] ] [] ]

  -- | Empty entry for padding.
  emptyUserEntry :: forall w. HH.HTML w (Action ba)
  emptyUserEntry = emptyEntryGen
    [ HH.button
        [ HP.type_ HP.ButtonButton
        ]
        [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-eye-slash-fill" ] ] [] ]
    ]

  handleAction
    :: (Action ba)
    -> H.HalogenM (State ba) (Action ba) Slots (Output ba) m Unit
  handleAction action = case action of
    Initialize -> do
      setLoading true
      fetchAndLoadUsers
      setLoading false
    HandleFilter (Filter.Filter f) -> do
      state <- H.get
      let
        filteredUsers =
          filter
            ( \user ->
                let
                  nameMatches =
                    not (String.null f.username) &&
                      contains (Pattern f.username) (UserOverviewDto.getName user)

                  emailMatches =
                    not (String.null f.email) &&
                      contains (Pattern f.email) (UserOverviewDto.getEmail user)
                in
                  if String.null f.username && String.null f.email then
                    true
                  else
                    nameMatches || emailMatches
            )
            state.users
      H.modify_ _ { filteredUsers = filteredUsers }
    Receive { context, input } -> do
      H.modify_ _ { translator = fromFpoTranslator context, buttonStyles = input }
    SetPage (P.Clicked p) -> do
      H.modify_ _ { page = p }
    HandleButtonPressed userOverviewDto effect -> do
      H.raise $ ButtonPressed userOverviewDto effect

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM (State ba) (Action ba) Slots (Output ba) m (Maybe a)
  handleQuery = case _ of
    ReloadUsersQ a -> do
      fetchAndLoadUsers
      pure $ Just a
    HandleFilterQ f a -> do
      handleAction (HandleFilter f)
      pure $ Just a

  fetchAndLoadUsers
    :: H.HalogenM (State ba) (Action ba) Slots (Output ba) m Unit
  fetchAndLoadUsers = do
    setLoading true
    maybeUsers <- H.liftAff $ R.getFromJSONEndpoint decodeJson "/users"
    case maybeUsers of
      Nothing -> do
        state <- H.get
        H.raise $ Error $ translate (label :: _ "admin_users_failedToLoadUsers")
          state.translator
      Just users -> do
        H.modify_ _ { users = users, filteredUsers = users }

    setLoading false

  setLoading
    :: MonadAff m
    => Boolean
    -> H.HalogenM (State ba) (Action ba) Slots (Output ba) m Unit
  setLoading b = do
    H.modify_ _ { loading = b }
    H.raise $ Loading b
