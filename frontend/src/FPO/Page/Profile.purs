-- | The profile page is used to display user information or settings.

module FPO.Page.Profile (component) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (LoadState(..), getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.UserDto (User)
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
import Type.Proxy (Proxy(Proxy))

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)

-- | TODO: Because `handleAction` handles the case of failing to load the user
-- |       explicitly (by navigating to the login page), we do not care about
-- |       the error case here. We should look for a more general approach to
-- |       handle loading entities in the future. Right now, this is at least
-- |       more descriptive than using `Maybe`.
type State = FPOState
  ( user :: LoadState User
  , loginSuccessfulBanner :: Boolean
  )

type Input = { loginSuccessfulBanner :: Maybe Boolean }

-- | User profile page component.
component
  :: forall query output m
   . Navigate m
  => MonadAff m
  => MonadStore Store.Action Store.Store m
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
  initialState { context, input: { loginSuccessfulBanner } } =
    { user: Loading
    , loginSuccessfulBanner: fromMaybe false loginSuccessfulBanner
    , translator: fromFpoTranslator context
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ HH.div [ HP.classes [ HB.col, HB.textCenter ] ]
          [ HH.h1 []
              [ HH.text $ translate (label :: _ "prof_profile") state.translator ]
          , case state.user of
              Loaded user -> HH.div
                [ HP.classes [ HB.dFlex, HB.justifyContentCenter, HB.my5 ] ]
                [ HH.div [ HP.classes [ HB.colMd3 ] ]
                    [ HH.div [ HP.classes [ HB.textCenter, HB.mt3 ] ]
                        [ case state.loginSuccessfulBanner of
                            true -> HH.div
                              [ HP.classes [ HB.alert, HB.alertSuccess ] ]
                              [ HH.text $ translate
                                  (Proxy :: _ "prof_loginSuccessful")
                                  state.translator
                              ]
                            false -> HH.text ""
                        ]
                    , HH.div [ HP.classes [ HB.card ] ]
                        [ HH.div [ HP.classes [ HB.cardHeader ] ]
                            [ HH.text $ translate (label :: _ "prof_userData")
                                state.translator
                            ]
                        , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                            [ HH.li [ HP.classes [ HB.listGroupItem ] ]
                                [ HH.strong_
                                    [ HH.text $
                                        ( translate (label :: _ "common_userName")
                                            state.translator
                                        ) <> ": "
                                    ]
                                , HH.text user.userName
                                ]
                            , HH.li [ HP.classes [ HB.listGroupItem ] ]
                                [ HH.strong_
                                    [ HH.text $
                                        ( translate (label :: _ "prof_role")
                                            state.translator
                                        ) <> ": "
                                    ]
                                , HH.span
                                    [ HP.classes
                                        [ HB.badge
                                        , if user.isAdmin then HB.bgPrimary
                                          else HB.bgSecondary
                                        ]
                                    ]
                                    [ HH.text $
                                        if user.isAdmin then "Administrator"
                                        else "Member"
                                    ]
                                ]
                            ]
                        ]

                    ]
                ]
              Loading -> HH.div [ HP.classes [ HB.my3 ] ]
                [ HH.div [ HP.classes [ HB.spinnerBorder ] ] [] ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff getUser
      case u of
        Just us -> do
          H.modify_ \currState -> currState { user = Loaded us }
          pure unit
        Nothing -> do
          navigate Login
          pure unit
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
