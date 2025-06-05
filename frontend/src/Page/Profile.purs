-- | The profile page is used to display user information or settings.

module FPO.Page.Profile (component) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store (User)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (translate)
import Type.Proxy (Proxy(Proxy))

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)

type State = FPOState (user :: Maybe User, loginSuccessfulBanner :: Boolean)

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
    { user: Nothing
    , loginSuccessfulBanner: fromMaybe false loginSuccessfulBanner
    , translator: fromFpoTranslator context
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ HH.div [ HP.classes [ HB.col, HB.textCenter ] ]
          [ HH.h1 []
              [ HH.text $ translate (Proxy :: _ "prof_profile") state.translator ]
          , case state.user of
              Just user -> HH.div
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
                            [ HH.text $ translate (Proxy :: _ "prof_userData")
                                state.translator
                            ]
                        , HH.ul [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                            [ HH.li [ HP.classes [ HB.listGroupItem ] ]
                                [ HH.strong_
                                    [ HH.text $
                                        ( translate (Proxy :: _ "prof_userName")
                                            state.translator
                                        ) <> ": "
                                    ]
                                , HH.text user.userName
                                ]
                            , HH.li [ HP.classes [ HB.listGroupItem ] ]
                                [ HH.strong_
                                    [ HH.text $
                                        ( translate (Proxy :: _ "prof_role")
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
              -- TODO: This case should not happen because on initialization, we leave this component
              --       if the user is not logged in. We should somehow parametrize the component
              --       over the user, so that it can only be used when a user is actually logged in,
              --       or look for another approach to handle this.
              --
              --       For now, we just handle this case gracefully.
              Nothing -> HH.div [ HP.classes [ HB.my3 ] ]
                [ HH.i [] [ HH.text "unknown user" ]
                ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      u <- liftAff getUser
      case u of
        Just us -> do
          H.modify_ \currState -> currState { user = Just us }
          pure unit
        Nothing -> do
          navigate Login
          pure unit
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
