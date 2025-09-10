-- | Simple test page for login.
-- |
-- | This page is currently not connected to any backend and does not perform any
-- | authentication.
-- |
-- | Additionally, this page shows how to use `MonadStore` to update and read data
-- | from the store.

module FPO.Page.Login (component) where

import Prelude

import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (postString) as Request
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.Login (LoginDto)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onSubmit) as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = Unit

data Action
  = Initialize
  | NavigateToPasswordReset
  | UpdateEmail String
  | UpdatePassword String
  | DoLogin LoginDto Event
  | Receive (Connected FPOTranslator Input)

toLoginDto :: State -> LoginDto
toLoginDto state = { loginEmail: state.email, loginPassword: state.password }

type State = FPOState
  ( email :: String
  , password :: String
  )

-- | Login component.
-- |
-- | Notice how we are using MonadStore to update the store with the user's
-- | email when the user clicks on the button.
component
  :: forall query output m
   . Navigate m
  => MonadStore Store.Action Store.Store m
  => MonadAff m
  => H.Component query Input output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState: \{ context } ->
        { email: ""
        , password: ""
        , translator: fromFpoTranslator context
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ renderLoginForm state
      ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      -- When opening the login tab, we simply take the user's email
      -- address from the store, provided that it exists (was
      -- previously set).
      store <- getStore
      let
        initialState =
          { email: store.inputMail
          , password: ""
          , translator: fromFpoTranslator store.translator
          }
      H.put initialState
    UpdateEmail email -> do
      H.modify_ \state -> state { email = email }
      -- In this example, we are simply storing the user's email in our
      -- store, every time the user clicks on the button (either login or
      -- register).
      updateStore $ Store.SetMail email
    UpdatePassword password -> do
      H.modify_ \state -> state { password = password }
    NavigateToPasswordReset -> do
      navigate (PasswordReset { token: Nothing })
    DoLogin loginDto event -> do
      H.liftEffect $ preventDefault event
      -- trying to do a login by calling the api at /api/login
      -- we show the error response that comes back from the backend
      loginResponse <- Request.postString "/login" (encodeJson loginDto)
      case loginResponse of
        Left _ -> pure unit
        Right _ -> handleLoginRedirect
      pure unit
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }

  -- After successful login, redirect to either a previously set redirect route
  -- or to the profile page with a login success banner.
  handleLoginRedirect :: H.HalogenM State Action () output m Unit
  handleLoginRedirect = do
    redirect <- _.loginRedirect <$> getStore
    case redirect of
      Just r -> do
        updateStore $ Store.SetLoginRedirect Nothing
        navigate r
      Nothing -> do
        navigate (Profile { loginSuccessful: Just true, userId: Nothing })

  renderLoginForm :: forall w. State -> HH.HTML w Action
  renderLoginForm state =
    HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
      [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text "Login" ]
          , HH.form
              [ HE.onSubmit \e -> DoLogin (toLoginDto state) e ]
              [ addColumn
                  state.email
                  ( (translate (label :: _ "common_emailAddress") state.translator) <>
                      ":"
                  )
                  (translate (label :: _ "common_email") state.translator)
                  "bi-envelope-fill"
                  HP.InputEmail
                  UpdateEmail
              , addColumn
                  state.password
                  ((translate (label :: _ "common_password") state.translator) <> ":")
                  (translate (label :: _ "common_password") state.translator)
                  "bi-lock-fill"
                  HP.InputPassword
                  UpdatePassword
              , HH.div [ HP.classes [ HB.mb4, HB.textCenter ] ]
                  [ HH.button
                      [ HP.classes [ HB.btn, HB.btnPrimary ] ]
                      [ HH.text "Login" ]
                  ]
              , HH.div [ HP.classes [ HB.textCenter ] ]
                  [ HH.button
                      [ HP.classes [ HB.btn, HB.btnLink ]
                      , HP.type_ HP.ButtonButton
                      , HE.onClick $ const NavigateToPasswordReset
                      ]
                      [ HH.text
                          ( translate (label :: _ "login_passwordForgotten")
                              state.translator
                          )
                      ]
                  ]
              ]
          ]
      ]
