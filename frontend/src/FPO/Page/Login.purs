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
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (postString) as Request
import FPO.Data.Route (Route(..))
import FPO.Data.Store (preventErrorHandlingLocally)
import FPO.Data.Store as Store
import FPO.Dto.Login (LoginDto)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

foreign import selectElement :: forall a. a -> Effect Unit

type Input = Unit

data Action
  = Initialize
  | NavigateToPasswordReset
  | UpdateEmail String
  | UpdatePassword String
  | DoLogin LoginDto Event
  | Receive (Connected FPOTranslator Input)
  | ToggleShowPassword

toLoginDto :: State -> LoginDto
toLoginDto state = { loginEmail: state.email, loginPassword: state.password }

type State = FPOState
  ( email :: String
  , password :: String
  , loginFailed :: Boolean
  , showPassword :: Boolean
  )

passwordRef :: H.RefLabel
passwordRef = H.RefLabel "password-input"

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
        , loginFailed: false
        , showPassword: false
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
          , loginFailed: false
          , showPassword: false
          , translator: fromFpoTranslator store.translator
          }
      H.put initialState
    UpdateEmail email -> do
      H.modify_ \state -> state { email = email, loginFailed = false }
      -- In this example, we are simply storing the user's email in our
      -- store, every time the user clicks on the button (either login or
      -- register).
      updateStore $ Store.SetMail email
    UpdatePassword password -> do
      state <- H.get
      -- If the user edits the password after a failed login attempt,
      -- select all text so they can easily retype.
      when state.loginFailed $ selectPasswordField
      H.modify_ \s -> s { password = password, loginFailed = false }
    NavigateToPasswordReset -> do
      navigate (PasswordReset { token: Nothing })
    DoLogin loginDto event -> do
      H.liftEffect $ preventDefault event
      -- trying to do a login by calling the api at /api/login
      -- we show the error response that comes back from the backend
      loginResponse <- preventErrorHandlingLocally $
        Request.postString "/login" (encodeJson loginDto)
      case loginResponse of
        Left _ -> do
          H.modify_ _ { loginFailed = true }
          selectPasswordField
        Right _ -> handleLoginRedirect
      pure unit
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }
    ToggleShowPassword -> do
      H.modify_ \state -> state { showPassword = not state.showPassword }

  selectPasswordField :: H.HalogenM State Action () output m Unit
  selectPasswordField = do
    mEl <- H.getRef passwordRef
    case mEl of
      Nothing -> pure unit
      Just el -> H.liftEffect $ selectElement el

  -- After successful login, redirect to either a previously set redirect route
  -- or to the project overview page (home).
  handleLoginRedirect :: H.HalogenM State Action () output m Unit
  handleLoginRedirect = do
    redirect <- _.loginRedirect <$> getStore
    case redirect of
      Just r -> do
        updateStore $ Store.SetLoginRedirect Nothing
        navigate r
      Nothing -> do
        navigate Home

  renderLoginForm :: forall w. State -> HH.HTML w Action
  renderLoginForm state =
    let
      errorMsg = translate (label :: _ "error_invalidCredentials") state.translator
      invalidClass = HH.ClassName "is-invalid"
      emailClasses =
        [ HB.formControl ]
          <> if state.loginFailed then [ invalidClass ] else []
      passwordClasses =
        [ HB.formControl ]
          <> if state.loginFailed then [ invalidClass ] else []
      passwordType =
        if state.showPassword then HP.InputText
        else HP.InputPassword
      eyeIcon =
        if state.showPassword then "bi-eye-slash-fill"
        else "bi-eye-fill"
      loginFailedVisibility = HP.style
        if state.loginFailed then ""
        else "visibility: hidden;"
    in
      HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
        [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
            [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
                [ HH.text "Login" ]
            , HH.form
                [ HE.onSubmit \e -> DoLogin (toLoginDto state) e ]
                [ HH.div [ HP.classes [ HB.mb2 ] ]
                    [ HH.label [ HP.classes [ HB.formLabel ] ]
                        [ HH.text $
                            ( translate (label :: _ "common_emailAddress")
                                state.translator
                            ) <> ":"
                        ]
                    , HH.div [ HP.classes [ HB.inputGroup ] ]
                        [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                            [ HH.i [ HP.class_ (HH.ClassName "bi-envelope-fill") ] []
                            ]
                        , HH.input
                            [ HP.type_ HP.InputEmail
                            , HP.classes emailClasses
                            , HP.placeholder
                                ( translate (label :: _ "common_email")
                                    state.translator
                                )
                            , HP.value state.email
                            , HE.onValueInput UpdateEmail
                            ]
                        ]
                    , HH.div
                        [ HP.classes
                            [ HH.ClassName "invalid-feedback", HB.dBlock ]
                        , loginFailedVisibility
                        ]
                        [ HH.text errorMsg ]
                    ]
                , HH.div [ HP.classes [ HB.mb2 ] ]
                    [ HH.label [ HP.classes [ HB.formLabel ] ]
                        [ HH.text $
                            ( translate (label :: _ "common_password")
                                state.translator
                            ) <> ":"
                        ]
                    , HH.div [ HP.classes [ HB.inputGroup ] ]
                        [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                            [ HH.i [ HP.class_ (HH.ClassName "bi-lock-fill") ] [] ]
                        , HH.input
                            [ HP.type_ passwordType
                            , HP.classes passwordClasses
                            , HP.placeholder
                                ( translate (label :: _ "common_password")
                                    state.translator
                                )
                            , HP.value state.password
                            , HP.ref passwordRef
                            , HE.onValueInput UpdatePassword
                            ]
                        , HH.button
                            [ HP.type_ HP.ButtonButton
                            , HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                            , HE.onClick $ const ToggleShowPassword
                            , HP.tabIndex (-1)
                            ]
                            [ HH.i [ HP.class_ (HH.ClassName eyeIcon) ] [] ]
                        ]
                    , HH.div
                        [ HP.classes
                            [ HH.ClassName "invalid-feedback", HB.dBlock ]
                        , loginFailedVisibility
                        ]
                        [ HH.text errorMsg ]
                    ]
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
