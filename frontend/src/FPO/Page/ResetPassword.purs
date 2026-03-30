-- | Password reset page component.
-- |
-- | Two flows:
-- | 1. No token in URL → show email input, user requests a reset link.
-- | 2. Token present in URL → show password inputs, user sets a new password.

module FPO.Page.ResetPassword (component) where

import Prelude

import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (null)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppError (AppError(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (postIgnore, postIgnoreOrError)
import FPO.Data.Route (loginRoute)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import FPO.UI.Css as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | UpdateEmail String
  | UpdatePasswordPrimary String
  | UpdatePasswordSecondary String
  | SubmitRequestLink Event
  | SubmitNewPassword Event
  | ToggleShowPassword

data Phase
  = RequestLinkForm   -- no token, user enters email
  | RequestLinkSent   -- email sent, show success
  | NewPasswordForm   -- token present, user enters passwords

type State = FPOState
  ( email :: String
  , passwordPrimary :: String
  , passwordSecondary :: String
  , token :: String
  , phase :: Phase
  , submitting :: Boolean
  , showPassword :: Boolean
  )

type Input = { token :: Maybe String }

component
  :: forall query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component query Input output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState: \{ input: { token }, context } ->
        { email: ""
        , passwordPrimary: ""
        , passwordSecondary: ""
        , token: fromMaybe "" token
        , translator: fromFpoTranslator context
        , phase: if isJust token then NewPasswordForm else RequestLinkForm
        , submitting: false
        , showPassword: false
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
      [ HP.style "min-height: 60vh; display: flex; align-items: center; justify-content: center;" ]
      [ HH.div
          [ HP.style "width: 100%; max-width: 420px; padding: 0 var(--fpo-space-4);" ]
          [ case state.phase of
              RequestLinkForm -> renderRequestLinkForm state
              RequestLinkSent -> renderRequestLinkSent state
              NewPasswordForm -> renderNewPasswordForm state
          ]
      ]

  -- ─── Flow 1: Enter email to request reset link ───────────────────
  renderRequestLinkForm :: State -> H.ComponentHTML Action () m
  renderRequestLinkForm state =
    HH.div
      [ HP.style "background: var(--fpo-bg-elevated); border: 1px solid var(--fpo-border-subtle); border-radius: var(--fpo-radius-xl); box-shadow: var(--fpo-shadow-sm); padding: var(--fpo-space-6);" ]
      [ HH.h2
          [ HP.style "font-size: var(--fpo-text-xl); font-weight: 700; color: var(--fpo-text-primary); margin: 0 0 var(--fpo-space-1) 0;" ]
          [ HH.text $ translate (label :: _ "rp_Header") state.translator ]
      , HH.p
          [ HP.style "font-size: var(--fpo-text-sm); color: var(--fpo-text-tertiary); margin: 0 0 var(--fpo-space-5) 0;" ]
          [ HH.text $ translate (label :: _ "rp_requestLinkHint") state.translator ]
      , HH.form
          [ HE.onSubmit SubmitRequestLink ]
          [ HH.label
              [ HP.classes [ HB.formLabel ]
              , HP.style "font-size: var(--fpo-text-sm); font-weight: 500;"
              ]
              [ HH.text $ translate (label :: _ "common_emailAddress") state.translator ]
          , HH.div
              [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
              [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                  [ HH.i [ HP.class_ (H.ClassName "bi-envelope") ] [] ]
              , HH.input
                  [ HP.type_ HP.InputEmail
                  , HP.classes [ HB.formControl ]
                  , HP.placeholder (translate (label :: _ "common_email") state.translator)
                  , HP.value state.email
                  , HE.onValueInput UpdateEmail
                  , HP.required true
                  ]
              ]
          , HH.button
              ( [ HP.classes [ HB.btn, HB.btnPrimary, HB.w100 ]
                , HP.type_ HP.ButtonSubmit
                , HP.style "margin-top: var(--fpo-space-1);"
                ] <>
                  if not (isValidEmail state.email) || state.submitting then
                    [ HP.disabled true ]
                  else []
              )
              [ HH.text $ translate (label :: _ "rp_sendResetLink") state.translator ]
          ]
      ]

  -- ─── Flow 1 success: Link was sent ───────────────────────────────
  renderRequestLinkSent :: State -> H.ComponentHTML Action () m
  renderRequestLinkSent state =
    HH.div
      [ HP.style "background: var(--fpo-bg-elevated); border: 1px solid var(--fpo-border-subtle); border-radius: var(--fpo-radius-xl); box-shadow: var(--fpo-shadow-sm); padding: var(--fpo-space-6); text-align: center;" ]
      [ HH.div
          [ HP.style "width: 48px; height: 48px; border-radius: 50%; background: var(--fpo-success-subtle); color: var(--fpo-success); display: inline-flex; align-items: center; justify-content: center; font-size: var(--fpo-text-xl); margin-bottom: var(--fpo-space-4);" ]
          [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check-lg" ] ] [] ]
      , HH.h2
          [ HP.style "font-size: var(--fpo-text-lg); font-weight: 700; color: var(--fpo-text-primary); margin: 0 0 var(--fpo-space-2) 0;" ]
          [ HH.text $ translate (label :: _ "rp_linkSentTitle") state.translator ]
      , HH.p
          [ HP.style "font-size: var(--fpo-text-sm); color: var(--fpo-text-secondary); margin: 0; line-height: 1.5;" ]
          [ HH.text $ translate (label :: _ "rp_linkSentBodyBefore") state.translator
          , HH.span [ HP.style "font-weight: 600; color: var(--fpo-text-primary);" ] [ HH.text $ " " <> state.email <> " " ]
          , HH.text $ translate (label :: _ "rp_linkSentBodyAfter") state.translator
          ]
      ]

  -- ─── Flow 2: Set a new password (token in URL) ──────────────────
  renderNewPasswordForm :: State -> H.ComponentHTML Action () m
  renderNewPasswordForm state =
    let
      passwordsMatch = state.passwordPrimary == state.passwordSecondary
      formReady = state.passwordPrimary /= "" && passwordsMatch && not state.submitting
      showMismatch = state.passwordSecondary /= "" && not passwordsMatch
      passwordType = if state.showPassword then HP.InputText else HP.InputPassword
      eyeIcon = if state.showPassword then "bi-eye-slash-fill" else "bi-eye-fill"
    in
      HH.div
        [ HP.style "background: var(--fpo-bg-elevated); border: 1px solid var(--fpo-border-subtle); border-radius: var(--fpo-radius-xl); box-shadow: var(--fpo-shadow-sm); padding: var(--fpo-space-6);" ]
        [ HH.h2
            [ HP.style "font-size: var(--fpo-text-xl); font-weight: 700; color: var(--fpo-text-primary); margin: 0 0 var(--fpo-space-1) 0;" ]
            [ HH.text $ translate (label :: _ "rp_Header") state.translator ]
        , HH.p
            [ HP.style "font-size: var(--fpo-text-sm); color: var(--fpo-text-tertiary); margin: 0 0 var(--fpo-space-5) 0;" ]
            [ HH.text $ translate (label :: _ "rp_newPasswordHint") state.translator ]
        , HH.form
            [ HE.onSubmit SubmitNewPassword ]
            [ -- New password
              HH.label
                [ HP.classes [ HB.formLabel ]
                , HP.style "font-size: var(--fpo-text-sm); font-weight: 500;"
                ]
                [ HH.text $ translate (label :: _ "rp_PasswordNew") state.translator ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb3 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-lock") ] [] ]
                , HH.input
                    [ HP.type_ passwordType
                    , HP.classes [ HB.formControl ]
                    , HP.placeholder (translate (label :: _ "common_password") state.translator)
                    , HP.value state.passwordPrimary
                    , HE.onValueInput UpdatePasswordPrimary
                    , HP.required true
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                    , HE.onClick $ const ToggleShowPassword
                    , HP.tabIndex (-1)
                    ]
                    [ HH.i [ HP.class_ (H.ClassName eyeIcon) ] [] ]
                ]
            , -- Confirm password
              HH.label
                [ HP.classes [ HB.formLabel ]
                , HP.style "font-size: var(--fpo-text-sm); font-weight: 500;"
                ]
                [ HH.text $ translate (label :: _ "rp_PasswordConfirm") state.translator ]
            , HH.div [ HP.classes [ HB.inputGroup, HB.mb1 ] ]
                [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                    [ HH.i [ HP.class_ (H.ClassName "bi-lock") ] [] ]
                , HH.input
                    [ HP.type_ passwordType
                    , HP.classes $
                        [ HB.formControl ] <>
                          if showMismatch then [ H.ClassName "is-invalid" ] else []
                    , HP.placeholder (translate (label :: _ "rp_PasswordConfirm") state.translator)
                    , HP.value state.passwordSecondary
                    , HE.onValueInput UpdatePasswordSecondary
                    , HP.required true
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                    , HE.onClick $ const ToggleShowPassword
                    , HP.tabIndex (-1)
                    ]
                    [ HH.i [ HP.class_ (H.ClassName eyeIcon) ] [] ]
                ]
            , -- Mismatch feedback
              if showMismatch then
                HH.div
                  [ HP.style "font-size: var(--fpo-text-xs); color: var(--fpo-danger); margin-bottom: var(--fpo-space-3);" ]
                  [ HH.text $ translate (label :: _ "rp_NoMatch") state.translator ]
              else
                HH.div [ HP.style "margin-bottom: var(--fpo-space-4);" ] []
            , HH.button
                ( [ HP.classes [ HB.btn, HB.btnPrimary, HB.w100 ]
                  , HP.type_ HP.ButtonSubmit
                  ] <>
                    if not formReady then [ HP.disabled true ] else []
                )
                [ HH.text $ translate (label :: _ "rp_setNewPassword") state.translator ]
            ]
        ]

  -- ─── Helpers ─────────────────────────────────────────────────────
  isValidEmail :: String -> Boolean
  isValidEmail email =
    let pattern = "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"
    in case regex pattern noFlags of
      Right r -> test r email
      Left _ -> false

  -- ─── Action handlers ─────────────────────────────────────────────
  handleAction :: MonadAff m => Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      mail <- _.inputMail <$> getStore
      H.modify_ _ { email = mail }

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

    UpdateEmail email -> do
      H.modify_ _ { email = email }
      updateStore $ Store.SetMail email

    UpdatePasswordPrimary pw -> do
      H.modify_ _ { passwordPrimary = pw }

    UpdatePasswordSecondary pw -> do
      H.modify_ _ { passwordSecondary = pw }

    ToggleShowPassword -> do
      H.modify_ \s -> s { showPassword = not s.showPassword }

    SubmitRequestLink event -> do
      H.liftEffect $ preventDefault event
      state <- H.get
      if null state.email then
        updateStore $ Store.AddWarning $
          translate (label :: _ "rp_NoEmail") state.translator
      else do
        H.modify_ _ { submitting = true }
        response <- postIgnore "/password-reset/request"
          (encodeJson { resetRequestEmail: state.email })
        case response of
          Left _ -> H.modify_ _ { submitting = false }
          Right _ -> H.modify_ _ { phase = RequestLinkSent, submitting = false }

    SubmitNewPassword event -> do
      H.liftEffect $ preventDefault event
      state <- H.get
      if state.passwordPrimary /= state.passwordSecondary then
        updateStore $ Store.AddWarning $
          translate (label :: _ "rp_NoMatch") state.translator
      else do
        H.modify_ _ { submitting = true }
        response <- postIgnoreOrError "/password-reset/confirm"
          ( encodeJson
              { resetConfirmToken: state.token
              , resetConfirmNewPassword: state.passwordPrimary
              }
          )
        H.modify_ _ { submitting = false }
        case response of
          Left _ -> pure unit
          Right (Left msg) -> Store.addError $ ServerError msg
          Right (Right _) -> do
            updateStore $ Store.AddSuccess $
              translate (label :: _ "common_passwordUpdated") state.translator
            navigate loginRoute
