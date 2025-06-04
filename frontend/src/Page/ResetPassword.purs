-- | Simple test page for password reset.
-- |
-- | This page is currently not connected to any backend and
-- | does not perform any meaningful password reset.

module FPO.Page.ResetPassword (component) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Store as Store
import FPO.Page.HTML (addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput) as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Translations.Translator (FPOTranslator, fromFpoTranslator)
import Translations.Util (FPOState, selectTranslator)

data Action
  = Initialize
  | RequestCode
  | SendPasswordReset
  | UpdateEmail String
  | UpdatePasswordPrimary String
  | UpdatePasswordSecondary String
  | UpdateCode String
  | EmitError String
  | Receive (Connected FPOTranslator Unit)

type State = FPOState
  ( email :: String
  , passwordPrimary :: String
  , passwordSecondary :: String
  , error :: Maybe String
  , code :: String
  )

-- | Password reset component.
component
  :: forall query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Unit output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState: \{ context } ->
        { email: ""
        , passwordPrimary: ""
        , passwordSecondary: ""
        , code: ""
        , error: Nothing
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
      [ renderResetForm state
      , HH.div [ HP.classes [ HB.textCenter ] ]
          [ case state.error of
              Just err -> HH.div [ HP.classes [ HB.alert, HB.alertDanger ] ]
                [ HH.text err ]
              Nothing -> HH.text ""
          ]
      ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      mail <- _.inputMail <$> getStore
      H.modify_ \state -> state { email = mail }
    UpdateCode code -> do
      H.modify_ \state -> state { code = code, error = Nothing }
    UpdateEmail email -> do
      H.modify_ \state -> state { email = email, error = Nothing }
      updateStore $ Store.SetMail email
    UpdatePasswordPrimary password -> do
      H.modify_ \state -> state { passwordPrimary = password, error = Nothing }
    UpdatePasswordSecondary password -> do
      H.modify_ \state -> state { passwordSecondary = password, error = Nothing }
    RequestCode -> do
      -- TODO: This is where we would ask the backend to send a code to the user's email.
      --       We could also, instead of handling a code here, simply send an email with a link
      --       to reset the password.
      --       For now, we are just emitting an error.
      H.modify_ \state -> state
        { error = Just "[TODO] A code has (not) been sent to you by email!" }
      pure unit
    SendPasswordReset -> do
      st <- H.get
      if (st.passwordPrimary /= st.passwordSecondary) then do
        H.modify_ \state -> state
          { error = Just $ translate (label :: _ "rpNoMatch") st.translator }
      else do
        H.modify_ \state -> state
          { error = Just "[TODO] Password reset is not supported yet!" }
    EmitError error -> do
      mail <- H.gets _.email
      H.modify_ \state -> state { error = Just error }

      when (not $ null mail) do
        updateStore $ Store.SetMail mail
    Receive { context } -> H.modify_ _ { translator = fromFpoTranslator context }

renderResetForm :: forall w. State -> HH.HTML w Action
renderResetForm state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
            [ HH.text $ translate (label :: _ "rpHeader") state.translator ]
        , HH.form
            []
            [ addColumn
                state.email
                (translate (label :: _ "emailAddress") state.translator <> ":")
                (translate (label :: _ "email") state.translator)
                "bi-envelope-fill"
                HP.InputEmail
                UpdateEmail
            , addColumn
                state.passwordPrimary
                (translate (label :: _ "rpPasswordNew") state.translator <> ":")
                (translate (label :: _ "password") state.translator)
                "bi-lock-fill"
                HP.InputPassword
                UpdatePasswordPrimary
            , addColumn
                state.passwordSecondary
                (translate (label :: _ "rpPasswordConfirm") state.translator <> ":")
                (translate (label :: _ "password") state.translator)
                "bi-lock-fill"
                HP.InputPassword
                UpdatePasswordSecondary
            , HH.div []
                [ HH.label [ HP.classes [ HB.formLabel ], HP.for "code" ]
                    [ HH.text $
                        translate (label :: _ "rpConfirmationCode") state.translator
                          <> ":"
                    ]
                , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                    [ HH.button
                        ( [ HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                          , HP.type_ HP.ButtonButton
                          , HE.onClick \_ -> RequestCode
                          ] <>
                            if not (isValidEmail state.email) then
                              [ HP.disabled true ]
                            else []
                        )
                        [ HH.text $ translate (label :: _ "rpRequestCode")
                            state.translator
                        ]
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.classes [ HB.formControl ]
                        , HP.placeholder $ translate (label :: _ "rpInputCode")
                            state.translator
                        , HP.value state.code
                        , HE.onValueInput UpdateCode
                        , HP.id "code"
                        ]
                    ]
                ]

            , HH.div [ HP.classes [ HB.mb4, HB.textCenter ] ]
                [ HH.button
                    ( [ HP.classes [ HB.btn, HB.btnPrimary ]
                      , HP.type_ HP.ButtonSubmit
                      , HE.onClick $ const SendPasswordReset
                      ] <>
                        if not (isValidEmail state.email) then [ HP.disabled true ]
                        else []
                    )
                    [ HH.text $ translate (label :: _ "submit") state.translator ]
                ]
            ]
        ]
    ]
  where
  isValidEmail :: String -> Boolean
  isValidEmail email =
    let
      pattern = "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"
    in
      case regex pattern noFlags of
        Right r -> test r email
        Left _ -> false
