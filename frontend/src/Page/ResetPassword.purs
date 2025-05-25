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
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB

data Action
  = Initialize
  | RequestCode
  | SendPasswordReset
  | UpdateEmail String
  | UpdatePasswordPrimary String
  | UpdatePasswordSecondary String
  | UpdateCode String
  | EmitError String

type State =
  { email :: String
  , passwordPrimary :: String
  , passwordSecondary :: String
  , error :: Maybe String
  , code :: String
  }

-- | Passwort reset component.
component
  :: forall query input output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState :: input -> State
  initialState _ =
    { email: ""
    , passwordPrimary: ""
    , passwordSecondary: ""
    , code: ""
    , error: Nothing
    }

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
      H.modify_ \state -> state { error = Just "TODO; Es wurde Ihnen (k)ein Code per E-Mail geschickt!" }
      pure unit
    SendPasswordReset -> do
      { passwordPrimary, passwordSecondary } <- H.get
      if (passwordPrimary /= passwordSecondary) then do
        H.modify_ \state -> state { error = Just "Die Passwörter stimmen nicht überein!" }
      else do
        H.modify_ \state -> state { error = Just "Passwortreset wird noch nicht unterstützt!" }
    EmitError error -> do
      mail <- H.gets _.email
      H.modify_ \state -> state { error = Just error }

      when (not $ null mail) do
        updateStore $ Store.SetMail mail

renderResetForm :: forall w. State -> HH.HTML w Action
renderResetForm state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
            [ HH.text "Passwort zurücksetzen" ]
        , HH.form
            []
            [ addColumn
                state.email
                "E-Mail-Addresse:"
                "E-Mail"
                "bi-envelope-fill"
                HP.InputEmail
                UpdateEmail
            , addColumn
                state.passwordPrimary
                "Neues Passwort:"
                "Passwort"
                "bi-lock-fill"
                HP.InputPassword
                UpdatePasswordPrimary
            , addColumn
                state.passwordSecondary
                "Neues Passwort wiederholen:"
                "Passwort"
                "bi-lock-fill"
                HP.InputPassword
                UpdatePasswordSecondary
            , HH.div []
                [ HH.label [ HP.classes [ HB.formLabel ], HP.for "code" ]
                    [ HH.text "Bestätigungscode:" ]
                , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                    [ HH.button
                        ( [ HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                          , HP.type_ HP.ButtonButton
                          , HE.onClick \_ -> RequestCode
                          ] <> if not (isValidEmail state.email) then [ HP.disabled true ] else []
                        )
                        [ HH.text "Code anfordern" ]
                    , HH.input
                        [ HP.type_ HP.InputText
                        , HP.classes [ HB.formControl ]
                        , HP.placeholder "Code eingeben"
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
                      ] <> if not (isValidEmail state.email) then [ HP.disabled true ] else []
                    )
                    [ HH.text "Abschicken" ]
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