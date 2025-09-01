module FPO.Page.ResetPassword (component) where

import Prelude

import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppError (AppError(..))
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (postIgnore, postIgnoreOrError)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onSubmit, onValueInput) as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
  = Initialize
  | RequestCode
  | DoSubmit Event
  | UpdateEmail String
  | UpdatePasswordPrimary String
  | UpdatePasswordSecondary String
  | UpdateCode String
  | Receive (Connected FPOTranslator Unit)

type State = FPOState
  ( email :: String
  , passwordPrimary :: String
  , passwordSecondary :: String
  , code :: String
  )

-- | Password reset component.
component
  :: forall query output m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component query Unit output m
component =
  connect selectTranslator $ H.mkComponent
    { initialState: \{ context } ->
        { email: ""
        , passwordPrimary: ""
        , passwordSecondary: ""
        , code: ""
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
      [ HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
          [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
              [ HH.text $ translate (label :: _ "rp_Header") state.translator ]
          , HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
              [ HH.form
                  [ HE.onSubmit DoSubmit ]
                  [ addColumn
                      state.email
                      ( translate (label :: _ "common_emailAddress") state.translator
                          <> ":"
                      )
                      (translate (label :: _ "common_email") state.translator)
                      "bi-envelope-fill"
                      HP.InputEmail
                      UpdateEmail
                  , addColumn
                      state.passwordPrimary
                      ( translate (label :: _ "rp_PasswordNew") state.translator <>
                          ":"
                      )
                      (translate (label :: _ "common_password") state.translator)
                      "bi-lock-fill"
                      HP.InputPassword
                      UpdatePasswordPrimary
                  , addColumn
                      state.passwordSecondary
                      ( translate (label :: _ "rp_PasswordConfirm") state.translator
                          <> ":"
                      )
                      (translate (label :: _ "common_password") state.translator)
                      "bi-lock-fill"
                      HP.InputPassword
                      UpdatePasswordSecondary
                  , HH.div []
                      [ HH.label [ HP.classes [ HB.formLabel ], HP.for "code" ]
                          [ HH.text $
                              translate (label :: _ "rp_ConfirmationCode")
                                state.translator
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
                              [ HH.text $ translate (label :: _ "rp_RequestCode")
                                  state.translator
                              ]
                          , HH.input
                              [ HP.type_ HP.InputText
                              , HP.classes [ HB.formControl ]
                              , HP.placeholder $ translate (label :: _ "rp_InputCode")
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
                            ] <>
                              if not (isValidForm state) then [ HP.disabled true ]
                              else []
                          )
                          [ HH.text $ translate (label :: _ "common_submit")
                              state.translator
                          ]
                      ]
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

    isValidForm :: State -> Boolean
    isValidForm s = s.code /= ""
      && s.passwordPrimary /= ""
      && s.passwordPrimary == s.passwordSecondary

  handleAction :: MonadAff m => Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      mail <- _.inputMail <$> getStore
      H.modify_ \state -> state { email = mail }
    UpdateCode code -> do
      H.modify_ \state -> state { code = code }
    UpdateEmail email -> do
      H.modify_ \state -> state { email = email }
      updateStore $ Store.SetMail email
    UpdatePasswordPrimary password -> do
      H.modify_ \state -> state { passwordPrimary = password }
    UpdatePasswordSecondary password -> do
      H.modify_ \state -> state { passwordSecondary = password }
    RequestCode -> do
      state <- H.get
      if null state.email then
        updateStore $ Store.AddWarning $ translate (label :: _ "rp_NoEmail")
          state.translator
      else do
        response <- postIgnore "/password-reset/request"
          (encodeJson { resetRequestEmail: state.email })
        case response of
          Left _ -> pure unit
          Right _ -> do
            updateStore $ Store.AddSuccess
              ( ( translate (label :: _ "common_sentResetLinkDone")
                    state.translator
                ) <> state.email
              )
    DoSubmit event -> do
      H.liftEffect $ preventDefault event
      state <- H.get
      if (state.passwordPrimary /= state.passwordSecondary) then do
        updateStore $ Store.AddWarning $ translate (label :: _ "rp_NoMatch")
          state.translator
      else do
        response <- postIgnoreOrError "/password-reset/confirm"
          ( encodeJson
              { resetConfirmToken: state.code
              , resetConfirmNewPassword: state.passwordPrimary
              }
          )
        case response of
          Left _ -> pure unit
          Right (Left msg) -> updateStore $ Store.AddError $ ServerError msg
          Right (Right _) -> do
            updateStore $ Store.AddSuccess $ translate
              (label :: _ "common_passwordUpdated")
              state.translator
    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }
