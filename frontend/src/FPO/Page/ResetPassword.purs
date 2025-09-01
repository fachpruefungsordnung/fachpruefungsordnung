-- | Simple test page for password reset.
-- |
-- | This page is currently not connected to any backend and
-- | does not perform any meaningful password reset.

module FPO.Page.ResetPassword (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (null)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onSubmit, onValueInput) as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
  = Initialize
  | RequestCode
  | DoSubmit Event
  | UpdatePasswordPrimary String
  | UpdatePasswordSecondary String
  | UpdateCode String
  | EmitError String
  | Receive (Connected FPOTranslator Unit)

type State = FPOState
  ( passwordPrimary :: String
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
        { passwordPrimary: ""
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
    Initialize -> pure unit
    UpdateCode code -> do
      H.modify_ \state -> state { code = code, error = Nothing }
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
    DoSubmit event -> do
      H.liftEffect $ preventDefault event

      st <- H.get
      if (st.passwordPrimary /= st.passwordSecondary) then do
        H.modify_ \state -> state
          { error = Just $ translate (label :: _ "rp_NoMatch") st.translator }
      else do
        H.modify_ \state -> state
          { error = Just "[TODO] Password reset is not supported yet!" }
    EmitError error -> do
      H.modify_ \state -> state { error = Just error }

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

renderResetForm :: forall w. State -> HH.HTML w Action
renderResetForm state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
    [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
        [ HH.text $ translate (label :: _ "rp_Header") state.translator ]
    , HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.form
            [ HE.onSubmit DoSubmit ]
            [ addColumn
                state.passwordPrimary
                (translate (label :: _ "rp_PasswordNew") state.translator <> ":")
                (translate (label :: _ "common_password") state.translator)
                "bi-lock-fill"
                HP.InputPassword
                UpdatePasswordPrimary
            , addColumn
                state.passwordSecondary
                (translate (label :: _ "rp_PasswordConfirm") state.translator <> ":")
                (translate (label :: _ "common_password") state.translator)
                "bi-lock-fill"
                HP.InputPassword
                UpdatePasswordSecondary
            , HH.div []
                [ HH.label [ HP.classes [ HB.formLabel ], HP.for "code" ]
                    [ HH.text $
                        translate (label :: _ "rp_ConfirmationCode") state.translator
                          <> ":"
                    ]
                , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
                    [ HH.button
                        ( [ HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                          , HP.type_ HP.ButtonButton
                          , HE.onClick \_ -> RequestCode
                          ] <>
                            disable
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
                        disable
                    )
                    [ HH.text $ translate (label :: _ "common_submit")
                        state.translator
                    ]
                ]
            ]
        ]
    ]
  where
  disable =
    if not (isAllFilledOut state) then
      [ HP.disabled true ]
    else []
  isAllFilledOut st =
    not (null st.passwordPrimary)
      && not (null st.passwordSecondary)
      &&
        not (null st.code)
