-- | Simple test page for login.
-- |
-- | This page is currently not connected to any backend and does not perform any
-- | authentication. 
-- |
-- | Additionally, this page shows how to use `MonadStore` to update and read data
-- | from the store.

module FPO.Page.Login (component) where

import Prelude

import Data.Maybe (Maybe(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Page.HTML (addColumn)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB

data Action
  = Initialize
  | NavigateToPasswordReset
  | UpdateEmail String
  | UpdatePassword String
  | EmitError String

type State =
  { email :: String
  , password :: String
  , error :: Maybe String
  }

-- | Login component.
-- |
-- | Notice how we are using MonadStore to update the store with the user's
-- | email when the user clicks on the button. 
component
  :: forall query input output m
   . Navigate m
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
    , password: ""
    , error: Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my5 ] ]
      [ renderLoginForm state
      , HH.div [ HP.classes [ HB.textCenter ] ]
          [ case state.error of
              Just err -> HH.div [ HP.classes [ HB.alert, HB.alertDanger ] ]
                [ HH.text err ]
              Nothing -> HH.text ""
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      -- When opening the login tab, we simply take the user's email 
      -- address from the store, provided that it exists (was 
      -- previously set).
      mail <- _.inputMail <$> getStore
      H.modify_ \state -> state { email = mail }
    UpdateEmail email -> do
      H.modify_ \state -> state { email = email, error = Nothing }
      -- In this example, we are simply storing the user's email in our
      -- store, every time the user clicks on the button (either login or
      -- register).
      updateStore $ Store.SetMail email
    UpdatePassword password -> do
      H.modify_ \state -> state { password = password, error = Nothing }
    EmitError error -> do
      updateStore $ Store.SetUser $ Just { userName: "local invalid User" }
      H.modify_ \state -> state { error = Just error }
    NavigateToPasswordReset -> do
      navigate PasswordReset

renderLoginForm :: forall w. State -> HH.HTML w Action
renderLoginForm state =
  HH.div [ HP.classes [ HB.row, HB.justifyContentCenter, HB.my3 ] ]
    [ HH.div [ HP.classes [ HB.colLg4, HB.colMd6, HB.colSm8 ] ]
        [ HH.h1 [ HP.classes [ HB.textCenter, HB.mb4 ] ]
            [ HH.text "Login" ]
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
                state.password
                "Passwort:"
                "Passwort"
                "bi-lock-fill"
                HP.InputPassword
                UpdatePassword
            , HH.div [ HP.classes [ HB.mb4, HB.textCenter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnPrimary ]
                    , HP.type_ HP.ButtonSubmit
                    , HE.onClick $ const (EmitError "Login not implemented!")
                    ]
                    [ HH.text "Login" ]
                ]
            , HH.div [ HP.classes [ HB.textCenter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnLink ]
                    , HP.type_ HP.ButtonButton
                    , HE.onClick $ const NavigateToPasswordReset
                    ]
                    [ HH.text "Passwort vergessen?" ]
                ]
            ]
        ]
    ]