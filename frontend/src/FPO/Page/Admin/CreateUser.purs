-- | Page for creating a new user.
-- |
-- | This is a sub-page of the Administration page, accessed via the "Add" button
-- | on the Users tab.

module FPO.Page.Admin.CreateUser
  ( component
  ) where

import Prelude

import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppError (AppError(..))
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (getUser, postString)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.CreateUserDto
  ( CreateUserDto
  , getEmail
  , getName
  , getPassword
  , withEmail
  , withName
  , withPassword
  )
import FPO.Dto.CreateUserDto as CreateUserDto
import FPO.Dto.UserDto (isUserSuperadmin)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn)
import FPO.Util (isValidEmailStrict)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

data Action
  = Initialize
  | Receive (Connected FPOTranslator Unit)
  | ChangeUsername String
  | ChangeEmail String
  | ChangePassword String
  | SubmitCreateUser
  | Cancel

type State = FPOState
  ( createUserDto :: CreateUserDto
  , waiting :: Boolean
  )

component
  :: forall query output m
   . MonadStore Store.Action Store.Store m
  => MonadAff m
  => Navigate m
  => H.Component query Unit output m
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
  initialState :: Connected FPOTranslator Unit -> State
  initialState { context } =
    { translator: fromFpoTranslator context
    , createUserDto: CreateUserDto.empty
    , waiting: false
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.container, HB.my5 ] ]
      [ HH.div [ HP.classes [ HB.row, HB.justifyContentCenter ] ]
          [ HH.div [ HP.classes [ HB.col12, HB.colMd8, HB.colLg6 ] ]
              [ HH.div [ HP.classes [ HB.card ] ]
                  [ HH.div [ HP.classes [ HB.cardHeader ] ]
                      [ HH.h5 [ HP.classes [ HB.mb0 ] ]
                          [ HH.i [ HP.classes [ H.ClassName "bi-person-plus-fill", HB.me2 ] ] []
                          , HH.text $ translate (label :: _ "admin_users_createNewUser") state.translator
                          ]
                      ]
                  , HH.div [ HP.classes [ HB.cardBody ] ]
                      [ renderForm state ]
                  ]
              ]
          ]
      ]

  renderForm :: State -> H.ComponentHTML Action () m
  renderForm state =
    HH.form
      [ HE.onSubmit $ const SubmitCreateUser ]
      [ addColumn
          (getName state.createUserDto)
          (translate (label :: _ "common_userName") state.translator)
          (translate (label :: _ "common_userName") state.translator)
          "bi-person"
          HP.InputText
          ChangeUsername
      , addColumn
          (getEmail state.createUserDto)
          (translate (label :: _ "common_email") state.translator)
          (translate (label :: _ "common_email") state.translator)
          "bi-envelope-fill"
          HP.InputEmail
          ChangeEmail
      , addColumn
          (getPassword state.createUserDto)
          (translate (label :: _ "common_password") state.translator)
          (translate (label :: _ "common_password") state.translator)
          "bi-lock-fill"
          HP.InputPassword
          ChangePassword
      , HH.div [ HP.classes [ HB.dFlex, HB.justifyContentBetween, HB.mt4 ] ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnSecondary ]
              , HE.onClick $ const Cancel
              , HP.disabled state.waiting
              ]
              [ HH.i [ HP.classes [ H.ClassName "bi-arrow-left", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "common_cancel") state.translator
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit
              , HP.classes [ HB.btn, HB.btnPrimary ]
              , HP.disabled $ not (isFormValid state.createUserDto) || state.waiting
              ]
              [ if state.waiting
                  then HH.span [ HP.classes [ HB.spinnerBorderSm, HB.me2 ] ] []
                  else HH.i [ HP.classes [ H.ClassName "bi-plus-circle", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "admin_users_create") state.translator
              ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      userResult <- getUser
      case userResult of
        Left _ -> navigate Page404
        Right user ->
          when (not $ isUserSuperadmin user) $ navigate Page404

    Receive { context } ->
      H.modify_ _ { translator = fromFpoTranslator context }

    ChangeUsername username -> do
      state <- H.get
      H.modify_ _ { createUserDto = withName username state.createUserDto }

    ChangeEmail email -> do
      state <- H.get
      H.modify_ _ { createUserDto = withEmail email state.createUserDto }

    ChangePassword password -> do
      state <- H.get
      H.modify_ _ { createUserDto = withPassword password state.createUserDto }

    SubmitCreateUser -> do
      state <- H.get
      H.modify_ _ { waiting = true }

      response <- postString "/register" (encodeJson state.createUserDto)
      case response of
        Left err -> do
          Store.addError $ ServerError
            ( (translate (label :: _ "admin_users_failedToCreateUser") state.translator)
                <> ": " <> show err
            )
          H.modify_ _ { waiting = false }
        Right _ -> do
          updateStore $ Store.AddSuccess
            (translate (label :: _ "admin_users_successfullyCreatedUser") state.translator)
          navigate $ Administration { tab: Nothing }

    Cancel -> navigate $ Administration { tab: Nothing }

isFormValid :: CreateUserDto -> Boolean
isFormValid dto =
  not (null $ getName dto)
    && not (null $ getEmail dto)
    && not (null $ getPassword dto)
    && isValidEmailStrict (getEmail dto)
