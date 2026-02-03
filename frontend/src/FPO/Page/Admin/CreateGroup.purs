-- | Page for creating a new group.
-- |
-- | This is a sub-page of the Administration page, accessed via the "Add" button
-- | on the Groups tab.

module FPO.Page.Admin.CreateGroup
  ( component
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Request (addGroup, getUser)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.GroupDto (GroupCreate(..))
import FPO.Dto.UserDto (isAdmin)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.UI.HTML (addColumn)
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
  | ChangeGroupName String
  | ChangeGroupDescription String
  | SubmitCreateGroup
  | Cancel

type State = FPOState
  ( groupName :: String
  , groupDescription :: String
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
    , groupName: ""
    , groupDescription: ""
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
                          [ HH.i
                              [ HP.classes [ H.ClassName "bi-folder-plus", HB.me2 ] ]
                              []
                          , HH.text $ translate
                              (label :: _ "admin_groups_createNewGroup")
                              state.translator
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
      [ HE.onSubmit $ const SubmitCreateGroup ]
      [ addColumn
          state.groupName
          (translate (label :: _ "admin_groups_groupName") state.translator)
          (translate (label :: _ "admin_groups_enterGroupName") state.translator)
          "bi-people"
          HP.InputText
          ChangeGroupName
      , HH.div [ HP.classes [ HB.mb4 ] ]
          [ HH.label [ HP.classes [ HB.formLabel ] ]
              [ HH.text $ translate (label :: _ "admin_groups_desc") state.translator
              ]
          , HH.div [ HP.classes [ HB.inputGroup ] ]
              [ HH.span [ HP.classes [ HB.inputGroupText ] ]
                  [ HH.i [ HP.classes [ H.ClassName "bi-card-text" ] ] [] ]
              , HH.textarea
                  [ HP.classes [ HB.formControl ]
                  , HP.placeholder $ translate
                      (label :: _ "admin_groups_enterGroupDesc")
                      state.translator
                  , HP.value state.groupDescription
                  , HP.rows 3
                  , HE.onValueInput ChangeGroupDescription
                  ]
              ]
          ]
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
              , HP.disabled $ null state.groupName || state.waiting
              ]
              [ if state.waiting then HH.span
                  [ HP.classes [ HB.spinnerBorderSm, HB.me2 ] ]
                  []
                else HH.i [ HP.classes [ H.ClassName "bi-plus-circle", HB.me2 ] ] []
              , HH.text $ translate (label :: _ "common_create") state.translator
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
          when (not $ isAdmin user) $ navigate Page404

    Receive { context } ->
      H.modify_ _ { translator = fromFpoTranslator context }

    ChangeGroupName name ->
      H.modify_ _ { groupName = name }

    ChangeGroupDescription desc ->
      H.modify_ _ { groupDescription = desc }

    SubmitCreateGroup -> do
      state <- H.get
      if null state.groupName then
        updateStore $ Store.AddWarning
          (translate (label :: _ "admin_groups_notEmpty") state.translator)
      else do
        H.modify_ _ { waiting = true }

        response <- addGroup $ GroupCreate
          { groupCreateName: state.groupName
          , groupCreateDescription: state.groupDescription
          }

        case response of
          Left _ -> do
            H.modify_ _ { waiting = false }
          Right _ -> do
            updateStore $ Store.AddSuccess
              ( translate (label :: _ "admin_groups_successfullyCreatedGroup")
                  state.translator
              )
            navigate $ Administration { tab: Just "groups" }

    Cancel -> navigate $ Administration { tab: Just "groups" }
