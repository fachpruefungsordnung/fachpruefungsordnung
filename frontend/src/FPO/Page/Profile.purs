-- | The profile page is used to display user information or settings.

module FPO.Page.Profile
  ( component
  , strengthColor
  , Output(..)
  ) where

import Prelude

import Data.Argonaut (encodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Array (head, null, tail)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length, take, toUpper)
import Data.String.Regex (regex, split)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.AppError (AppError)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getUser, getUserWithId, patchString, postIgnore)
import FPO.Data.Store as Store
import FPO.Dto.UserDto
  ( PatchUserDto(..)
  , getFullUserRoles
  , getUserEmail
  , getUserID
  , getUserName
  )
import FPO.Dto.UserRoleDto (FullUserRoleDto, Role(..), getGroupName, getUserRole)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (AutocompleteType(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

data Action
  = Initialize
  | Receive (Connected FPOTranslator Input)
  | UsernameInput String
  | CancelEdit
  | SaveUsername
  | HideSavedToast
  | SendResetLink
  | HideLinkToast
  | NewPwInput String
  | NewPw2Input String
  | UpdatePassword
  | HidePwToast
  | HideNotYetImplementedToast

data Output = ChangedUsername

type State = FPOState
  ( username :: String
  , originalUsername :: String
  , userId :: String
  , emailAddress :: String
  , unsaved :: Boolean
  , newPw :: String
  , newPw2 :: String
  , loadSaveUsername :: Boolean
  , showSavedToast ::
      Maybe String -- xyz saved successfull/ xyz erfolgreich gespeichert
  , showLinkToast :: Boolean
  , showPwToast :: Boolean
  , showNotYetImplementedToast :: Boolean
  , groupMemberships :: Array FullUserRoleDto
  , loginSuccessfulBanner :: Boolean
  , isYourProfile :: Boolean
  )

type Input = { userId :: Maybe String }

-- | User profile page component.
component
  :: forall query m
   . Navigate m
  => MonadAff m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Output m
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
  initialState :: Connected FPOTranslator Input -> State
  initialState { context, input: { userId } } =
    { username: ""
    , originalUsername: ""
    , userId: fromMaybe "" userId
    , emailAddress: ""
    , unsaved: false
    , newPw: ""
    , newPw2: ""
    , loadSaveUsername: false
    , showSavedToast: Nothing
    , showLinkToast: false
    , showPwToast: false
    , showNotYetImplementedToast: false
    , loginSuccessfulBanner: false
    , groupMemberships: []
    , translator: fromFpoTranslator context
    , isYourProfile: userId == Nothing -- We only input the userId if it is the profile from another person
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.vh100 ] ]
      [ HH.div [ HP.classes [ HB.container, HB.py4 ] ]
          [ HH.div [ HP.classes [ HB.row, HB.g4 ] ]
              [ -- LEFT COLUMN
                HH.div [ HP.classes [ HB.colLg8 ] ]
                  [ HH.div [ HP.classes [ HB.card ] ]
                      [ HH.div [ HP.classes [ HB.cardBody ] ]
                          [ -- header
                            HH.div
                              [ HP.classes [ HB.dFlex, HB.alignItemsCenter, HB.mb3 ] ]
                              [ HH.div [ HP.classes [ ClassName "avatar", HB.me3 ] ]
                                  [ HH.text $ fromMaybe "" (initials state.username) ]
                              , HH.div_
                                  [ HH.h5_
                                      [ HH.text $
                                          ( translate (label :: _ "prof_profile")
                                              state.translator
                                          ) <>
                                            if state.isYourProfile then
                                              ( " ("
                                                  <>
                                                    ( translate
                                                        (label :: _ "prof_you")
                                                        state.translator
                                                    )
                                                  <> ")"
                                              )
                                            else ""
                                      ]
                                  ]
                              ]
                          , -- Username editable
                            HH.div [ HP.classes [ HB.mb3 ] ]
                              [ HH.label
                                  [ HP.for "username", HP.classes [ HB.formLabel ] ]
                                  [ HH.text
                                      ( translate (label :: _ "common_userName")
                                          state.translator
                                      )
                                  ]
                              , HH.div [ HP.classes [ HB.positionRelative ] ]
                                  [ HH.div [ HP.classes [ ClassName "input-icon" ] ]
                                      [ HH.i
                                          [ HP.classes
                                              [ HB.bi, ClassName "bi-card-heading" ]
                                          ]
                                          []
                                      ]
                                  , HH.input
                                      [ HP.id "username"
                                      , HP.classes
                                          [ HB.formControl
                                          , ClassName "input-with-icon"
                                          ]
                                      , HP.type_ HP.InputText
                                      , HP.value state.username
                                      , HP.disabled (not state.isYourProfile)
                                      , HE.onValueInput UsernameInput
                                      , HP.autocomplete AutocompleteUsername
                                      , HPA.describedBy "usernameHelp"
                                      ]
                                  ]
                              , HH.div
                                  [ HP.classes
                                      [ HB.dFlex
                                      , HB.alignItemsCenter
                                      , HB.gap2
                                      , HB.mt2
                                      ]
                                  ]
                                  [ HH.small
                                      [ HP.id "usernameHelp"
                                      , HP.classes [ HB.textMuted ]
                                      ]
                                      [ HH.text $ translate
                                          (label :: _ "prof_usernameHelp")
                                          state.translator
                                      ]
                                  , if state.unsaved then
                                      HH.span
                                        [ HP.id "unsavedBadge"
                                        , HP.classes
                                            [ HB.badge
                                            , HB.roundedPill
                                            , HB.textBgWarning
                                            ]
                                        ]
                                        [ HH.span
                                            [ HP.classes
                                                [ HB.me1, ClassName "unsaved-dot" ]
                                            ]
                                            []
                                        , HH.text $ translate
                                            (label :: _ "prof_unsaved")
                                            state.translator
                                        ]
                                    else HH.text ""
                                  ]
                              ]
                          , -- Action bar
                            if state.unsaved then
                              HH.div
                                [ HP.id "actionBar"
                                , HP.classes
                                    [ HB.dFlex, HB.gap2, HB.justifyContentEnd ]
                                ]
                                [ HH.button
                                    [ HP.id "cancelBtn"
                                    , HP.classes [ HB.btn, HB.btnLight ]
                                    , HE.onClick (const CancelEdit)
                                    ]
                                    [ HH.text $ translate (label :: _ "common_cancel")
                                        state.translator
                                    ]
                                , let
                                    isLoading = state.loadSaveUsername
                                    btnVariant =
                                      if isLoading then HB.btnSecondary
                                      else HB.btnPrimary
                                    labelChildren =
                                      if isLoading then
                                        [ HH.span
                                            [ HP.classes
                                                [ HB.spinnerBorder
                                                , HB.spinnerBorderSm
                                                , HB.me2
                                                ]
                                            , HP.attr (HH.AttrName "aria-hidden")
                                                "true"
                                            ]
                                            []
                                        , HH.text $ translate
                                            (label :: _ "common_saving")
                                            state.translator
                                        ]
                                      else
                                        [ HH.text $ translate
                                            (label :: _ "common_save")
                                            state.translator
                                        ]
                                  in
                                    HH.button
                                      [ HP.id "saveBtn"
                                      , HP.classes [ HB.btn, btnVariant ]
                                      , HP.disabled isLoading
                                      , HP.attr (HH.AttrName "aria-busy")
                                          (if isLoading then "true" else "false")
                                      , HE.onClick (const SaveUsername)
                                      ]
                                      [ HH.span_ labelChildren ]
                                ]
                            else HH.text ""
                          , HH.hr [ HP.classes [ HB.my4 ] ]
                          , if (not state.isYourProfile) then HH.div_ []
                            else
                              HH.div
                                [ HP.classes
                                    [ HB.dFlex
                                    , HB.alignItemsCenter
                                    , HB.justifyContentBetween
                                    ]
                                ]
                                [ HH.div_
                                    [ HH.h6_
                                        [ HH.text $ translate
                                            (label :: _ "common_password")
                                            state.translator
                                        ]
                                    , HH.small [ HP.classes [ HB.textMuted ] ]
                                        [ HH.text $ translate
                                            (label :: _ "prof_passwordSecurity")
                                            state.translator
                                        ]
                                    ]
                                , -- Trigger modal via data attributes (Bootstrap handles visuals)
                                  HH.button
                                    [ HP.classes [ HB.btn, HB.btnOutlineDanger ]
                                    , HP.attr (HH.AttrName "data-bs-toggle") "modal"
                                    , HP.attr (HH.AttrName "data-bs-target")
                                        "#resetModal"
                                    ]
                                    [ HH.text $ translate
                                        (label :: _ "prof_resetPassword")
                                        state.translator
                                    ]
                                ]
                          ]
                      ]
                  ]
              , -- RIGHT COLUMN (groups sidebar)
                HH.div [ HP.classes [ HB.colLg4 ] ]
                  [ HH.div [ HP.classes [ ClassName "sidebar-sticky" ] ]
                      [ HH.div [ HP.classes [ HB.card, HB.mb4 ] ]
                          [ HH.div
                              [ HP.classes
                                  [ HB.cardHeader
                                  , HB.dFlex
                                  , HB.alignItemsCenter
                                  , HB.justifyContentBetween
                                  ]
                              ]
                              [ HH.h6_
                                  [ HH.text $ translate
                                      (label :: _ "prof_groupsAndRoles")
                                      state.translator
                                  ]
                              , HH.span
                                  [ HP.classes [ HB.badge, HB.textBgSecondary ] ]
                                  [ HH.text $ show
                                      (Array.length state.groupMemberships)
                                  ]
                              ]
                          , HH.div [ HP.classes [ HB.listGroup, HB.listGroupFlush ] ]
                              (map groupListItem state.groupMemberships)
                          , HH.div [ HP.classes [ HB.cardFooter ] ]
                              [ HH.small [ HP.classes [ HB.textMuted ] ]
                                  [ HH.text $ translate (label :: _ "prof_rolesHelp")
                                      state.translator
                                  ]
                              ]
                          ]
                      , HH.div [ HP.classes [ HB.card ] ]
                          [ HH.div [ HP.classes [ HB.cardBody ] ]
                              [ HH.h6_
                                  [ HH.text $ translate
                                      (label :: _ "prof_accountEmail")
                                      state.translator
                                  ]
                              , HH.div [ HP.classes [ HB.inputGroup ] ]
                                  [ HH.span
                                      [ HP.classes [ HB.inputGroupText ]
                                      , HP.id "email-addon"
                                      ]
                                      [ HH.text "@" ]
                                  , HH.input
                                      [ HP.type_ HP.InputEmail
                                      , HP.classes [ HB.formControl ]
                                      , HP.value state.emailAddress
                                      , HP.disabled true
                                      , HPA.describedBy "email-addon"
                                      ]
                                  ]
                              , HH.small [ HP.classes [ HB.textMuted ] ]
                                  [ HH.text $ translate
                                      (label :: _ "prof_accountEmailHelp")
                                      state.translator
                                  ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      , -- Password reset modal markup (Bootstrap)
        modal state
      , -- Toasts (show/hide via state toggles)
        toasts state
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      userResult <-
        if state.isYourProfile then getUser
        else getUserWithId state.userId
      case userResult of
        Left _ -> pure unit -- TODO Ignore error like in editor
        Right user -> do
          H.modify_ _
            { username = getUserName user
            , originalUsername = getUserName user
            , emailAddress = getUserEmail user
            , userId = getUserID user
            , groupMemberships = getFullUserRoles user
            }
    Receive { context, input } -> do
      let newTranslator = fromFpoTranslator context
      state <- H.get
      let oldUserId = if state.isYourProfile then Nothing else Just state.userId
      let newUserId = input.userId
      H.modify_ _ { translator = newTranslator }
      when (oldUserId /= newUserId) $ do
        H.modify_ _
          { userId = fromMaybe "" newUserId
          , isYourProfile = newUserId == Nothing
          , username = ""
          , originalUsername = ""
          , emailAddress = ""
          , unsaved = false
          , newPw = ""
          , newPw2 = ""
          , groupMemberships = []
          , showSavedToast = Nothing
          , showLinkToast = false
          , showPwToast = false
          , showNotYetImplementedToast = false
          , loginSuccessfulBanner = false
          , loadSaveUsername = false
          }
        handleAction Initialize
    UsernameInput value -> do
      state <- H.get
      H.modify_ _
        { username = value
        , unsaved = (value /= state.originalUsername)
        }
    CancelEdit -> do
      state <- H.get
      H.put state { username = state.originalUsername, unsaved = false }
    SaveUsername -> do
      state <- H.get
      let
        patchUserDto = PatchUserDto
          { newEmail: state.emailAddress, newName: state.username }
      H.put state { loadSaveUsername = true }
      response <- patchString ("/users/" <> state.userId)
        (encodeJson patchUserDto)
      H.put state { loadSaveUsername = false }
      case response of
        -- | In this path the error message is mostly to technical for the user to show.
        Left _ -> pure unit
        Right _ -> do
          H.put state
            { originalUsername = state.username
            , unsaved = false
            , showSavedToast = Just $ translate (label :: _ "common_userName")
                state.translator
            }
          H.raise ChangedUsername
          pure unit
    HideSavedToast -> H.modify_ _ { showSavedToast = Nothing }
    HideNotYetImplementedToast -> H.modify_ _ { showNotYetImplementedToast = false }
    SendResetLink -> do
      maybeUser <- getUser
      case maybeUser of
        Left _ -> pure unit -- App error case
        Right user -> do
          let mailAddress = getUserEmail user
          response <- postIgnore "/password-reset/request"
            (encodeJson { resetRequestEmail: mailAddress })
          case response of
            Left _ -> pure unit
            Right _ -> do
              state <- H.get
              updateStore $ Store.AddSuccess
                ( ( translate (label :: _ "prof_sentResetLinkDone")
                      state.translator
                  ) <> mailAddress
                )
    HideLinkToast -> H.modify_ _ { showLinkToast = false }

    NewPwInput v -> H.modify_ _ { newPw = v }
    NewPw2Input v -> H.modify_ _ { newPw2 = v }

    UpdatePassword -> do
      state <- H.get
      result <- patchString "/me/reset-password" (encodeString state.newPw)
      case result of
        Left appError -> handleAppError appError
        Right _ -> H.modify_ _
          { showSavedToast = Just $ translate (label :: _ "prof_newPassword")
              state.translator
          }
    HidePwToast -> H.modify_ _ { showPwToast = false }

initials :: String -> Maybe String
initials name =
  let
    regex' = regex " " noFlags
    parts = case regex' of
      Left _ -> []
      Right regex'' -> split regex'' name
  in
    do
      firstWord <- head parts
      case tail parts of
        Nothing -> -- only one word
          let
            firstLetter = toUpper (take 1 firstWord)
          in
            pure firstLetter
        Just rest -> do
          if null rest then
            let
              firstLetter = toUpper (take 1 firstWord)
            in
              pure firstLetter
          else do
            lastWord <- head rest
            let firstLetter = toUpper (take 1 firstWord)
            let lastLetter = toUpper (take 1 lastWord)
            pure (firstLetter <> lastLetter)

strengthColor :: Int -> String
strengthColor s =
  if s < 40 then "#dc3545" else if s < 70 then "#fd7e14" else "#198754"

toasts :: forall w. State -> HH.HTML w Action
toasts s =
  HH.div
    [ HP.classes [ HB.positionFixed, HB.top0, HB.end0, HB.p3 ]
    , HP.style "z-index: 1080;"
    ]
    [ toast (s.showSavedToast /= Nothing) "toastSaved"
        (translate (label :: _ "prof_usernameSaved") s.translator)
        HideSavedToast
        HB.textBgSuccess
    , toast (s.showLinkToast) "toastLink"
        (translate (label :: _ "prof_passwordResetLinkSent") s.translator)
        HideLinkToast
        HB.textBgPrimary
    , toast (s.showPwToast) "toastPw"
        (translate (label :: _ "prof_passwordUpdated") s.translator)
        HidePwToast
        HB.textBgSuccess
    , toast (s.showNotYetImplementedToast) "toastNotYetImplemented"
        (translate (label :: _ "prof_featureNotImplemented") s.translator)
        HideNotYetImplementedToast
        HB.textBgWarning
    ]

toast
  :: forall w. Boolean -> String -> String -> Action -> ClassName -> HH.HTML w Action
toast visible tid msg hideAct bgClass =
  let
    classes = [ HB.toast, HB.alignItemsCenter, bgClass, HB.border0 ] <>
      if visible then [ HB.show ] else []
  in
    HH.div [ HP.id tid, HP.classes classes, HPA.live "polite", HPA.atomic "true" ]
      [ HH.div [ HP.classes [ HB.dFlex ] ]
          [ HH.div [ HP.classes [ HB.toastBody ] ] [ HH.text msg ]
          , HH.button
              [ HP.classes [ HB.btnClose, HB.me2, HB.mAuto ]
              , HE.onClick (const hideAct)
              , HPA.label "Close"
              ]
              []
          ]
      ]

-- Helpers
listItem :: forall w. String -> String -> ClassName -> HH.HTML w Action
listItem group role badgeClass =
  HH.div
    [ HP.classes
        [ HB.listGroupItem, HB.dFlex, HB.justifyContentBetween, HB.alignItemsCenter ]
    ]
    [ HH.text group
    , HH.span [ HP.classes [ HB.badge, badgeClass ] ] [ HH.text role ]
    ]

modal :: State -> forall w. HH.HTML w Action
modal state =
  HH.div
    [ HP.classes [ HB.modal, HB.fade ]
    , HP.id "resetModal"
    , HP.tabIndex (-1)
    , HPA.labelledBy "resetLabel"
    , HPA.hidden "true"
    ]
    [ HH.div [ HP.classes [ HB.modalDialog, HB.modalDialogCentered ] ]
        [ HH.div [ HP.classes [ HB.modalContent ] ]
            [ HH.div [ HP.classes [ HB.modalHeader ] ]
                [ HH.h5 [ HP.classes [ HB.modalTitle ], HP.id "resetLabel" ]
                    [ HH.text $ translate (label :: _ "prof_resetPassword")
                        state.translator
                    ]
                , HH.button
                    [ HP.classes [ HB.btnClose ]
                    , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                    , HPA.label "Close"
                    ]
                    []
                ]
            , HH.div [ HP.classes [ HB.modalBody ] ]
                [ HH.p [ HP.classes [ HB.mb3 ] ]
                    [ HH.text $ translate (label :: _ "prof_chooseResetMethod")
                        state.translator
                    ]
                , HH.div [ HP.classes [ HB.dGrid, HB.gap2, HB.mb3 ] ]
                    [ HH.button
                        [ HP.id "sendLinkBtn"
                        , HP.classes [ HB.btn, HB.btnOutlinePrimary ]
                        , HE.onClick (const SendResetLink)
                        ]
                        [ HH.text $ translate (label :: _ "prof_sendResetLink")
                            state.translator
                        ]
                    ]
                , HH.div [ HP.classes [ HB.textCenter, HB.textMuted, HB.mb2 ] ]
                    [ HH.text $ translate (label :: _ "prof_orSeparator")
                        state.translator
                    ]
                , HH.div [ HP.classes [ HB.mb3 ] ]
                    [ HH.label [ HP.for "newPw", HP.classes [ HB.formLabel ] ]
                        [ HH.text $ translate (label :: _ "prof_newPassword")
                            state.translator
                        ]
                    , HH.input
                        [ HP.id "newPw"
                        , HP.type_ HP.InputPassword
                        , HP.classes [ HB.formControl ]
                        , HP.placeholder "••••••••"
                        , HE.onValueInput NewPwInput
                        , HP.autocomplete AutocompleteNewPassword
                        ]
                    , HH.small [ HP.id "pwHint", HP.classes [ HB.textMuted ] ]
                        [ HH.text $ translate (label :: _ "prof_passwordStrengthHelp")
                            state.translator
                        ]
                    ]
                , HH.div [ HP.classes [ HB.mb3 ] ]
                    [ HH.label [ HP.for "newPw2", HP.classes [ HB.formLabel ] ]
                        [ HH.text $ translate (label :: _ "prof_confirmPassword")
                            state.translator
                        ]
                    , HH.input
                        [ HP.id "newPw2"
                        , HP.type_ HP.InputPassword
                        , HP.classes [ HB.formControl ]
                        , HP.placeholder "••••••••"
                        , HE.onValueInput NewPw2Input
                        , HP.autocomplete AutocompleteNewPassword
                        ]
                    , let
                        mismatch = state.newPw /= "" && state.newPw2 /= "" &&
                          state.newPw /= state.newPw2
                      in
                        HH.div
                          [ HP.id "matchHelp"
                          , HP.classes
                              [ HB.formText
                              , if mismatch then HB.textDanger else HB.textMuted
                              ]
                          ]
                          [ HH.text
                              ( if mismatch then translate
                                  (label :: _ "prof_passwordMismatch")
                                  state.translator
                                else ""
                              )
                          ]
                    ]
                ]
            , HH.div [ HP.classes [ HB.modalFooter ] ]
                [ HH.button
                    [ HP.classes [ HB.btn, HB.btnLight ]
                    , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                    ]
                    [ HH.text $ translate (label :: _ "prof_close") state.translator ]
                , let
                    canUpdate = length state.newPw >= 8 && state.newPw == state.newPw2
                  in
                    HH.button
                      [ HP.id "updatePwBtn"
                      , HP.classes
                          [ HB.btn
                          , HB.btnDanger
                          , if not canUpdate then HB.disabled else ClassName ""
                          ]
                      , HE.onClick (const UpdatePassword)
                      ]
                      [ HH.text $ translate (label :: _ "prof_updatePassword")
                          state.translator
                      ]
                ]
            ]
        ]
    ]

groupListItem :: forall w. FullUserRoleDto -> HH.HTML w Action
groupListItem fullUserRoleDto = listItem (getGroupName fullUserRoleDto)
  (show $ getUserRole fullUserRoleDto)
  ( if getUserRole fullUserRoleDto == Admin then HB.textBgPrimary
    else HB.textBgSecondary
  )

handleAppError
  :: forall act slots msg m
   . MonadStore Store.Action Store.Store m
  => AppError
  -> H.HalogenM State act slots msg m Unit
handleAppError appError = do
  Store.addError appError
