-- | Modal component for viewing paragraph revision history.
-- | Replaces the cramped popover interface with a spacious modal dialog.

module FPO.UI.Modals.ParagraphHistoryModal
  ( Input
  , Output(..)
  , Query(..)
  , paragraphHistoryModal
  ) where

import Prelude
import Data.Function (identity)

import Data.Array (head, length, null)
import Data.DateTime (Date, adjust)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..), Minutes)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset, nowDateTime)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getTextElemHistory)
import FPO.Data.Store as Store
import FPO.Data.Time (dateToDatetime, formatAbsoluteTimeDetailed)
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.TextElement as TE
import FPO.Translations.Labels (Labels)
import FPO.Translations.Translator (fromFpoTranslator)
import Data.Array (tail) as Array
import FPO.Util (handleKeyDownEscape)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB
import Parsing (runParserT)
import Simple.I18n.Translator (Translator, label, translate)

-- | Version representation for display
type Version =
  { author :: DH.User
  , identifier :: Maybe Int
  , timestamp :: DD.DocDate
  }

-- | Input to the modal
type Input =
  { documentID :: DH.DocumentID
  , textElementID :: TE.TextElementID
  , paragraphTitle :: String
  }

-- | Output events from the modal
data Output
  = ViewVersion Int (Maybe Int)
  | CompareVersion Int (Maybe Int)
  | Closed

-- | Query interface for parent components
data Query a
  = Close a

-- | Internal actions
data Action
  = Init
  | Receive (Connected Store.Store Input)
  | LoadHistory
  | ApplyDateFilter
  | ClearFilters
  | SelectView Version
  | SelectCompare Version
  | CloseModal
  | UpdateFromDate String
  | UpdateToDate String
  | ToggleSubmenu (Maybe Int)

-- | Component state
type State =
  { documentID :: DH.DocumentID
  , textElementID :: TE.TextElementID
  , paragraphTitle :: String
  , versions :: Array Version
  , loading :: Boolean
  , error :: Maybe String
  , fromDate :: Maybe Date
  , toDate :: Maybe Date
  , fromDateInput :: String
  , toDateInput :: String
  , upToDateVersion :: Maybe Version
  , timezoneOffset :: Maybe Minutes
  , translator :: Translator Labels
  , showSubmenu :: Maybe (Maybe Int)
  }

-- | Maximum versions to load at once
maxVersionsToLoad :: Int
maxVersionsToLoad = 200

paragraphHistoryModal
  :: forall m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
paragraphHistoryModal = connect (selectEq identity) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< Receive
      , handleQuery = handleQuery
      }
  }

initialState :: Connected Store.Store Input -> State
initialState { context: store, input } =
  { documentID: input.documentID
  , textElementID: input.textElementID
  , paragraphTitle: input.paragraphTitle
  , versions: []
  , loading: false
  , error: Nothing
  , fromDate: Nothing
  , toDate: Nothing
  , fromDateInput: ""
  , toDateInput: ""
  , upToDateVersion: Nothing
  , timezoneOffset: Nothing
  , translator: fromFpoTranslator store.translator
  , showSubmenu: Nothing
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.div
        [ HP.classes [ HB.modal, HB.fade, HB.show ]
        , HP.id "paragraphHistoryModal"
        , HP.attr (HH.AttrName "data-bs-backdrop") "static"
        , HP.attr (HH.AttrName "data-bs-keyboard") "false"
        , HP.attr (HH.AttrName "tabindex") "-1"
        , HP.attr (HH.AttrName "aria-hidden") "false"
        , HP.attr (HH.AttrName "aria-labelledby") "paragraphHistoryModalLabel"
        , HP.style "display: block;"
        , HP.tabIndex 0
        , HE.onKeyDown $ handleKeyDownEscape CloseModal CloseModal
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "modal-dialog", HH.ClassName "modal-lg" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "modal-content" ] ]
                [ renderHeader state
                , renderBody state
                , renderFooter state
                ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "modal-backdrop", HH.ClassName "show" ]
        , HE.onClick $ const CloseModal
        ]
        []
    ]

renderHeader :: forall slots m. State -> H.ComponentHTML Action slots m
renderHeader state =
  HH.div
    [ HP.classes [ HH.ClassName "modal-header" ] ]
    [ HH.h5
        [ HP.classes [ HH.ClassName "modal-title" ]
        , HP.id "paragraphHistoryModalLabel"
        ]
        [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-clock-history", HB.me2 ] ] []
        , HH.text $ translate (label :: _ "modal_paragraphHistory_title") state.translator
        , HH.small [ HP.classes [ HB.textMuted, HB.ms2 ] ]
            [ HH.text $ " - " <> state.paragraphTitle ]
        ]
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btnClose ]
        , HP.attr (HH.AttrName "aria-label") "Close"
        , HE.onClick $ const CloseModal
        ]
        []
    ]

renderBody :: forall slots m. State -> H.ComponentHTML Action slots m
renderBody state =
  HH.div
    [ HP.classes [ HH.ClassName "modal-body" ] ]
    [ -- Date filter section
      renderDateFilters state
      -- Error message
    , case state.error of
        Just err ->
          HH.div
            [ HP.classes [ HB.alert, HB.alertDanger, HB.mt3 ] ]
            [ HH.text err ]
        Nothing -> HH.text ""
      -- Loading spinner
    , if state.loading then
        HH.div [ HP.classes [ HB.textCenter, HB.my4 ] ]
          [ HH.span [ HP.classes [ HB.spinnerBorder, HB.textPrimary ] ] [] ]
      else
        -- Version list
        renderVersionList state
    ]

renderDateFilters :: forall slots m. State -> H.ComponentHTML Action slots m
renderDateFilters state =
  HH.div
    [ HP.classes [ HB.card, HB.mb3 ] ]
    [ HH.div
        [ HP.classes [ HB.cardBody, HB.py2 ] ]
        [ HH.div
            [ HP.classes [ HB.row, HB.gx3, HB.alignItemsEnd ] ]
            [ -- From date
              HH.div
                [ HP.classes [ HB.colMd4 ] ]
                [ HH.label
                    [ HP.classes [ HB.formLabel, HB.small, HB.mb1 ] ]
                    [ HH.text $ translate (label :: _ "common_from") state.translator ]
                , HH.input
                    [ HP.type_ HP.InputDate
                    , HP.classes [ HB.formControl, HB.formControlSm ]
                    , HP.value state.fromDateInput
                    , HE.onValueInput UpdateFromDate
                    ]
                ]
              -- To date
            , HH.div
                [ HP.classes [ HB.colMd4 ] ]
                [ HH.label
                    [ HP.classes [ HB.formLabel, HB.small, HB.mb1 ] ]
                    [ HH.text $ translate (label :: _ "common_to") state.translator ]
                , HH.input
                    [ HP.type_ HP.InputDate
                    , HP.classes [ HB.formControl, HB.formControlSm ]
                    , HP.value state.toDateInput
                    , HE.onValueInput UpdateToDate
                    ]
                ]
              -- Buttons
            , HH.div
                [ HP.classes [ HB.colMd4 ] ]
                [ HH.div
                    [ HP.classes [ HB.btnGroup, HB.w100 ] ]
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.classes [ HB.btn, HB.btnPrimary, HB.btnSm ]
                        , HE.onClick $ const ApplyDateFilter
                        ]
                        [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-search", HB.me1 ] ] []
                        , HH.text $ translate (label :: _ "common_search") state.translator
                        ]
                    , HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.classes [ HB.btn, HB.btnOutlineSecondary, HB.btnSm ]
                        , HE.onClick $ const ClearFilters
                        ]
                        [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-x-circle", HB.me1 ] ] []
                        , HH.text $ translate (label :: _ "common_clear") state.translator
                        ]
                    ]
                ]
            ]
        ]
    ]

renderVersionList :: forall slots m. State -> H.ComponentHTML Action slots m
renderVersionList state =
  if null state.versions then
    HH.div
      [ HP.classes [ HB.textCenter, HB.textMuted, HB.py4 ] ]
      [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-inbox", HB.fs1, HB.mb3, HB.dBlock ] ] []
      , HH.text $ translate (label :: _ "modal_noHistory") state.translator
      ]
  else
    HH.div
      [ HP.classes [ HB.listGroup ]
      , HP.style "max-height: 400px; overflow-y: auto;"
      ]
      ( map (renderVersionItem state) state.versions
          <> versionLimitNotice
      )
  where
  versionLimitNotice =
    if length state.versions >= maxVersionsToLoad then
      [ HH.div
          [ HP.classes [ HB.listGroupItem, HB.textCenter, HB.textMuted, HB.small ] ]
          [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-info-circle", HB.me1 ] ] []
          , HH.text $ translate (label :: _ "toc_full") state.translator
          ]
      ]
    else
      []

renderVersionItem :: forall slots m. State -> Version -> H.ComponentHTML Action slots m
renderVersionItem state version =
  let
    isCurrentVersion = version.identifier == Nothing
    isSelected = state.showSubmenu == Just version.identifier
    itemClasses =
      [ HB.listGroupItem
      , HB.listGroupItemAction
      , HB.dFlex
      , HB.justifyContentBetween
      , HB.alignItemsCenter
      ]
        <> (if isCurrentVersion then [ HB.listGroupItemPrimary ] else [])
  in
    HH.div
      [ HP.classes itemClasses
      , HE.onClick $ const $ ToggleSubmenu version.identifier
      , HP.style "cursor: pointer;"
      ]
      [ HH.div
          [ HP.classes [ HB.dFlex, HB.alignItemsCenter ] ]
          [ HH.i
              [ HP.classes
                  [ HB.bi
                  , HH.ClassName $ if isCurrentVersion then "bi-check-circle-fill" else "bi-clock-history"
                  , HB.me2
                  , if isCurrentVersion then HB.textSuccess else HB.textSecondary
                  ]
              ]
              []
          , HH.div_
              [ HH.div [ HP.classes [ HB.fwMedium ] ]
                  [ HH.text $
                      formatAbsoluteTimeDetailed state.timezoneOffset
                        (DD.docDateToDateTime version.timestamp)
                  ]
              , HH.small [ HP.classes [ HB.textMuted ] ]
                  [ HH.text $
                      translate (label :: _ "common_by") state.translator
                        <> " "
                        <> DH.getUserName version.author
                  ]
              ]
          ]
      , HH.div
          [ HP.classes [ HB.dFlex, HB.alignItemsCenter ] ]
          [ if isCurrentVersion then
              HH.span
                [ HP.classes [ HB.badge, HB.bgSuccess, HB.me2 ] ]
                [ HH.text $ translate (label :: _ "modal_currentVersion") state.translator ]
            else
              HH.text ""
          , if isSelected then
              HH.div
                [ HP.classes [ HB.btnGroup, HB.btnGroupSm ] ]
                [ HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnOutlinePrimary ]
                    , HE.onClick $ const $ SelectView version
                    ]
                    [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-eye", HB.me1 ] ] []
                    , HH.text $ translate (label :: _ "editor_viewVersion") state.translator
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.classes [ HB.btn, HB.btnOutlineSecondary ]
                    , HE.onClick $ const $ SelectCompare version
                    ]
                    [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-file-diff", HB.me1 ] ] []
                    , HH.text $ translate (label :: _ "editor_compareVersion") state.translator
                    ]
                ]
            else
              HH.i
                [ HP.classes [ HB.bi, HH.ClassName "bi-chevron-right", HB.textMuted ] ]
                []
          ]
      ]

renderFooter :: forall slots m. State -> H.ComponentHTML Action slots m
renderFooter state =
  HH.div
    [ HP.classes [ HH.ClassName "modal-footer" ] ]
    [ HH.div
        [ HP.classes [ HB.textMuted, HB.small, HB.meAuto ] ]
        [ HH.text $
            show (length state.versions)
              <> " "
              <> translate (label :: _ "modal_versionsFound") state.translator
        ]
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btn, HB.btnSecondary ]
        , HE.onClick $ const CloseModal
        ]
        [ HH.text $ translate (label :: _ "common_close") state.translator ]
    ]

handleAction
  :: forall slots m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => Action
  -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  Init -> do
    offset <- liftEffect getTimezoneOffset
    H.modify_ _ { timezoneOffset = Just offset }
    handleAction LoadHistory

  Receive { context: store, input } -> do
    H.modify_ _
      { translator = fromFpoTranslator store.translator
      , documentID = input.documentID
      , textElementID = input.textElementID
      , paragraphTitle = input.paragraphTitle
      }

  LoadHistory -> do
    state <- H.get
    H.modify_ _ { loading = true, error = Nothing }

    let
      after =
        case state.fromDate of
          Nothing -> Nothing
          Just val -> Just (DD.DocDate $ dateToDatetime val)
      before =
        case state.toDate of
          Nothing -> Nothing
          -- dateToDateTime assumes time of 0:00, so shift by 1 day to include entire day
          Just val -> Just
            ( DD.DocDate $ fromMaybe (dateToDatetime val) $ adjust (Days 1.0)
                (dateToDatetime val)
            )

    history <- getTextElemHistory state.documentID state.textElementID before after Nothing

    case history of
      Left _ -> do
        H.modify_ _
          { loading = false
          , error = Just "Failed to load history"
          , versions = []
          }
      Right h -> do
        let
          versions = map
            ( \hEntry ->
                { identifier: Just (TE.getHistoryElementID hEntry)
                , timestamp: TE.getHistoryElementTimestamp hEntry
                , author: TE.getHistoryElementAuthor hEntry
                }
            )
            (TE.getTEHsFromFTEH h)

        -- Determine up-to-date version
        upToDate <- case before of
          Just _ -> do
            temp <- getTextElemHistory state.documentID state.textElementID Nothing Nothing (Just 1)
            case temp of
              Left _ ->
                pure
                  { identifier: Nothing
                  , timestamp: DD.genericDocDate
                  , author: DH.U { identifier: "", name: "" }
                  }
              Right e ->
                case head (TE.getTEHsFromFTEH e) of
                  Nothing ->
                    pure
                      { identifier: Nothing
                      , timestamp: DD.genericDocDate
                      , author: DH.U { identifier: "", name: "" }
                      }
                  Just val ->
                    pure
                      { identifier: Just (TE.getHistoryElementID val)
                      , timestamp: TE.getHistoryElementTimestamp val
                      , author: TE.getHistoryElementAuthor val
                      }
          Nothing -> case head versions of
            Just entry -> pure entry
            Nothing ->
              pure
                { identifier: Nothing
                , timestamp: DD.genericDocDate
                , author: DH.U { identifier: "", name: "" }
                }

        -- Mark current version in list
        let
          markedVersions = case head versions of
            Just entry ->
              if upToDate.identifier == entry.identifier then
                [ { identifier: Nothing
                  , timestamp: entry.timestamp
                  , author: entry.author
                  }
                ] <> (fromMaybe [] $ Array.tail versions)
              else
                versions
            Nothing -> versions

        H.modify_ _
          { loading = false
          , versions = markedVersions
          , upToDateVersion = Just upToDate
          }

  ApplyDateFilter -> do
    handleAction LoadHistory

  ClearFilters -> do
    H.modify_ _
      { fromDate = Nothing
      , toDate = Nothing
      , fromDateInput = ""
      , toDateInput = ""
      }
    handleAction LoadHistory

  UpdateFromDate input -> do
    result <- runParserT input DD.shortDateParser
    let
      newDate = case result of
        Left _ -> Nothing
        Right date -> Just date
    H.modify_ _ { fromDate = newDate, fromDateInput = input }

  UpdateToDate input -> do
    result <- runParserT input DD.shortDateParser
    let
      newDate = case result of
        Left _ -> Nothing
        Right date -> Just date
    H.modify_ _ { toDate = newDate, toDateInput = input }

  SelectView version -> do
    state <- H.get
    H.raise $ ViewVersion state.textElementID version.identifier

  SelectCompare version -> do
    state <- H.get
    H.raise $ CompareVersion state.textElementID version.identifier

  ToggleSubmenu versionId -> do
    state <- H.get
    if state.showSubmenu == Just versionId then
      H.modify_ _ { showSubmenu = Nothing }
    else
      H.modify_ _ { showSubmenu = Just versionId }

  CloseModal -> do
    H.raise Closed

handleQuery
  :: forall slots m a
   . Query a
  -> H.HalogenM State Action slots Output m (Maybe a)
handleQuery = case _ of
  Close a -> do
    H.raise Closed
    pure $ Just a
