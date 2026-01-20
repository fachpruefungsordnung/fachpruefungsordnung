-- | Modal component for viewing document-wide revision history.
-- | Shows both tree revisions (structure changes) and text revisions (content changes).

module FPO.UI.Modals.DocumentHistoryModal
  ( Input
  , Output(..)
  , Query(..)
  , documentHistoryModal
  ) where

import Prelude
import Data.Function (identity)

import Data.Array (length, mapWithIndex, null)
import Data.DateTime (Date, adjust)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..), Minutes)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getDocumentHistory)
import FPO.Data.Store as Store
import FPO.Data.Time (dateToDatetime, formatAbsoluteTimeDetailed)
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.DocumentHistory as DHist
import FPO.Translations.Labels (Labels)
import FPO.Translations.Translator (fromFpoTranslator)
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

-- | Input to the modal
type Input =
  { documentID :: DH.DocumentID
  , documentName :: String
  }

-- | Output events from the modal
data Output
  = ViewTreeRevision Int
  | ViewTextRevision Int Int -- textElementID, revisionID
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
  | SelectTreeRevision DHist.TreeRevisionHeader
  | SelectTextRevision Int DHist.TreeRevisionHeader -- textElementID, header
  | CloseModal
  | UpdateBeforeDate String
  | ToggleSubmenu Int -- index in the list

-- | Component state
type State =
  { documentID :: DH.DocumentID
  , documentName :: String
  , historyItems :: Array DHist.DocumentHistoryItem
  , loading :: Boolean
  , error :: Maybe String
  , beforeDate :: Maybe Date
  , beforeDateInput :: String
  , timezoneOffset :: Maybe Minutes
  , translator :: Translator Labels
  , selectedIndex :: Maybe Int
  }

-- | Maximum versions to load at once
maxVersionsToLoad :: Int
maxVersionsToLoad = 200

documentHistoryModal
  :: forall m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
documentHistoryModal = connect (selectEq identity) $ H.mkComponent
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
  , documentName: input.documentName
  , historyItems: []
  , loading: false
  , error: Nothing
  , beforeDate: Nothing
  , beforeDateInput: ""
  , timezoneOffset: Nothing
  , translator: fromFpoTranslator store.translator
  , selectedIndex: Nothing
  }

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state =
  HH.div_
    [ HH.div
        [ HP.classes [ HB.modal, HB.fade, HB.show ]
        , HP.id "documentHistoryModal"
        , HP.attr (HH.AttrName "data-bs-backdrop") "static"
        , HP.attr (HH.AttrName "data-bs-keyboard") "false"
        , HP.attr (HH.AttrName "tabindex") "-1"
        , HP.attr (HH.AttrName "aria-hidden") "false"
        , HP.attr (HH.AttrName "aria-labelledby") "documentHistoryModalLabel"
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
        , HP.id "documentHistoryModalLabel"
        ]
        [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-clock-history", HB.me2 ] ] []
        , HH.text $ translate (label :: _ "modal_documentHistory_title") state.translator
        , HH.small [ HP.classes [ HB.textMuted, HB.ms2 ] ]
            [ HH.text $ " - " <> state.documentName ]
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
      renderDateFilter state
      -- Legend
    , renderLegend state
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
        -- History list
        renderHistoryList state
    ]

renderDateFilter :: forall slots m. State -> H.ComponentHTML Action slots m
renderDateFilter state =
  HH.div
    [ HP.classes [ HB.card, HB.mb3 ] ]
    [ HH.div
        [ HP.classes [ HB.cardBody, HB.py2 ] ]
        [ HH.div
            [ HP.classes [ HB.row, HB.gx3, HB.alignItemsEnd ] ]
            [ -- Before date (document history only supports "before")
              HH.div
                [ HP.classes [ HB.colMd6 ] ]
                [ HH.label
                    [ HP.classes [ HB.formLabel, HB.small, HB.mb1 ] ]
                    [ HH.text $ translate (label :: _ "modal_historyBefore") state.translator ]
                , HH.input
                    [ HP.type_ HP.InputDate
                    , HP.classes [ HB.formControl, HB.formControlSm ]
                    , HP.value state.beforeDateInput
                    , HE.onValueInput UpdateBeforeDate
                    ]
                ]
              -- Buttons
            , HH.div
                [ HP.classes [ HB.colMd6 ] ]
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

renderLegend :: forall slots m. State -> H.ComponentHTML Action slots m
renderLegend state =
  HH.div
    [ HP.classes [ HB.dFlex, HB.gap3, HB.mb3, HB.small ] ]
    [ HH.div
        [ HP.classes [ HB.dFlex, HB.alignItemsCenter ] ]
        [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-diagram-3", HB.me1, HB.textInfo ] ] []
        , HH.text $ translate (label :: _ "modal_treeRevision") state.translator
        ]
    , HH.div
        [ HP.classes [ HB.dFlex, HB.alignItemsCenter ] ]
        [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-file-text", HB.me1, HB.textSuccess ] ] []
        , HH.text $ translate (label :: _ "modal_textRevision") state.translator
        ]
    ]

renderHistoryList :: forall slots m. State -> H.ComponentHTML Action slots m
renderHistoryList state =
  if null state.historyItems then
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
      ( mapWithIndex (renderHistoryItem state) state.historyItems
          <> versionLimitNotice
      )
  where
  versionLimitNotice =
    if length state.historyItems >= maxVersionsToLoad then
      [ HH.div
          [ HP.classes [ HB.listGroupItem, HB.textCenter, HB.textMuted, HB.small ] ]
          [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-info-circle", HB.me1 ] ] []
          , HH.text $ translate (label :: _ "toc_full") state.translator
          ]
      ]
    else
      []

renderHistoryItem :: forall slots m. State -> Int -> DHist.DocumentHistoryItem -> H.ComponentHTML Action slots m
renderHistoryItem state index item =
  let
    header = DHist.getItemHeader item
    isTreeItem = DHist.isTreeItem item
    isSelected = state.selectedIndex == Just index
    itemClasses =
      [ HB.listGroupItem
      , HB.listGroupItemAction
      , HB.dFlex
      , HB.justifyContentBetween
      , HB.alignItemsCenter
      ]
    iconClass = if isTreeItem then "bi-diagram-3" else "bi-file-text"
    iconColor = if isTreeItem then HB.textInfo else HB.textSuccess
  in
    HH.div
      [ HP.classes itemClasses
      , HE.onClick $ const $ ToggleSubmenu index
      , HP.style "cursor: pointer;"
      ]
      [ HH.div
          [ HP.classes [ HB.dFlex, HB.alignItemsCenter ] ]
          [ HH.i
              [ HP.classes [ HB.bi, HH.ClassName iconClass, HB.me2, iconColor ] ]
              []
          , HH.div_
              [ HH.div [ HP.classes [ HB.fwMedium ] ]
                  [ HH.text $
                      formatAbsoluteTimeDetailed state.timezoneOffset
                        (DD.docDateToDateTime $ DHist.getTreeRevisionTimestamp header)
                  ]
              , HH.small [ HP.classes [ HB.textMuted ] ]
                  [ HH.text $
                      translate (label :: _ "common_by") state.translator
                        <> " "
                        <> DH.getUserName (DHist.getTreeRevisionAuthor header)
                  , case DHist.getTextElementID item of
                      Just textId ->
                        HH.span_
                          [ HH.text $ " | "
                          , HH.span [ HP.classes [ HB.fwMedium ] ]
                              [ HH.text $ translate (label :: _ "modal_paragraph") state.translator
                                  <> " #"
                                  <> show textId
                              ]
                          ]
                      Nothing -> HH.text ""
                  ]
              ]
          ]
      , HH.div
          [ HP.classes [ HB.dFlex, HB.alignItemsCenter ] ]
          [ HH.span
              [ HP.classes
                  [ HB.badge
                  , if isTreeItem then HB.bgInfo else HB.bgSuccess
                  , HB.me2
                  ]
              ]
              [ HH.text $
                  if isTreeItem then
                    translate (label :: _ "modal_structure") state.translator
                  else
                    translate (label :: _ "modal_content") state.translator
              ]
          , if isSelected then
              HH.button
                [ HP.type_ HP.ButtonButton
                , HP.classes [ HB.btn, HB.btnOutlinePrimary, HB.btnSm ]
                , HE.onClick $ const $ selectAction item header
                ]
                [ HH.i [ HP.classes [ HB.bi, HH.ClassName "bi-eye", HB.me1 ] ] []
                , HH.text $ translate (label :: _ "editor_viewVersion") state.translator
                ]
            else
              HH.i
                [ HP.classes [ HB.bi, HH.ClassName "bi-chevron-right", HB.textMuted ] ]
                []
          ]
      ]
  where
  selectAction itm hdr =
    case DHist.getTextElementID itm of
      Just textId -> SelectTextRevision textId hdr
      Nothing -> SelectTreeRevision hdr

renderFooter :: forall slots m. State -> H.ComponentHTML Action slots m
renderFooter state =
  HH.div
    [ HP.classes [ HH.ClassName "modal-footer" ] ]
    [ HH.div
        [ HP.classes [ HB.textMuted, HB.small, HB.meAuto ] ]
        [ HH.text $
            show (length state.historyItems)
              <> " "
              <> translate (label :: _ "modal_changesFound") state.translator
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
      , documentName = input.documentName
      }

  LoadHistory -> do
    state <- H.get
    H.modify_ _ { loading = true, error = Nothing }

    let
      before =
        case state.beforeDate of
          Nothing -> Nothing
          Just val -> Just
            ( DD.DocDate $ fromMaybe (dateToDatetime val) $ adjust (Days 1.0)
                (dateToDatetime val)
            )

    history <- getDocumentHistory state.documentID before (Just maxVersionsToLoad)

    case history of
      Left _ -> do
        H.modify_ _
          { loading = false
          , error = Just "Failed to load document history"
          , historyItems = []
          }
      Right h -> do
        H.modify_ _
          { loading = false
          , historyItems = DHist.getHistoryItems h
          }

  ApplyDateFilter -> do
    handleAction LoadHistory

  ClearFilters -> do
    H.modify_ _
      { beforeDate = Nothing
      , beforeDateInput = ""
      }
    handleAction LoadHistory

  UpdateBeforeDate input -> do
    result <- runParserT input DD.shortDateParser
    let
      newDate = case result of
        Left _ -> Nothing
        Right date -> Just date
    H.modify_ _ { beforeDate = newDate, beforeDateInput = input }

  SelectTreeRevision header -> do
    let revisionID = DHist.getTreeRevisionIdentifier header
    H.raise $ ViewTreeRevision revisionID

  SelectTextRevision textElementID header -> do
    let revisionID = DHist.getTreeRevisionIdentifier header
    H.raise $ ViewTextRevision textElementID revisionID

  ToggleSubmenu index -> do
    state <- H.get
    if state.selectedIndex == Just index then
      H.modify_ _ { selectedIndex = Nothing }
    else
      H.modify_ _ { selectedIndex = Just index }

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
