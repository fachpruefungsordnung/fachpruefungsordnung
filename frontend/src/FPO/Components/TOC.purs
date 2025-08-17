module FPO.Components.TOC where

import Prelude

import Data.Array (concat, last, length, mapWithIndex, snoc, unsnoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getDocumentHeader, postJson)
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..))
import FPO.Dto.PostTextDto (PostTextDto(..))
import FPO.Dto.PostTextDto as PostTextDto
import FPO.Translations.Translator (fromFpoTranslator)
import FPO.Translations.Util (FPOState)
import FPO.Types (ShortendTOCEntry, TOCEntry, TOCTree, shortenTOC)
import FPO.Util (isPrefixOf, prependIf)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)
import Web.UIEvent.KeyboardEvent as KE

type Input = DH.DocumentID

data Output
  = ChangeSection String Int
  | AddNode Path (Tree TOCEntry)
  | DeleteNode Path
  | ReorderItems { from :: Path, to :: Path }
  | RenameNode { path :: Path, newName :: String }

type Path = Array Int

data Action
  = Init
  | Receive (Connected Store.Store Input)
  | DoNothing
  | JumpToSection String Int
  | ToggleAddMenu Path
  | CreateNewSubsection Path
  | CreateNewSection Path
  -- | Section renaming
  | StartRenameSection String Path
  | RenameSection String
  | ApplyRenameSection
  | CancelRenameSection
  -- | Section deletion
  | RequestDeleteSection { path :: Path, kind :: EntityKind, title :: String }
  | CancelDeleteSection
  | ConfirmDeleteSection Path
  -- | Drag and Drop
  | StartDrag Path
  | HighlightDropZone Path DragEvent
  | ClearDropZones
  | CompleteDrop Path

data EntityKind = Section | Paragraph

data Query a = ReceiveTOCs (TOCTree) a

type RenameState = { title :: String, path :: Path }

type State = FPOState
  ( docID :: DH.DocumentID
  , documentName :: String
  , tocEntries :: RootTree ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  , showAddMenu :: Array Int
  , dragState :: Maybe { draggedId :: Path, hoveredId :: Path }
  , requestDelete :: Maybe { path :: Path, kind :: EntityKind, title :: String }
  , renameSection :: Maybe RenameState
  )

tocview
  :: forall m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
tocview = connect (selectEq identity) $ H.mkComponent
  { initialState: \{ context: store, input } ->
      { documentName: ""
      , tocEntries: Empty
      , mSelectedTocEntry: Nothing
      , showAddMenu: [ -1 ]
      , docID: input
      , dragState: Nothing
      , requestDelete: Nothing
      , translator: fromFpoTranslator store.translator
      , renameSection: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , receive = Just <<< Receive
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state =
    HH.div_ $
      renderDeleteModal
        <>
          ( rootTreeToHTML state state.documentName state.showAddMenu
              state.mSelectedTocEntry
              state.tocEntries
          )
    where
    renderDeleteModal = case state.requestDelete of
      Nothing -> []
      Just { path, kind, title } ->
        [ deleteConfirmationModal
            state.translator
            path
            (const title)
            CancelDeleteSection
            ConfirmDeleteSection
            (kindToString kind)
        ]

    kindToString :: EntityKind -> String
    kindToString = case _ of
      Section -> translate (label :: _ "toc_section") state.translator
      Paragraph -> translate (label :: _ "toc_paragraph") state.translator

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Init -> do
      s <- H.get
      mDoc <- getDocumentHeader s.docID
      let
        docName = case mDoc of
          Left _ -> "" -- TODO error handling
          Right doc -> DH.getName doc
      H.modify_ \st -> do
        st { documentName = docName }

    DoNothing -> do
      pure unit

    JumpToSection title id -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just id }
      H.raise (ChangeSection title id)

    ToggleAddMenu path -> do
      H.modify_ \state ->
        state
          { showAddMenu =
              if state.showAddMenu == [ -1 ] || state.showAddMenu /= path then path
              else [ -1 ]
          }

    CreateNewSubsection path -> do
      H.modify_ _ { showAddMenu = [ -1 ] }
      s <- H.get
      gotRes <- postJson PostTextDto.decodePostTextDto
        ("/docs/" <> show s.docID <> "/text")
        ( PostTextDto.encodePostTextDto
            (PostTextDto { identifier: 0, kind: "new Text" })
        )
      case gotRes of
        Left _ -> pure unit -- TODO error handling
        Right dto -> do
          let
            newEntry =
              Leaf
                { title: "New Subsection"
                , node:
                    { id: PostTextDto.getID dto
                    , name: "New Subsection"
                    , paraID: 0 -- to be implemented later
                    , newMarkerNextID: 0
                    , markers: []
                    }
                }
          H.raise (AddNode path newEntry)

    CreateNewSection path -> do
      H.modify_ \st ->
        st { showAddMenu = [ -1 ] }
      let
        newEntry = Node
          { title: "New Section"
          , children: []
          , header: { headerKind: "section", headerType: "section" }
          }
      H.raise (AddNode path newEntry)

    RequestDeleteSection entity -> do
      H.modify_ _ { requestDelete = Just entity }

    CancelDeleteSection -> do
      H.modify_ _ { requestDelete = Nothing }

    ConfirmDeleteSection path -> do
      H.raise (DeleteNode path)
      H.modify_ _ { requestDelete = Nothing }

    StartRenameSection title path -> do
      H.modify_ _ { renameSection = Just { title, path } }

    RenameSection name -> do
      H.modify_ \state ->
        case state.renameSection of
          Just { path } ->
            state { renameSection = Just { title: name, path } }
          Nothing -> state

    ApplyRenameSection -> do
      s <- H.get
      case s.renameSection of
        Just { title, path } -> do
          -- let newTocEntries = changeNodeName path title s.tocEntries
          -- TODO raise action to update structure
          -- H.modify_ _ { tocEntries = newTocEntries, renameSection = Nothing }
          H.raise (RenameNode { path, newName: title })
        Nothing -> do
          pure unit -- liftEffect $ log "ApplyRenameSection: No renameSection found"
      H.modify_ _ { renameSection = Nothing }

    CancelRenameSection -> do
      H.modify_ _ { renameSection = Nothing }

    StartDrag id -> do
      H.modify_ _ { dragState = Just { draggedId: id, hoveredId: id } }

    HighlightDropZone targetId e -> do
      -- We need to prevent the default behavior to allow dropping.
      H.liftEffect $ preventDefault (toEvent e)
      H.modify_ \s -> s { dragState = map (_ { hoveredId = targetId }) s.dragState }

    ClearDropZones -> do
      H.modify_ _ { dragState = Nothing }

    CompleteDrop targetId -> do
      state <- H.get
      case state.dragState of
        Just { draggedId } -> do
          if isPrefixOf draggedId targetId then
            -- If the dragged item is a prefix of the target, we do not allow dropping.
            pure unit
          else do
            H.raise (ReorderItems { from: draggedId, to: targetId })
          handleAction ClearDropZones
        Nothing -> pure unit

    Receive { context: store } -> do
      H.modify_ _
        { translator = fromFpoTranslator store.translator
        }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of
    ReceiveTOCs entries a -> do
      let
        shortendEntries = map shortenTOC entries
      H.modify_ \state ->
        state
          { tocEntries = shortendEntries }
      pure (Just a)

  rootTreeToHTML
    :: forall slots
     . State
    -> String
    -> Array Int
    -> Maybe Int
    -> RootTree ShortendTOCEntry
    -> Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ _ _ Empty = []
  rootTreeToHTML state docName menuPath mSelectedTocEntry (RootTree { children }) =
    [ HH.div
        [ HP.classes [ HB.bgWhite, HB.shadow ] ]
        [ HH.div
            [ HP.classes [ HB.borderBottom, HB.ms1, HB.me2 ] ]
            [ HH.div
                [ HP.classes
                    [ HB.dFlex, HB.alignItemsCenter, HB.justifyContentBetween ]
                ]
                [ HH.span
                    [ HP.classes [ HB.fwSemibold, HB.textTruncate, HB.fs4, HB.p2 ] ]
                    [ HH.text docName ]
                , renderButtonInterface menuPath [] false Section docName
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-list" ] ]
            ( concat $ mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML state menuPath 1 mSelectedTocEntry [ ix ] child
                )
                children
            )
        ]
    ]

  treeToHTML
    :: forall slots
     . State
    -> Array Int
    -> Int
    -> Maybe Int
    -> Array Int
    -> Tree ShortendTOCEntry
    -> Array (H.ComponentHTML Action slots m)
  treeToHTML state menuPath level mSelectedTocEntry path = case _ of
    Node { title, children } ->
      let
        innerDivClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py1, HB.positionRelative ]
        titleClasses =
          [ HB.textTruncate, HB.flexGrow1, HB.fwBold, HB.fs5 ]
      in
        [ HH.div
            ( [ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] ] <> dragProps
                isRenaming
            )
            [ addDropZone state path
            , HH.div
                [ HP.classes innerDivClasses ]
                [ dragHandle
                , case state.renameSection of
                    Just rs | rs.path == path ->
                      renderInput rs
                    _ ->
                      HH.span
                        [ HP.classes titleClasses
                        , HP.style "align-self: stretch; flex-basis: 0;"
                        , HE.onDoubleClick $ const $ StartRenameSection title path
                        ]
                        [ HH.text title ]
                , renderButtonInterface menuPath path true Section title
                ]
            ]
        ]
          <> concat
            ( mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML state menuPath (level + 1) mSelectedTocEntry
                      (path <> [ ix ])
                      child
                )
                children
            )
          <>
            -- Create a new end drop zone at the end of the section.
            -- It is handled like a normal element during drag and drop detection,
            -- i.e., it has its own path.
            [ addEndDropZone state (snoc path (length children)) level ]
      where
      -- Render input field (editing mode)
      renderInput :: RenameState -> H.ComponentHTML Action slots m
      renderInput rs =
        HH.input
          [ HP.type_ HP.InputText
          , HP.value rs.title
          , HP.classes
              [ HH.ClassName "text-input"
              , HH.ClassName "fw-bold"
              , HH.ClassName "fs-5"
              , HH.ClassName "text-truncate"
              , HH.ClassName "flex-grow-1"
              ]
          , HP.style "min-width: 0; align-self: stretch; flex-basis: 0;"
          , HE.onValueInput RenameSection
          , HE.onBlur $ const ApplyRenameSection
          , HE.onFocusOut $ const ApplyRenameSection
          , HE.onKeyDown \e -> case KE.key e of
              "Enter" -> ApplyRenameSection
              "Escape" -> CancelRenameSection
              _ -> DoNothing
          , HP.autofocus true
          ]

    Leaf { title, node: { id, paraID: _, name: _ } } ->
      let
        selectedClasses =
          if Just id == mSelectedTocEntry then
            [ HH.ClassName "active" ]
          else []
        containerProps =
          ( [ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] <> selectedClasses
            , HP.title ("Jump to section " <> title)
            ] <> dragProps true
          )
        innerDivBaseClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py1, HB.positionRelative ]
        innerDivProps =
          [ HP.classes innerDivBaseClasses
          , HP.style "cursor: pointer;"
          ] <>
            (if level > 0 then [ HE.onClick \_ -> JumpToSection title id ] else [])
      in
        [ HH.div
            containerProps
            [ addDropZone state path
            , HH.div
                innerDivProps
                [ dragHandle
                , HH.span
                    [ HP.classes
                        [ HB.textTruncate, HB.flexGrow1, HB.fwNormal, HB.fs6 ]
                    , HP.style "align-self: stretch; flex-basis: 0;"
                    ]
                    [ HH.text $ prettyTitle title ]
                , HH.div [ HP.classes [ HB.positionRelative ] ]
                    [ deleteSectionButton path Paragraph (prettyTitle title)
                    ]
                ]
            ]
        ]
    where
    -- If the title is of shape "§{<label>:} Name", change it to "§ Name".
    prettyTitle :: String -> String
    prettyTitle title =
      case regex "§\\{[^}]+:\\}\\s*" noFlags of
        Left _err -> title -- fallback - if regex fails, just return the input
        Right pattern -> replace pattern "§ " title

    dragProps draggable =
      [ HP.draggable draggable
      , HE.onDragStart $ const $ StartDrag path
      , HE.onDragOver $ HighlightDropZone path
      , HE.onDrop $ const $ CompleteDrop path
      , HE.onDragEnd $ const $ ClearDropZones
      ]

    dragHandle = HH.span
      [ HP.classes
          [ HH.ClassName "toc-drag-handle", HB.textMuted, HB.me2 ]
      , HP.style ("margin-left: " <> show level <> "rem;")
      ]
      [ HH.text "⋮⋮" ]

    isRenaming =
      case state.renameSection of
        Just { path: renamingPath } -> renamingPath /= path
        Nothing -> true

  -- Helper to check if the current path is the active dropzone.
  -- This is used to highlight the dropzone when dragging an item.
  activeDropzone
    :: State -> Path -> Boolean
  activeDropzone state path =
    case state.dragState of
      Just { draggedId, hoveredId } ->
        hoveredId == path
          &&
            hoveredId /= draggedId
          &&
            not (draggedId `isPrefixOf` path)
          &&
            draggedId /= path
      _ -> false

  -- Helper to check if the current path is the last element of the section,
  -- or the section header (if the section is empty). In this case, we
  -- preview the end dropzone.
  previewEndDropzone
    :: State -> Path -> Boolean
  previewEndDropzone state path =
    case state.dragState of
      Just { draggedId, hoveredId } ->
        let
          -- Check if we are hovering over an empty section
          -- (in this case, the end drop zone is associated with path [..., 0]).
          hoveringEmptySection = last path == Just 0 && hoveredId <> [ 0 ] == path
          -- Check if we are hovering over the end of a section
          -- (in this case, the end drop zone's path is the path of the last item, incremented by 1).
          hoveringSectionEnd = incrementPath hoveredId == path
        in
          hoveredId /= draggedId
            &&
              draggedId /= path
            &&
              (hoveringEmptySection || hoveringSectionEnd)
            &&
              not (draggedId `isPrefixOf` path)
      _ -> false

  -- Checks whether the current path is the active end dropzone.
  -- In this case, the end dropzone is not in preview mode (or even disabled),
  -- but active, and waiting for a drop.
  activeEndDropzone
    :: State -> Path -> Boolean
  activeEndDropzone state path =
    case state.dragState of
      Just { hoveredId, draggedId } ->
        hoveredId == path
          &&
            not (draggedId `isPrefixOf` path)
          &&
            hoveredId /= incrementPath draggedId
      _ -> false

  -- Increments the last element of the path by 1. In other words,
  -- it creates a new path that points to the next item
  -- in the same section/level.
  incrementPath :: Path -> Path
  incrementPath p = case unsnoc p of
    Just { init, last } -> init <> [ last + 1 ]
    Nothing -> [ 0 ]

  -- Creates a drop zone for the current path.
  addDropZone
    :: forall slots. State -> Array Int -> H.ComponentHTML Action slots m
  addDropZone state path = HH.div
    [ HP.classes
        $ prependIf (activeDropzone state path) (H.ClassName "active")
        $ [ H.ClassName "drop-zone" ]
    ]
    []

  -- Creates a drop zone at the end of the section, either active or preview,
  -- depending on the drag state.
  --
  -- TODO: The third parameter, "level", is not considered in the current implementation,
  --       but it could be used to adjust the styling or behavior of the drop zone based on
  --       the section level / depth (for example, to add padding or margin).
  addEndDropZone
    :: forall slots. State -> Array Int -> Int -> H.ComponentHTML Action slots m
  addEndDropZone state path _ =
    HH.div
      ( [ HP.classes
            $ prependIf (activeEndDropzone state path) (H.ClassName "active")
            $ prependIf (previewEndDropzone state path) (H.ClassName "preview")
            $ [ H.ClassName "drop-zone-end" ]
        ] <> dragProps
      )
      []
    where
    dragProps =
      [ HE.onDragStart $ const $ StartDrag path
      , HE.onDragOver $ HighlightDropZone path
      , HE.onDrop $ const $ CompleteDrop path
      , HE.onDragEnd $ const $ ClearDropZones
      , HP.attr (HH.AttrName "data-drop-text") $ translate
          (label :: _ "toc_end_dropzone")
          state.translator
      ]

  -- Creates a delete button for the section.
  deleteSectionButton
    :: forall slots
     . Array Int
    -> EntityKind
    -> String
    -> H.ComponentHTML Action slots m
  deleteSectionButton path kind title =
    HH.button
      [ HP.classes
          [ HB.btn
          , HB.btnDanger
          , HH.ClassName "toc-button"
          , HH.ClassName "toc-add-wrapper"
          ]
      , HE.onClick $ const $ RequestDeleteSection { kind, path, title }
      ]
      [ HH.text "-" ]

  -- Helper to render add button with dropdown, and optional delete button.
  renderButtonInterface
    :: forall slots
     . Array Int
    -> Array Int
    -> Boolean
    -> EntityKind
    -> String
    -> H.ComponentHTML Action slots m
  renderButtonInterface menuPath currentPath renderDeleteBtn kind title =
    HH.div
      [ HP.classes [ HB.positionRelative ] ] $
      [ HH.button
          [ HP.classes
              [ HB.btn
              , HB.btnSuccess
              , HH.ClassName "toc-button"
              , HH.ClassName "toc-add-wrapper"
              ]
          , HE.onClick \_ -> ToggleAddMenu currentPath
          ]
          [ HH.text "+" ]
      ]
        <>
          ( if renderDeleteBtn then [ deleteSectionButton currentPath kind title ]
            else []
          )
        <>
          -- Dropdown menu (only visible when path matches)
          [ if menuPath == currentPath then
              HH.div
                [ HP.classes
                    [ HB.positionAbsolute
                    , HB.bgWhite
                    , HB.border
                    , HB.rounded
                    , HB.shadowSm
                    , HB.py1
                    ]
                , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
                ]
                [ addSectionButton "Unterabschnitt" CreateNewSubsection
                , addSectionButton "Abschnitt" CreateNewSection
                ]
            else
              HH.text ""
          ]
    where
    addSectionButton str act = HH.button
      [ HP.classes
          [ HB.btn
          , HB.btnLink
          , HB.textStart
          , HB.textDecorationNone
          , HB.w100
          , HB.border0
          , HB.textBody
          , HB.dFlex
          , HB.alignItemsCenter
          ]
      , HE.onClick \_ -> act currentPath
      ]
      [ HH.div [ HP.classes [ H.ClassName "bi bi-plus", HB.fs5, HB.me1 ] ] []
      , HH.div [ HP.classes [ HB.fs6 ] ]
          [ HH.text str ]
      ]
