module FPO.Components.TOC where

import Data.Array
  ( concat
  , cons
  , drop
  , last
  , length
  , mapWithIndex
  , snoc
  , take
  , uncons
  , unsnoc
  )
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import FPO.Components.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getDocumentHeader, getTextElemHistory, postJson)
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.TextElement as TE
import FPO.Dto.DocumentDto.TreeDto (Edge(..), RootTree(..), Tree(..))
import FPO.Dto.PostTextDto (PostTextDto(..))
import FPO.Dto.PostTextDto as PostTextDto
import FPO.Page.Home (formatRelativeTime)
import FPO.Translations.Translator (fromFpoTranslator)
import FPO.Translations.Util (FPOState)
import FPO.Types (TOCEntry, TOCTree)
import FPO.Util (isPrefixOf, prependIf)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB
import Prelude
  ( class Eq
  , Unit
  , bind
  , const
  , discard
  , identity
  , map
  , negate
  , not
  , pure
  , show
  , unit
  , when
  , ($)
  , (&&)
  , (+)
  , (-)
  , (/=)
  , (<)
  , (<<<)
  , (<>)
  , (==)
  , (>)
  , (||)
  )
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)

type Input = DH.DocumentID

type Version = { identifier :: Int, timestamp :: DD.DocDate }

data Output
  -- | Opens the editor for some leaf node, that is, a subsection or paragraph.
  = ChangeToLeaf String Int
  -- | Opens the editor for some given node title. Used to rename sections.
  | ChangeToNode Path String
  -- | Used to tell the editor to update the path of the selected node
  --   during title renaming.
  | UpdateNodePosition Path
  | AddNode Path (Tree TOCEntry)
  | DeleteNode Path
  | ReorderItems { from :: Path, to :: Path }
  | ModifyVersion Int (Maybe Int)
  | CompareTo Int Int
  | RenameNode { path :: Path, newName :: String }

type Path = Array Int

type EntityToDelete = { path :: Path, kind :: EntityKind, title :: String }

-- | Leafs can be identified by their ID, while nodes must unfortunately
--   be identified by their path. Because the path can change after drag and drop,
--   we need to update the path accordingly, and might have to tell the editor to
--   account for the path change. This approach is far from ideal, but it is the
--   best we can do with the current architecture.
data SelectedEntity
  = SelLeaf Int
  | SelNode Path String

derive instance eqSelectedEntity :: Eq SelectedEntity

data Action
  = Init
  | Both Action Action
  | Receive (Connected Store.Store Input)
  | DoNothing
  | JumpToLeafSection Int String
  | JumpToNodeSection Path String
  | ToggleAddMenu Path
  | ToggleHistoryMenu (Array Int) Int
  | ToggleHistorySubmenu Int
  | CreateNewSubsection Path
  | CreateNewSection Path
  | OpenVersion Int Int
  | CompareVersion Int Int
  | UpdateVersions DateTime Int
  -- | Section deletion
  | RequestDeleteSection EntityToDelete
  | CancelDeleteSection
  | ConfirmDeleteSection Path
  -- | Drag and Drop
  | StartDrag Path
  | HighlightDropZone Path DragEvent
  | ClearDropZones
  | CompleteDrop Path

data EntityKind = Section | Paragraph

data Query a = ReceiveTOCs (TOCTree) a

type State = FPOState
  ( docID :: DH.DocumentID
  , documentName :: String
  , tocEntries :: RootTree TOCEntry
  , mSelectedTocEntry :: Maybe SelectedEntity
  , now :: Maybe DateTime
  , showAddMenu :: Array Int
  , showHistoryMenu :: Array Int
  , showHistorySubmenu :: Int
  , versions :: Array Version
  , dragState :: Maybe { draggedId :: Path, hoveredId :: Path }
  , requestDelete :: Maybe EntityToDelete
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
      , now: Nothing
      , showAddMenu: [ -1 ]
      , showHistoryMenu: [ -1 ]
      , showHistorySubmenu: -1
      , versions: []
      , docID: input
      , dragState: Nothing
      , requestDelete: Nothing
      , translator: fromFpoTranslator store.translator
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
          ( rootTreeToHTML
              state
              state.documentName
              state.showAddMenu
              state.showHistoryMenu
              state.mSelectedTocEntry
              state.now
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
      now <- liftEffect nowDateTime
      mDoc <- getDocumentHeader s.docID

      let
        docName = case mDoc of
          Left _ -> "" -- TODO error handling
          Right doc -> DH.getName doc
      H.modify_ \st -> do
        st
          { documentName = docName
          , now = Just now
          }

    Both act1 act2 -> do
      handleAction act1
      handleAction act2

    UpdateVersions ts elementID -> do
      s <- H.get
      history <- H.liftAff $ getTextElemHistory s.docID elementID (DD.DocDate ts) 5
      case history of
        Nothing -> do liftEffect $ log "unable to load textElements"
        Just h -> do
          let
            newVersions = map
              ( \hEntry ->
                  { identifier: TE.getHistoryElementID hEntry
                  , timestamp: TE.getHistoryElementTimestamp hEntry
                  }
              )
              (TE.getTEHsFromFTEH h)
          H.modify_ _ { versions = newVersions }

    OpenVersion elementID vID -> do
      H.raise (ModifyVersion elementID (Just vID))

    CompareVersion elementID vID -> do
      H.raise (CompareTo elementID vID)

    DoNothing -> do
      pure unit

    JumpToLeafSection id title -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just $ SelLeaf id }
      H.raise (ChangeToLeaf title id)

    JumpToNodeSection path title -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just $ SelNode path title }
      H.raise (ChangeToNode path title)

    ToggleAddMenu path -> do
      H.modify_ \state ->
        state
          { showAddMenu =
              if state.showAddMenu == [ -1 ] || state.showAddMenu /= path then path
              else [ -1 ]
          }

    ToggleHistoryMenu path elementID -> do
      now <- liftEffect nowDateTime
      handleAction (UpdateVersions now elementID)
      H.modify_ \state ->
        state
          { now = Just now
          , showHistoryMenu =
              if state.showHistoryMenu == [ -1 ] || state.showHistoryMenu /= path then
                path
              else [ -1 ]
          }

    ToggleHistorySubmenu vID -> do
      H.modify_ \state -> state
        { showHistorySubmenu =
            if state.showHistorySubmenu /= vID then vID
            else -1
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
            case state.mSelectedTocEntry of
              Just (SelLeaf _) -> do
                -- If we have a leaf selected, we don't need to update anything as
                -- the identifier is unique and can always be used to find the leaf,
                -- no matter where it moves in the TOC.
                pure unit
              Just (SelNode path title) -> do
                -- If we have a node selected, we might need to update the path
                -- to keep it in sync with the TOC structure, and tell the editor
                -- where exactly the section is now.
                let newPath = adjustPathAfterMove path draggedId
                when (newPath /= path) $ do
                  H.modify_ \s ->
                    s { mSelectedTocEntry = Just (SelNode newPath title) }
                  H.raise (UpdateNodePosition newPath)
              Nothing -> pure unit

            H.raise (ReorderItems { from: draggedId, to: targetId })
          handleAction ClearDropZones
        Nothing -> pure unit
      where
      -- Adjust the path after a move operation.
      -- This is needed as the whole TOC structure might change when moving any element
      -- from any position to another. If a section is selected (which is only identified by its path),
      -- and we move some entities (for example, the section itself, or an entity before, etc.),
      -- we need to adjust the path accordingly to keep it up-to-date with the editor.
      --
      -- Because we really only want to allow these sections at top-level in the future (no nested sections),
      -- we could simplify the logic a bunch, but for now we keep it as is, given that it seems to be reliable
      -- (hopefully ^^).
      adjustPathAfterMove :: Path -> Path -> Path
      adjustPathAfterMove oldPath draggedId
        | oldPath == draggedId = adjustTargetForSelfMove draggedId -- Moving the selected entity itself
        | isPrefixOf draggedId oldPath =
            let
              adjustedTarget = adjustTargetForSelfMove draggedId
              remainingPath = drop (length draggedId) oldPath
            in
              adjustedTarget <> remainingPath -- Moving a parent of selected
      adjustPathAfterMove oldPath draggedId = adjustForSiblingMove oldPath draggedId

      -- When moving an element to a new position, account for its own removal.
      adjustTargetForSelfMove :: Path -> Path
      adjustTargetForSelfMove draggedId =
        -- Only adjust if they share the same immediate parent
        if
          length draggedId == length targetId &&
            take (length draggedId - 1) draggedId == take (length targetId - 1)
              targetId then
          case
            uncons (drop (length targetId - 1) draggedId),
            uncons (drop (length targetId - 1) targetId)
            of
            Just { head: d }, Just { head: t } ->
              let
                adjustedTarget = if d < t then t - 1 else t
                prefix = take (length targetId - 1) targetId
              in
                prefix <> [ adjustedTarget ]
            _, _ -> targetId
        else targetId

      -- Adjust for moves that don't affect the path structure, just indices.
      adjustForSiblingMove :: Path -> Path -> Path
      adjustForSiblingMove oldPath draggedId =
        go oldPath draggedId targetId
        where
        go :: Path -> Path -> Path -> Path
        go oldRest draggedRest targetRest =
          case uncons oldRest, uncons draggedRest, uncons targetRest of
            Just { head: o, tail: os },
            Just { head: d, tail: ds },
            Just { head: t, tail: ts }
              | d == t -> cons o (go os ds ts) -- No actual move
            Just { head: o, tail: os },
            Just { head: d, tail: ds },
            Just { head: t, tail: ts } ->
              -- Index adjustment for sibling moves
              let
                afterRemoval = if o > d then o - 1 else o
                afterInsertion =
                  if afterRemoval > t then afterRemoval + 1 else afterRemoval
              in
                cons afterInsertion (go os ds ts)
            _, _, _ -> oldRest

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
      H.modify_ \state ->
        state
          { tocEntries = entries }
      pure (Just a)

  rootTreeToHTML
    :: forall slots
     . State
    -> String
    -> Array Int
    -> Array Int
    -> Maybe SelectedEntity
    -> Maybe DateTime
    -> RootTree TOCEntry
    -> Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ _ _ _ _ Empty = []
  rootTreeToHTML
    state
    docName
    menuPath
    historyPath
    mSelectedTocEntry
    now
    (RootTree { children }) =
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
                , renderSectionButtonInterface menuPath [] false Section docName
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-list" ] ]
            ( concat $ mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML state menuPath historyPath 1 mSelectedTocEntry [ ix ]
                      now
                      child
                )
                children
            )
        ]
    ]

  treeToHTML
    :: forall slots
     . State
    -> Array Int
    -> Array Int
    -> Int
    -> Maybe SelectedEntity
    -> Array Int
    -> Maybe DateTime
    -> Tree TOCEntry
    -> Array (H.ComponentHTML Action slots m)
  treeToHTML state menuPath historyPath level mSelectedTocEntry path now = case _ of
    Node { title, children } ->
      let
        selectedClasses =
          if selectedNodeHasPath path then
            [ HH.ClassName "active" ]
          else []
        innerDivClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py1, HB.positionRelative ]
        titleClasses =
          [ HB.textTruncate, HB.flexGrow1, HB.fwBold, HB.fs5 ]
      in
        [ HH.div
            ( [ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] <>
                  selectedClasses
              ]
                <>
                  dragProps true
                <>
                  [ HP.style "cursor: pointer;" ]
            )
            [ addDropZone state path
            , HH.div
                [ HP.classes innerDivClasses ]
                [ dragHandle
                , HH.span
                    ( [ HP.classes titleClasses
                      , HP.style "align-self: stretch; flex-basis: 0;"
                      , HP.title title
                      ] <>
                        ( if level > 0 then
                            [ HE.onClick \_ -> JumpToNodeSection path title ]
                          else []
                        )
                    )
                    [ HH.text title ]
                , renderSectionButtonInterface menuPath path true Section title
                ]
            ]
        ]
          <> concat
            ( mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML state menuPath historyPath (level + 1)
                      mSelectedTocEntry
                      (path <> [ ix ])
                      now
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
      selectedNodeHasPath :: Array Int -> Boolean
      selectedNodeHasPath p = case mSelectedTocEntry of
        Just (SelNode selectedPath _) -> selectedPath == p
        _ -> false

    Leaf { title, node: { id, paraID: _, name: _ } } ->
      let
        selectedClasses =
          if Just (SelLeaf id) == mSelectedTocEntry then
            [ HH.ClassName "active" ]
          else []
        containerProps =
          ( [ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] <> selectedClasses
            , HP.title ("Jump to section " <> prettyTitle title)
            ] <> dragProps true
          )
        innerDivBaseClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py1, HB.positionRelative ]
        innerDivProps =
          [ HP.classes innerDivBaseClasses
          , HP.style "cursor: pointer;"
          ] <>
            ( if level > 0 then [ HE.onClick \_ -> JumpToLeafSection id title ]
              else []
            )
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
                , renderParagraphButtonInterface historyPath path state.versions
                    state.showHistorySubmenu
                    now
                    title
                    id
                ]
            ]
        ]
    where
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

  -- If the title is of shape "§{<label>:} Name", change it to "§ Name".
  prettyTitle :: String -> String
  prettyTitle title =
    case regex "§\\{[^}]+:\\}\\s*" noFlags of
      Left _err -> title -- fallback - if regex fails, just return the input
      Right pattern -> replace pattern "§ " title

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

  -- Creates a history button for a paragraph.
  historyButton
    :: forall slots
     . Path
    -> Int
    -> H.ComponentHTML Action slots m
  historyButton path elementID = HH.button
    [ HP.classes
        [ HB.btn
        , HB.btnSecondary
        , HH.ClassName "toc-button"
        , HH.ClassName "toc-add-wrapper"
        , H.ClassName "bi bi-clock-history"
        ]
    , HE.onClick $ const $ ToggleHistoryMenu path elementID
    ]
    []

  renderParagraphButtonInterface
    :: forall slots
     . Path
    -> Path
    -> Array Version
    -> Int
    -> Maybe DateTime
    -> String
    -> Int
    -> H.ComponentHTML Action slots m
  renderParagraphButtonInterface
    historyPath
    path
    versions
    showHistorySubmenu
    now
    title
    elementID =
    HH.div
      [ HP.classes [ HB.positionRelative ] ] $
      [ historyButton path elementID
      , deleteSectionButton path Paragraph (prettyTitle title)
      ]
        <>
          [ if historyPath == path then
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
                versionHistoryMenu
            else
              HH.text ""
          ]
    where
    -- this is a placeholder that only allows to look at the 5 last versions
    versionHistoryMenu =
      map
        (\v -> addVersionButton v)
        versions

    addVersionButton version =
      let
        buttonStyle =
          if showHistorySubmenu == version.identifier then
            [ HH.ClassName "active" ]
          else
            []
      in
        HH.button
          [ HP.classes $
              [ HB.btn
              , HB.btnInfo
              , HB.textStart
              , HB.textDecorationNone
              , HB.w100
              , HB.border0
              , HB.textBody
              , HB.dFlex
              , HB.alignItemsCenter
              , HH.ClassName "toc-item"
              -- , HH.ClassName "active"
              ]
                <>
                  buttonStyle

          , HE.onClick \_ -> ToggleHistorySubmenu version.identifier
          ] $
          [ HH.div
              [ HP.classes [ H.ClassName "bi bi-clock-history", HB.fs5, HB.me1 ] ]
              []
          , HH.div [ HP.classes [ HB.fs6 ] ]
              [ HH.text
                  ( (formatRelativeTime now (DD.docDateToDateTime version.timestamp))
                      <> " "
                      <> (show version.identifier)
                  )
              ]
          ]
            <>
              [ if showHistorySubmenu == version.identifier then
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
                    [ versionHistorySubmenuButton "view Version" OpenVersion version
                    , versionHistorySubmenuButton "Compare to Current Version"
                        CompareVersion
                        version
                    ]
                else
                  HH.text ""
              ]

    versionHistorySubmenuButton t act version =
      HH.button
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
        , HE.onClick \_ -> Both (act elementID version.identifier)
            (ToggleHistoryMenu path elementID)
        ]
        [ HH.div [ HP.classes [ HB.fs6 ] ]
            [ HH.text t ]
        ]

  -- Helper to render add button with dropdown, and optional delete button.
  renderSectionButtonInterface
    :: forall slots
     . Array Int
    -> Array Int
    -> Boolean
    -> EntityKind
    -> String
    -> H.ComponentHTML Action slots m
  renderSectionButtonInterface menuPath currentPath renderDeleteBtn kind title =
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
