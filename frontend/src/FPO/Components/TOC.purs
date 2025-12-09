module FPO.Components.TOC
  ( Action(..)
  , EntityKind(..)
  , EntityToDelete
  , Input
  , Output(..)
  , Path
  , Query(..)
  , SelectedEntity(..)
  , Version
  , findLeafTitleInTree
  , tocview
  ) where

import Data.Array
  ( catMaybes
  , concat
  , cons
  , drop
  , head
  , index
  , last
  , length
  , mapWithIndex
  , null
  , snoc
  , tail
  , take
  , uncons
  , unsnoc
  )
import Data.DateTime (Date, DateTime, adjust)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Show (class Show)
import Data.Time.Duration (Days(..), Minutes)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset, nowDateTime)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getDocumentHeader, getTextElemHistory, postText)
import FPO.Data.Store as Store
import FPO.Data.Time (dateToDatetime, formatAbsoluteTimeDetailed)
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.MetaTree as MM
import FPO.Dto.DocumentDto.TextElement as TE
import FPO.Dto.DocumentDto.TreeDto
  ( Edge(..)
  , Meta(..)
  , RootTree(..)
  , Tree(..)
  , TreeHeader(..)
  , findRootTree
  , getContent
  , getFullTitle
  , getFullTitleForDisplay
  , getHeading
  , getShortTitleForDisplay
  , modifyNodeRootTree
  , unspecifiedMeta
  )
import FPO.Dto.PostTextDto (createPostTextDto)
import FPO.Dto.PostTextDto as PostTextDto
import FPO.Translations.Translator (fromFpoTranslator)
import FPO.Translations.Util (FPOState)
import FPO.Types (TOCEntry, TOCTree, firstTOCEntry)
import FPO.UI.HTML as HTMLP
import FPO.UI.Modals.DeleteModal (deleteConfirmationModal)
import FPO.Util (isPrefixOf, prependIf, singletonIf)
import FPO.Util as Util
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Themes.Bootstrap5 as HB
import Parsing (runParserT)
import Prelude
  ( class Eq
  , Unit
  , bind
  , const
  , discard
  , flip
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
  , (<$>)
  , (<<<)
  , (<>)
  , (==)
  , (>)
  , (>=)
  , (||)
  )
import Simple.I18n.Translator (label, translate)
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent, toEvent)

type Input = DH.DocumentID

type Version = { author :: DH.User, identifier :: Maybe Int, timestamp :: DD.DocDate }

data Output
  -- | Opens the editor for some leaf node, that is, a subsection or paragraph.
  = ChangeToLeaf Int (Maybe String)
  -- | Opens the editor for some non-leaf node. Used to rename sections
  --   (i.e., changing the heading).
  | ChangeToNode Path String (Maybe String)
  -- | Used to tell the editor to update the path of the selected node
  --   during title renaming.
  | UpdateNodePosition Path
  | AddNode Path (Tree TOCEntry)
  | DeleteNode Path
  | ReorderItems { from :: Path, to :: Path }
  | ModifyVersion Int (Maybe Int)
  | CompareTo Int (Maybe Int)
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

instance showSelectedEntity :: Show SelectedEntity where
  show = case _ of
    SelLeaf id -> "Leaf " <> show id
    SelNode path title ->
      "Node " <> show path <> " (" <> title <> ")"

derive instance eqSelectedEntity :: Eq SelectedEntity

data Action
  = Init
  | Both Action Action
  | Receive (Connected Store.Store Input)
  | DoNothing
  | JumpToLeafSection Int Path String
  | JumpToNodeSection Path String String
  | ToggleAddMenu Path
  | ToggleHistoryMenu Path Int
  | ToggleHistoryMenuOff Path
  | ToggleHistorySubmenu (Maybe Int)
  | CreateNewMSection MM.FullTypeName MM.ProperTypeMeta Path
  | OpenVersion Int (Maybe Int)
  | CompareVersion Int (Maybe Int)
  | UpdateVersions (Maybe Date) (Maybe Date) Int
  -- | Section deletion
  | RequestDeleteSection EntityToDelete
  | CancelDeleteSection
  | ConfirmDeleteSection Path
  -- | Drag and Drop
  | StartDrag Path (MM.Disjunction MM.FullTypeName)
  | HighlightDropZone Path (MM.Disjunction MM.FullTypeName) DragEvent
  | ClearDropZones
  | CompleteDrop Path
  --| UpdateSearchBarInputs Int String String
  --| ClearSearchData Int
  | SearchVersions Int
  | ModifyDateInput Boolean Int String
  | UpdateUpToDateVersion

data EntityKind = Section | Paragraph

data Query a
  = ReceiveTOCs TOCTree MM.MetaMap a
  | RequestCurrentTocEntryTitle (Maybe String -> a)
  | RequestCurrentTocEntry (Maybe SelectedEntity -> a)
  | RequestUpToDateVersion (Maybe Version -> a)
  | RequestFullTitle (Maybe String -> a)
  | SelectFirstEntry a
  | UpdateMSelectedTocEntry SelectedEntity (Maybe String) a

type SearchData =
  { elementID :: Int
  , fromDate :: Maybe Date
  , fromStringDate :: String
  , toDate :: Maybe Date
  , toStringDate :: String
  }

type State = FPOState
  ( docID :: DH.DocumentID
  , documentName :: String
  , tocEntries :: RootTree TOCEntry
  , mTitle :: Maybe String
  , metaMap :: MM.MetaMap
  , mSelectedTocEntry :: Maybe SelectedEntity
  , now :: Maybe DateTime
  , showAddMenu :: Array Int
  , showHistoryMenu :: Array Int
  , showHistorySubmenu :: Maybe (Maybe Int)
  , versions :: Array Version
  , dragState ::
      Maybe
        { draggedId :: Path
        , draggedKind :: MM.Disjunction MM.FullTypeName
        , hoveredId :: Path
        }
  -- ^ The current state of a drag-and-drop operation, if any. Includes the path of the dragged item,
  --   its kind (the disjunction of the parent, i.e., the general type of the element), and the path
  --   of the currently hovered item.
  , requestDelete :: Maybe EntityToDelete
  , searchData :: RootTree SearchData
  , timezoneOffset :: Maybe Minutes
  -- temporarily used, might be outdated at times and thus to be updated when needed.
  , upToDateVersion :: Maybe Version
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
      , mTitle: Nothing
      , metaMap: MM.emptyMetaMap
      , mSelectedTocEntry: Nothing
      , now: Nothing
      , showAddMenu: [ -1 ]
      , showHistoryMenu: [ -1 ]
      , showHistorySubmenu: Nothing
      , versions: []
      , docID: input
      , dragState: Nothing
      , requestDelete: Nothing
      , translator: fromFpoTranslator store.translator
      , searchData: Empty
      , timezoneOffset: Nothing
      , upToDateVersion: Nothing
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
  -- Reference to the delete button in the "delete entry" modal.
  modalDeleteRef :: H.RefLabel
  modalDeleteRef = H.RefLabel "modal-delete"

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state =
    HH.div
      [ HP.classes [ HH.ClassName "leftscrollbar" ] ]
      [ HH.div_ $
          renderDeleteModal
            <>
              ( rootTreeToHTML
                  state
                  state.documentName
                  state.showAddMenu
                  state.showHistoryMenu
                  state.mSelectedTocEntry
                  state.now
                  state.searchData
                  state.tocEntries
              )
      ]
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
            DoNothing
            (Just modalDeleteRef)
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
      offset <- liftEffect getTimezoneOffset
      mDoc <- getDocumentHeader s.docID
      let
        docName = case mDoc of
          Left _ -> "" -- TODO error handling
          Right doc -> DH.getName doc
      H.modify_ \st -> do
        st
          { documentName = docName
          , now = Just now
          , timezoneOffset = Just offset
          }

    Both act1 act2 -> do
      handleAction act1
      handleAction act2

    UpdateUpToDateVersion -> do
      s <- H.get
      case s.mSelectedTocEntry of
        Just (SelLeaf elementID) -> do
          temp <- getTextElemHistory s.docID elementID Nothing Nothing (Just 1)
          let
            upToDate =
              case temp of
                Left _ ->
                  { identifier: Nothing
                  , timestamp: DD.genericDocDate
                  , author: DH.U { identifier: "", name: "" }
                  }
                Right e ->
                  case head (TE.getTEHsFromFTEH e) of
                    Nothing ->
                      { identifier: Nothing
                      , timestamp: DD.genericDocDate
                      , author: DH.U { identifier: "", name: "" }
                      }
                    Just val ->
                      { identifier: Just (TE.getHistoryElementID val)
                      , timestamp: TE.getHistoryElementTimestamp val
                      , author: TE.getHistoryElementAuthor val
                      }
          H.modify_ _ { upToDateVersion = Just upToDate }
        _ -> pure unit

    -- the newest version requested in this action is assumed to be the newest version in general
    UpdateVersions mAfter mBefore elementID -> do
      let
        after =
          case mAfter of
            Nothing -> Nothing
            Just val -> Just (DD.DocDate $ dateToDatetime val)
        before =
          case mBefore of
            Nothing -> Nothing
            -- dateToDateTime assumed a time of 0:00, so we shift by 1 day to include the entire day.
            Just val -> Just
              ( DD.DocDate $ fromMaybe (dateToDatetime val) $ adjust (Days 1.0)
                  (dateToDatetime val)
              )
      s <- H.get
      history <- getTextElemHistory s.docID elementID before after Nothing
      -- if this is nothing something went wrong as every element should have a hsitory
      case history of
        Left _ -> pure unit
        Right h -> do
          let
            nV = map
              ( \hEntry ->
                  { identifier: Just (TE.getHistoryElementID hEntry)
                  , timestamp: TE.getHistoryElementTimestamp hEntry
                  , author: TE.getHistoryElementAuthor hEntry
                  }
              )
              (TE.getTEHsFromFTEH h)

          {- we want to detect which version is the up to date one as soon as we can. If we set before to something
          we can't be sure which one is the up to date one (at least until we try something like saving) as such
          we send a second request, this time only asking for the newest version. While this causes 2 requests,
          it helps against potentially unreasonably large requests down the road -}
          upToDate <- case before of
            Just _ -> do
              temp <- getTextElemHistory s.docID elementID Nothing Nothing (Just 1)
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

            Nothing -> case head nV of
              Just entry -> pure entry
              Nothing ->
                pure
                  { identifier: Nothing
                  , timestamp: DD.genericDocDate
                  , author: DH.U { identifier: "", name: "" }
                  }
          let
            -- used to correctly identify which one is the newest version
            -- Nothing signifies that it's the newest version
            -- neither of the Nothing cases should ever occur
            newVersions = case head nV of
              Just entry ->
                if upToDate /= entry then
                  nV
                else
                  case tail nV of
                    Just entries -> cons
                      { identifier: Nothing
                      , timestamp: entry.timestamp
                      , author: entry.author
                      }
                      entries
                    Nothing -> nV
              Nothing -> nV

          H.modify_ _
            { versions = newVersions
            , upToDateVersion = Just upToDate
            }

    SearchVersions elementID -> do
      state <- H.get
      case (findRootTree (\e -> e.elementID == elementID) state.searchData) of
        Nothing -> pure unit
        Just sd -> handleAction (UpdateVersions sd.fromDate sd.toDate elementID)

    -- isFrom determines whehter the from date or the to date is being updated
    ModifyDateInput isFrom elementID input -> do
      state <- H.get
      result <- runParserT input DD.shortDateParser
      let
        newData =
          case result of
            Left _ -> Nothing
            Right date -> Just date
        newSearchTree =
          if isFrom then
            modifyNodeRootTree
              (\v -> v.elementID == elementID)
              ( \v ->
                  { elementID: v.elementID
                  , fromDate: newData
                  , fromStringDate: input
                  , toDate: v.toDate
                  , toStringDate: v.toStringDate
                  }
              )
              state.searchData
          else
            modifyNodeRootTree
              (\v -> v.elementID == elementID)
              ( \v ->
                  { elementID: v.elementID
                  , fromDate: v.fromDate
                  , fromStringDate: v.fromStringDate
                  , toDate: newData
                  , toStringDate: input
                  }
              )
              state.searchData
      H.modify_ _ { searchData = newSearchTree }
      pure unit

    OpenVersion elementID vID -> do
      H.raise (ModifyVersion elementID vID)

    CompareVersion elementID vID -> do
      H.raise (CompareTo elementID vID)

    DoNothing -> do
      pure unit

    JumpToLeafSection id path title -> do
      handleAction (ToggleHistoryMenuOff path)
      mSelectedTocEntry <- H.gets _.mSelectedTocEntry
      when (mSelectedTocEntry /= Just (SelLeaf id)) do
        H.raise (ChangeToLeaf id (Just title))

    JumpToNodeSection path heading title -> do
      H.modify_ _ { mSelectedTocEntry = Just $ SelNode path heading }
      H.raise (ChangeToNode path heading (Just title))

    ToggleAddMenu path -> do
      H.modify_ \state ->
        state
          { showAddMenu =
              if state.showAddMenu == [ -1 ] || state.showAddMenu /= path then path
              else [ -1 ]
          }

    ToggleHistoryMenu path elementID -> do
      state <- H.get
      let
        sData =
          case (findRootTree (\s -> s.elementID == elementID) state.searchData) of
            Just d -> d
            Nothing ->
              { elementID: elementID
              , fromDate: Nothing
              , fromStringDate: ""
              , toDate: Nothing
              , toStringDate: ""
              }
      handleAction (UpdateVersions sData.fromDate sData.toDate elementID)
      now <- liftEffect nowDateTime
      H.modify_ _
        { now = Just now
        , showHistoryMenu =
            if state.showHistoryMenu == [ -1 ] || state.showHistoryMenu /= path then
              path
            else [ -1 ]
        }

    -- does not toggle off if clicked on same toc element
    ToggleHistoryMenuOff path -> do
      H.modify_ \state ->
        state
          { showHistoryMenu =
              if state.showHistoryMenu == path then
                path
              else [ -1 ]
          }

    ToggleHistorySubmenu vID -> do
      H.modify_ \state -> state
        { showHistorySubmenu =
            if state.showHistorySubmenu /= (Just vID) then (Just vID)
            else Nothing
        }

    CreateNewMSection fullTypeName meta path -> do
      H.modify_ _ { showAddMenu = [ -1 ] }

      s <- H.get
      tree <- createNode fullTypeName meta s.metaMap

      case tree of
        Nothing ->
          pure unit -- TODO: Error handling
        Just t ->
          H.raise (AddNode path t)

    RequestDeleteSection entity -> do
      H.modify_ _ { requestDelete = Just entity }

      Util.focusRef modalDeleteRef

    CancelDeleteSection -> do
      H.modify_ _ { requestDelete = Nothing }

    ConfirmDeleteSection path -> do
      H.raise (DeleteNode path)
      H.modify_ _ { requestDelete = Nothing }

    StartDrag id k -> do
      H.modify_ _
        { dragState = Just { draggedId: id, draggedKind: k, hoveredId: id } }

    HighlightDropZone targetId d e -> do
      -- We need to prevent the default behavior to allow dropping.
      H.liftEffect $ preventDefault (toEvent e)

      s <- H.get
      case s.dragState of
        Nothing -> pure unit
        Just { draggedKind } -> do
          -- Only allow highlighting (and, ultimately, dropping)
          -- if the target can accept the dragged item:
          when (MM.isAtLeastAsGeneral draggedKind d) do
            pure unit
            H.modify_ _
              { dragState = map (_ { hoveredId = targetId }) s.dragState }

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
                  H.modify_
                    _ { mSelectedTocEntry = Just (SelNode newPath title) }
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
      -- we could simplify the logic a bunch, but for now we keep it as is, given that the implementation
      -- is robust.
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

  -- Creates a new node (section) and returns its TOC node representation,
  -- not added to the TOC yet.
  -- Completely builds a full subtree with all mandatory children.
  --
  -- TODO: If the meta map is malformed, this might lead to infinite recursion!
  --       We should add a cycle detection mechanism to prevent this. Notice that
  --       the meta map usually just represents a tree and we're coolio, but this
  --       is by no means guaranteed.
  createNode
    :: forall slots
     . MM.FullTypeName
    -> MM.ProperTypeMeta
    -> MM.MetaMap
    -> H.HalogenM State Action slots Output m (Maybe (Tree TOCEntry))
  createNode fullTypeName meta metaMap = do
    let
      header = TreeHeader
        { headerKind: MM.getKindName fullTypeName
        , headerType: MM.getTypeName fullTypeName
        , heading: "// Specify your heading here! \n! New Heading"
        }
    if MM.isLeaf meta then do
      -- Create a new text element for the single child:
      createLeaf fullTypeName
    else do
      let
        mandatoryChildren = MM.getMandatoryChildren meta metaMap
      children <- catMaybes <$> traverse (flip createNodeFromTuple metaMap)
        mandatoryChildren

      pure $ Just $ Node
        { meta: unspecifiedMeta
        , children: map Edge children
        , header: header
        }
    where
    -- A wrapper for `createNode` to work with tuples.
    createNodeFromTuple
      :: Tuple MM.FullTypeName MM.ProperTypeMeta
      -> MM.MetaMap
      -> H.HalogenM State Action slots Output m (Maybe (Tree TOCEntry))
    createNodeFromTuple (Tuple ftm m) = createNode ftm m

  -- Creates a new text element and returns its TOC leaf representation,
  -- not added to the TOC yet.
  createLeaf
    :: forall slots
     . MM.FullTypeName
    -> H.HalogenM State Action slots Output m (Maybe (Tree TOCEntry))
  createLeaf fullTypeName = do
    s <- H.get
    let
      kind = MM.getKindName fullTypeName
      type_ = MM.getTypeName fullTypeName
    gotRes <- postText
      s.docID
      (createPostTextDto { kind: kind, type_: type_ })

    case gotRes of
      Left _ -> pure Nothing
      Right dto -> pure $ Just $ Leaf
        { meta: unspecifiedMeta
        , node:
            { id: PostTextDto.getID dto
            , name: "New Subsection"
            , paraID: 0 -- TODO: Do we still need this?
            }
        }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of
    ReceiveTOCs entries metaMap a -> do
      state <- H.get
      H.modify_ _ { tocEntries = entries, metaMap = metaMap }
      let
        sData = map
          ( \elem ->
              case (findRootTree (\s -> s.elementID == elem.id) state.searchData) of
                Just d -> d
                Nothing ->
                  { elementID: elem.id
                  , fromDate: Nothing
                  , fromStringDate: ""
                  , toDate: Nothing
                  , toStringDate: ""
                  }
          )
          entries
        newMTitle = case state.mSelectedTocEntry of
          Nothing -> Nothing
          Just (SelLeaf leafId) ->
            getFullTitle <$> findLeafMeta leafId entries
          Just (SelNode path _) ->
            getFullTitle <$> findMetaByPath path entries
      H.modify_ _
        { searchData = sData, mTitle = newMTitle }
      case state.mSelectedTocEntry of
        Just (SelLeaf id) ->
          if state.showHistoryMenu /= [ -1 ] then do
            case (findRootTree (\s -> s.elementID == id) sData) of
              Just d -> do
                handleAction $ UpdateVersions d.fromDate d.toDate id
              Nothing -> do
                pure unit
          else do
            pure unit
        _ -> do
          pure unit
      pure (Just a)

    RequestCurrentTocEntryTitle reply -> do
      state <- H.get
      let
        currentTitle = getCurrentTocEntryTitle state.mSelectedTocEntry
          state.tocEntries
      pure (Just (reply currentTitle))

    RequestCurrentTocEntry reply -> do
      state <- H.get
      pure (Just (reply state.mSelectedTocEntry))

    RequestUpToDateVersion reply -> do
      handleAction UpdateUpToDateVersion
      state <- H.get
      pure (Just (reply state.upToDateVersion))

    RequestFullTitle reply -> do
      mTitle <- H.gets _.mTitle
      pure (Just (reply mTitle))

    SelectFirstEntry a -> do
      state <- H.get
      case firstTOCEntry state.tocEntries of
        Nothing ->
          pure (Just a)
        Just entry -> do
          let
            leafId = entry.id
            mTitle = getFullTitleForDisplay <$> findLeafMeta leafId state.tocEntries
          H.modify_ \st ->
            st
              { mSelectedTocEntry = Just (SelLeaf leafId)
              , mTitle = mTitle
              }
          H.raise (ChangeToLeaf leafId mTitle)
          pure (Just a)

    UpdateMSelectedTocEntry entry mTitle a -> do
      H.modify_ _ { mSelectedTocEntry = Just entry, mTitle = mTitle }
      pure (Just a)

  rootTreeToHTML
    :: forall slots
     . State
    -> String
    -> Array Int
    -> Array Int
    -> Maybe SelectedEntity
    -> Maybe DateTime
    -> RootTree SearchData
    -> RootTree TOCEntry
    -> Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ _ _ _ _ _ Empty = []
  rootTreeToHTML
    state
    docName
    menuPath
    historyPath
    mSelectedTocEntry
    now
    searchData
    (RootTree { children, header }) =
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
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-list" ] ]
            ( concat $ mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML header state menuPath historyPath 1 mSelectedTocEntry
                      [ ix ]
                      now
                      searchData
                      child
                )
                children
            )
        ]
    ]

  treeToHTML
    :: forall slots
     . TreeHeader
    -> State
    -> Path
    -> Path
    -> Int
    -> Maybe SelectedEntity
    -> Path
    -> Maybe DateTime
    -> RootTree SearchData
    -> Tree TOCEntry
    -> Array (H.ComponentHTML Action slots m)
  treeToHTML
    parentHeader
    state
    menuPath
    historyPath
    level
    mSelectedTocEntry
    path
    now
    searchData = case _ of
    Node { meta, children, header } ->
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
                  dragProps
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
                      , HP.title $ getFullTitleForDisplay meta
                      ] <>
                        ( if canBeRenamed then
                            [ HE.onClick \_ -> JumpToNodeSection path
                                (getHeading header)
                                (getFullTitle meta)
                            ]
                          else
                            []
                        )
                    )
                    [ HH.text $ getFullTitleForDisplay meta ]
                , addItemInterface
                ]
            ]
        ]
          <> concat
            ( mapWithIndex
                ( \ix (Edge child) ->
                    treeToHTML
                      header
                      state
                      menuPath
                      historyPath
                      (level + 1)
                      mSelectedTocEntry
                      (path <> [ ix ])
                      now
                      searchData
                      child
                )
                children
            )
          <>
            -- Create a new end drop zone at the end of the section.
            -- It is handled like a normal element during drag and drop detection,
            -- i.e., it has its own path. Of course, this only happens if the parent
            -- allows for changes to the children structure (i.e., has `StarOrder` syntax).
            ( case MM.getDisjunction header state.metaMap of
                Nothing ->
                  []
                Just pd ->
                  [ addEndDropZone state (snoc path (length children)) level pd ]
            )
      where
      canBeRenamed = MM.hasEditableHeader header state.metaMap

      selectedNodeHasPath :: Array Int -> Boolean
      selectedNodeHasPath p = case mSelectedTocEntry of
        Just (SelNode selectedPath _) -> selectedPath == p
        _ -> false

      -- Adds the "+" button (for adding new sections or paragraphs) if
      -- the section allows for (more) children and the "-" button, if
      -- allowed for deletion.
      addItemInterface =
        renderSectionButtonInterface
          items
          menuPath
          path
          isDeletable
          Section
          (getFullTitle meta)
        where
        items = MM.findAllowedChildren header state.metaMap

    Leaf { meta, node: { id, paraID: _, name: _ } } ->
      let
        selectedClasses =
          if Just (SelLeaf id) == mSelectedTocEntry then
            [ HH.ClassName "active" ]
          else []
        containerProps =
          ( [ HP.classes $ [ HH.ClassName "toc-item", HB.rounded ] <> selectedClasses
            , HP.title ("Jump to section " <> getShortTitleForDisplay meta)
            ] <> dragProps
          )
        innerDivBaseClasses =
          [ HB.dFlex, HB.alignItemsCenter, HB.py1, HB.positionRelative ]
        innerDivProps =
          [ HP.classes innerDivBaseClasses
          , HP.style "cursor: pointer;"
          ] <>
            -- Stop to be able to click, if alredy selected (prevent spamming post requests)
            ( if level > 0 && mSelectedTocEntry /= Just (SelLeaf id) then
                [ HE.onClick \_ -> JumpToLeafSection id path (getFullTitle meta)
                ]
              else
                []
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
                    [ HH.text $ HTMLP.decodeHtmlEntity (getFullTitle meta) ]
                , renderParagraphButtonInterface
                    historyPath
                    path
                    isDeletable
                    state.versions
                    state.showHistorySubmenu
                    (getFullTitle meta)
                    id
                    searchData
                    state
                ]
            ]
        ]
    where
    -- Determines if the current section can be deleted.
    isDeletable = MM.allowsChildDeletion parentHeader state.metaMap
    -- Determines the disjunction of the parent. If this is `Nothing`, the current
    -- parent has tree syntax `SequenceOrder`, i.e., it allows for only specific children,
    -- otherwise, it has `StarOrder` syntax and allows for any number of children of the
    -- specified disjunction, meaning that we can drag and remove items.
    mParentDisjunction = MM.getDisjunction parentHeader state.metaMap

    dragProps = case mParentDisjunction of
      Nothing -> []
      Just pk ->
        [ HP.draggable true
        , HE.onDragStart $ const $ StartDrag path pk
        , HE.onDragOver $ HighlightDropZone path pk
        , HE.onDrop $ const $ CompleteDrop path
        , HE.onDragEnd $ const $ ClearDropZones
        ]

    -- Show the drag handle only if the parent allows changes to the children structure.
    dragHandle =
      HH.span
        [ HP.classes
            [ HH.ClassName "toc-drag-handle", HB.textMuted, HB.me2 ]
        , HP.style ("margin-left: " <> show level <> "rem;")
        ]
        (singletonIf (isJust mParentDisjunction) $ HH.text "⋮⋮")

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
    :: forall slots
     . State
    -> Path
    -- ^ The path where the drop zone is located.
    -> Int
    -- ^ The level of the section where the drop zone is located.
    -> MM.Disjunction MM.FullTypeName
    -- ^ The general type of acceptable children for this drop zone.
    -> H.ComponentHTML Action slots m
  addEndDropZone state path _ pd =
    HH.div
      ( [ HP.classes
            $ prependIf (activeEndDropzone state path) (H.ClassName "active")
            $ prependIf (previewEndDropzone state path) (H.ClassName "preview")
            $ [ H.ClassName "drop-zone-end" ]
        -- Give it a minimum height so it (i.e., the section)
        -- can receive drag events even when empty.
        , HP.style "min-height: 0.1px;"
        ] <> dragProps
      )
      []
    where
    dragProps =
      [ HE.onDragOver $ HighlightDropZone path pd
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
      [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-dash" ] ] [] ]

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
    -> Boolean
    -> Array Version
    -> Maybe (Maybe Int)
    -> String
    -> Int
    -> RootTree SearchData
    -> State
    -> H.ComponentHTML Action slots m
  renderParagraphButtonInterface
    historyPath
    path
    renderDeleteBtn
    versions
    showHistorySubmenu
    title
    elementID
    searchData
    state =
    HH.div
      [ HP.classes [ HB.positionRelative ] ] $
      [ historyButton path elementID
      ]
        <>
          singletonIf renderDeleteBtn (deleteSectionButton path Paragraph title)
        <>
          [ if historyPath == path then
              HH.div
                [ HP.classes
                    [ HB.positionAbsolute
                    , HB.bgWhite
                    , HB.border
                    , HB.borderSecondary
                    , HB.rounded
                    , HB.shadowSm
                    , HB.py1
                    , HB.px1
                    ]
                , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
                ]
                versionHistoryMenu
            else
              HH.text ""
          ]
    where
    -- this is a placeholder that only allows to look at the 5 last versions

    vHM =
      ( map
          (\v -> addVersionButton v)
          versions
      )

    versionHistoryMenu =
      searchBarSegment
        <>
          [ HH.div
              [ HP.style "overflow: auto; max-height: 19.3rem;" ] $
              vHM
                <>
                  if length vHM >= 200 then
                    [ HH.div
                        [ HP.classes $
                            [ HB.btn
                            , HB.btnInfo
                            , HB.textStart
                            , HB.textDecorationNone
                            , HB.w100
                            , HB.textBody
                            , HB.dFlex
                            , HB.alignItemsCenter
                            , HH.ClassName "toc-item"
                            ]
                        , HP.style
                            "border: none; border-top-style: solid; border-color: grey; border-width: 1px; border-radius: 0;"
                        ] $
                        [ HH.text (translate (label :: _ "toc_full") state.translator)
                        ]
                    ]
                  else
                    []
          ]

    searchBarSegment =
      let
        fromDate =
          case (findRootTree (\e -> e.elementID == elementID) searchData) of
            Nothing -> ""
            Just sd -> sd.fromStringDate
        toDate =
          case (findRootTree (\e -> e.elementID == elementID) searchData) of
            Nothing -> ""
            Just sd -> sd.toStringDate
      in
        [ HH.div
            [ HP.classes [ HB.dFlex, HB.flexColumn ]
            ]
            [ HH.div
                [ HP.classes
                    [ HB.dFlex, HB.flexRow, HB.justifyContentBetween, HB.mb1 ]
                ]
                [ punctuation $
                    (translate (label :: _ "common_from") state.translator) <> ": "
                , HH.input
                    [ HP.type_ HP.InputDate
                    , HP.value fromDate
                    , HE.onValueInput (ModifyDateInput true elementID)
                    ]
                ]
            , HH.div
                [ HP.classes
                    [ HB.dFlex, HB.flexRow, HB.justifyContentBetween, HB.mb1 ]
                ]
                [ punctuation $ (translate (label :: _ "common_to") state.translator)
                    <> ": "
                , HH.input
                    [ HP.type_ HP.InputDate
                    , HP.value toDate
                    , HE.onValueInput (ModifyDateInput false elementID)
                    ]
                ]
            , HH.div
                [ HP.classes
                    [ HB.dFlex, HB.flexRow, HB.justifyContentBetween, HB.mb2 ]
                ]
                [ searchBarButton
                    (SearchVersions elementID)
                    "bi bi-search"
                    (translate (label :: _ "common_search") state.translator)
                ]
            ]
        ]

    searchBarButton action biName smallText = HH.button
      [ HP.classes [ HB.btn, HB.btnOutlineDark, HB.w100, HB.px1, HB.py0, HB.m0 ]
      , HP.style "white-space: nowrap;"
      , HE.onClick \_ -> action
      , HP.enabled true
      ]
      [ HH.small [ HP.style "margin-right: 0.25rem;" ] [ HH.text smallText ]
      , HH.i [ HP.classes [ HB.bi, H.ClassName biName ] ] []
      ]

    punctuation str =
      HH.div
        [ HP.classes
            [ HB.dFlex, HB.alignItemsCenter, HB.textBody, HH.ClassName "mx05" ]
        ]
        [ HH.text str ]

    addVersionButton version =
      let
        buttonStyle =
          if showHistorySubmenu == (Just version.identifier) then
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
              , HB.textBody
              , HB.dFlex
              , HB.alignItemsCenter
              , HH.ClassName "toc-item"
              -- , HH.ClassName "active"
              ]
                <>
                  buttonStyle
          , HP.style
              "border: none; border-top-style: solid; border-color: grey; border-width: 1px; border-radius: 0;"

          , HE.onClick \_ -> ToggleHistorySubmenu version.identifier
          ] $
          [ HH.div
              [ HP.classes [ H.ClassName "bi bi-clock-history", HB.fs5, HB.me1 ] ]
              []
          , HH.div [ HP.classes [ HB.fs6 ] ]
              [ HH.text
                  ( formatAbsoluteTimeDetailed state.timezoneOffset
                      (DD.docDateToDateTime version.timestamp)
                      <> " "
                      <> (translate (label :: _ "common_by") state.translator)
                      <> " "
                      <> (DH.getUserName version.author)
                  )
              ]
          ]
            <>
              [ if showHistorySubmenu == (Just version.identifier) then
                  HH.div
                    [ HP.classes
                        [ HB.positionAbsolute
                        , HB.bgWhite
                        , HB.border
                        , HB.borderSecondary
                        , HB.rounded
                        , HB.shadowSm
                        , HB.py1
                        ]
                    , HP.style "top: 100%; right: 0; z-index: 1000; min-width: 160px;"
                    ]
                    [ versionHistorySubmenuButton
                        (translate (label :: _ "editor_viewVersion") state.translator)
                        OpenVersion
                        version
                    , versionHistorySubmenuButton
                        ( translate (label :: _ "editor_compareVersion")
                            state.translator
                        )
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
     . Array (Tuple MM.FullTypeName MM.ProperTypeMeta)
    -> Array Int
    -> Array Int
    -> Boolean
    -> EntityKind
    -> String
    -> H.ComponentHTML Action slots m
  renderSectionButtonInterface
    items
    menuPath
    currentPath
    renderDeleteBtn
    kind
    title =
    HH.div
      [ HP.classes [ HB.positionRelative ] ] $
      ( singletonIf (not $ null items) $
          HH.button
            [ HP.classes
                [ HB.btn
                , HB.btnSuccess
                , HH.ClassName "toc-button"
                , HH.ClassName "toc-add-wrapper"
                ]
            , HE.onClick \_ -> ToggleAddMenu currentPath
            ]
            [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-plus" ] ] [] ]
      )
        <>
          ( singletonIf renderDeleteBtn $ deleteSectionButton currentPath kind title
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
                buttons
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

    -- Creates buttons for each allowed item type.
    buttons = map createSectionButton items
      where
      createSectionButton (Tuple tyName meta) =
        addSectionButton (MM.getDisplayNameAsString meta)
          (CreateNewMSection tyName meta)

-- Helper function to extract the title from the current TOC entry
getCurrentTocEntryTitle :: Maybe SelectedEntity -> RootTree TOCEntry -> Maybe String
getCurrentTocEntryTitle mSelectedEntry tocEntries = case mSelectedEntry of
  Nothing -> Nothing
  Just (SelLeaf leafId) -> findLeafTitle leafId tocEntries
  Just (SelNode _ title) -> Just title

-- Helper to find a leaf title by ID
findLeafTitle :: Int -> RootTree TOCEntry -> Maybe String
findLeafTitle _ Empty = Nothing
findLeafTitle targetId (RootTree { children }) =
  findLeafTitleInChildren targetId children

findLeafTitleInChildren :: Int -> Array (Edge TOCEntry) -> Maybe String
findLeafTitleInChildren targetId children =
  case uncons children of
    Nothing -> Nothing
    Just { head: Edge tree, tail: rest } ->
      case findLeafTitleInTree targetId tree of
        Just title -> Just title
        Nothing -> findLeafTitleInChildren targetId rest

findLeafTitleInTree :: Int -> Tree TOCEntry -> Maybe String
findLeafTitleInTree targetId = case _ of
  Leaf { meta: Meta meta, node: { id } } ->
    if id == targetId then getContent meta.title else Nothing
  Node { children } ->
    findLeafTitleInChildren targetId children

-- find meta helper function

findLeafMeta :: Int -> RootTree TOCEntry -> Maybe Meta
findLeafMeta _ Empty = Nothing
findLeafMeta targetId (RootTree { children }) =
  goChildren children
  where
  goChildren :: Array (Edge TOCEntry) -> Maybe Meta
  goChildren cs = case uncons cs of
    Nothing -> Nothing
    Just { head: Edge t, tail: rest } ->
      case t of
        Leaf { meta, node: { id } } ->
          if id == targetId then Just meta else goChildren rest
        Node { children: nChildren } ->
          case goChildren nChildren of
            Just m -> Just m
            Nothing -> goChildren rest

findMetaByPath :: Path -> RootTree TOCEntry -> Maybe Meta
findMetaByPath _ Empty = Nothing
findMetaByPath path (RootTree { children }) = go path children
  where
  go :: Array Int -> Array (Edge TOCEntry) -> Maybe Meta
  go p cs = case uncons p of
    Nothing -> Nothing
    Just { head: i, tail: rest } ->
      case index cs i of
        Nothing -> Nothing
        Just (Edge t) ->
          case rest, t of
            [], Leaf { meta } -> Just meta
            [], Node { meta } -> Just meta
            _, Node { children: nChildren } -> go rest nChildren
            _, Leaf _ -> Nothing
