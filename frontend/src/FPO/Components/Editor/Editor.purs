module FPO.Components.Editor
  ( Action(..)
  , Input
  , LiveMarker
  , Output(..)
  , Query(..)
  , State
  , addAnnotation
  , editor
  , findAllIndicesOf
  ) where

import Prelude

import Ace (ace, editNode) as Ace
import Ace.Anchor as Anchor
import Ace.Document as Document
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Range as Range
import Ace.Types as Types
import Ace.UndoManager as UndoMgr
import Data.Array (catMaybes, filter, intercalate, uncons, (:))
import Data.Either (Either(..))
import Data.Foldable (find, for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class as EC
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FPO.Components.Editor.Keybindings
  ( keyBinding
  , makeBold
  , makeItalic
  , underscore
  )
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.ContentDto (Content)
import FPO.Dto.ContentDto as ContentDto
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.UserDto (getUserName)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( AnnotatedMarker
  , TOCEntry
  , emptyTOCEntry
  , markerToAnnotation
  , sortMarkers
  )
import FPO.Util (prependIf)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, ref, style, title) as HP
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Subscription as HS
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType(..), preventDefault)
import Web.Event.EventTarget
  ( EventListener
  , addEventListener
  , eventListener
  , removeEventListener
  )
import Web.HTML (window)
import Web.HTML.HTMLElement (offsetWidth, toElement)
import Web.HTML.Window as Win
import Web.ResizeObserver as RO
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

type State = FPOState
  ( docID :: DocumentID
  , mEditor :: Maybe Types.Editor
  , mTocEntry :: Maybe TOCEntry
  , title :: String
  , mContent :: Maybe Content
  , liveMarkers :: Array LiveMarker
  , fontSize :: Int
  , mListener :: Maybe (HS.Listener Action)
  , resizeObserver :: Maybe RO.ResizeObserver
  , resizeSubscription :: Maybe SubscriptionId
  , showButtonText :: Boolean
  -- for saving when closing window
  , mDirtyRef :: Maybe (Ref Boolean)
  , mBeforeUnloadL :: Maybe EventListener
  -- saved icon
  , showSavedIcon :: Boolean
  , mSavedIconF :: Maybe H.ForkId
  -- for periodically saving the content
  , mPendingDebounceF :: Maybe H.ForkId -- 2s-Timer
  , mPendingMaxWaitF :: Maybe H.ForkId -- 20s-Max-Timer
  )

-- For tracking the comment markers live
-- Only store the values in save
type LiveMarker =
  { annotedMarkerID :: Int
  , startAnchor :: Types.Anchor
  , endAnchor :: Types.Anchor
  , ref :: Ref Int
  }

type Input = DocumentID

data Output
  = ClickedQuery (Array String)
  | DeletedComment TOCEntry (Array Int)
  -- SavedSection toBePosted title TOCEntry
  | SavedSection Boolean String TOCEntry
  | SelectedCommentSection Int Int
  | SendingTOC TOCEntry
  | ShowAllCommentsOutput

data Action
  = Init
  | Comment
  | DeleteComment
  | SelectComment
  | Bold
  | Italic
  | Underline
  | FontSizeUp
  | FontSizeDown
  | Undo
  | Redo
  | Save
  -- Subsection of Save
  | Upload TOCEntry String Content
  -- Subsection of Upload
  | LostParentID TOCEntry String Content
  | SavedIcon
  -- new change in editor -> reset timer
  | AutoSaveTimer
  -- called by AutoSaveTimer subscription
  | AutoSave
  | RenderHTML
  | ShowAllComments
  | Receive (Connected FPOTranslator Input)
  | HandleResize Number
  | Finalize

-- We use a query to get the content of the editor
data Query a
  -- = RequestContent (Array String -> a)
  = QueryEditor a
  -- save the current content and send it to splitview
  | SaveSection a
  -- receive the selected TOC and put its content into the editor
  | ChangeSection String TOCEntry (Maybe Int) a
  | SendCommentSections a

editor
  :: forall m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Input Output m
editor = connect selectTranslator $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      , finalize = Just Finalize
      }
  }
  where
  initialState :: Connected FPOTranslator Input -> State
  initialState { context, input } =
    { docID: input
    , translator: fromFpoTranslator context
    , mEditor: Nothing
    , mTocEntry: Nothing
    , title: ""
    , mContent: Nothing
    , liveMarkers: []
    , fontSize: 12
    , mListener: Nothing
    , resizeObserver: Nothing
    , resizeSubscription: Nothing
    , showButtonText: true
    , mDirtyRef: Nothing
    , mBeforeUnloadL: Nothing
    , showSavedIcon: false
    , mSavedIconF: Nothing
    , mPendingDebounceF: Nothing
    , mPendingMaxWaitF: Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HE.onClick $ const SelectComment
      , HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ]
      ]
      [ HH.div -- Second toolbar

          [ HP.classes [ HB.dFlex, HB.justifyContentBetween ] ]
          [ HH.div
              [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
              [ makeEditorToolbarButton
                  (translate (label :: _ "editor_textBold") state.translator)
                  Bold
                  "bi-type-bold"
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_textItalic") state.translator)
                  Italic
                  "bi-type-italic"
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_textUnderline") state.translator)
                  Underline
                  "bi-type-underline"

              , buttonDivisor
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_fontSizeUp") state.translator)
                  FontSizeUp
                  "bi-plus-square"
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_fontSizeDown") state.translator)
                  FontSizeDown
                  "bi-dash-square"

              , buttonDivisor
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_undo") state.translator)
                  Undo
                  "bi-arrow-counterclockwise"
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_redo") state.translator)
                  Redo
                  "bi-arrow-clockwise"

              , buttonDivisor
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_comment") state.translator)
                  Comment
                  "bi-chat-square-text"
              , makeEditorToolbarButton
                  (translate (label :: _ "editor_deleteComment") state.translator)
                  DeleteComment
                  "bi-chat-square-text-fill"

              ]
          , HH.div
              [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ]
              , HP.style "min-width: 0;"
              ]
              [ makeEditorToolbarButtonWithText
                  state.showButtonText
                  Save
                  "bi-floppy"
                  (translate (label :: _ "editor_save") state.translator)
              , makeEditorToolbarButtonWithText
                  state.showButtonText
                  RenderHTML
                  "bi-file-richtext"
                  (translate (label :: _ "editor_preview") state.translator)
              , makeEditorToolbarButtonWithText
                  state.showButtonText
                  ShowAllComments
                  "bi-chat-square"
                  (translate (label :: _ "editor_allComments") state.translator)
              ]
          ]
      , HH.div -- Editor container

          [ HP.ref (H.RefLabel "container")
          , HP.classes [ HB.flexGrow1 ]
          , HP.style "min-height: 0"
          ]
          []
      -- Saved Icon
      , if state.showSavedIcon then
          HH.div
            [ HP.classes [ HH.ClassName "save-toast" ]
            , HP.style "position: absolute; right: .5rem; bottom: .5rem; z-index: 10;"
            ]
            [ HH.text $ (translate (label :: _ "editor_save") state.translator) <>
                " 💾"
            ]
        else
          HH.text ""
      ]

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Init -> do
      -- create subscription for later use
      { emitter, listener } <- H.liftEffect HS.create
      H.modify_ _ { mListener = Just listener }
      -- Subscribe to resize events and store subscription for cleanup
      subscription <- H.subscribe emitter
      H.modify_ _ { resizeSubscription = Just subscription }
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el -> do
        editor_ <- H.liftEffect $ Ace.editNode el Ace.ace

        H.modify_ _ { mEditor = Just editor_ }
        fontSize <- H.gets _.fontSize

        H.liftEffect $ do
          Editor.setFontSize (show fontSize <> "px") editor_
          eventListen <- eventListener (keyBinding editor_)
          container <- Editor.getContainer editor_
          addEventListener keydown eventListen true
            (toEventTarget $ toElement container)
          session <- Editor.getSession editor_

          -- Set the editor's theme and mode
          Editor.setTheme "ace/theme/github" editor_
          Session.setMode "ace/mode/custom_mode" session
          Editor.setEnableLiveAutocompletion true editor_

      -- New Ref for keeping track, if the content in editor has changed
      -- since last save
      dref <- H.liftEffect $ Ref.new false
      H.modify_ _ { mDirtyRef = Just dref }

      -- add and start Editor change listener
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ addChangeListenerWithRef ed dref listener

      win <- H.liftEffect window
      let
        winTarget = Win.toEventTarget win
        -- creating EventTypes
        beforeunload = EventType "beforeunload"

      -- create eventListener for preventing the tab from closing
      -- when content has not been saved (Not changing through Navbar)
      buL <- H.liftEffect $ eventListener \ev -> do
        readRef <- traverse Ref.read (Just dref) -- dref ist da; zur Not: Maybe-handling
        case readRef of
          -- Prevent the tab from closing in a certain way
          Just true -> do
            preventDefault ev
            HS.notify listener Save
          _ -> pure unit
      H.modify_ _ { mBeforeUnloadL = Just buL }
      H.liftEffect $ addEventListener beforeunload buL false winTarget

      -- Setup ResizeObserver for the container element
      H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \element -> do

        let
          callback _ _ = do
            -- Get the current width directly from the element
            width <- offsetWidth element
            HS.notify listener (HandleResize width)

        observer <- H.liftEffect $ RO.resizeObserver callback
        H.liftEffect $ RO.observe (toElement element) {} observer
        H.modify_ _ { resizeObserver = Just observer }

    Bold -> do
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ do
          makeBold ed
          Editor.focus ed

    Italic -> do
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ do
          makeItalic ed
          Editor.focus ed

    Underline -> do
      H.gets _.mEditor >>= traverse_ \ed ->
        H.liftEffect $ do
          underscore ed
          Editor.focus ed

    FontSizeUp -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        let newSize = state.fontSize + 2
        H.modify_ \st -> st { fontSize = newSize }
        -- Set the new font size in the editor
        H.liftEffect $ do
          Editor.setFontSize (show newSize <> "px") ed
          Editor.focus ed

    FontSizeDown -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        let newSize = state.fontSize - 2
        H.modify_ \st -> st { fontSize = newSize }
        -- Set the new font size in the editor
        H.liftEffect $ do
          Editor.setFontSize (show newSize <> "px") ed
          Editor.focus ed

    Undo -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        H.liftEffect $ do
          Editor.undo ed
          Editor.focus ed

    Redo -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        H.liftEffect $ do
          Editor.redo ed
          Editor.focus ed

    RenderHTML -> do
      _ <- handleQuery (QueryEditor unit)
      pure unit

    ShowAllComments -> do
      H.raise ShowAllCommentsOutput

    -- Save section

    Save -> do
      state <- H.get
      -- Save the current content of the editor and send it to the server
      case state.mContent of
        Nothing -> pure unit
        Just content -> do
          allLines <- H.gets _.mEditor >>= traverse \ed -> do
            H.liftEffect $ Editor.getSession ed
              >>= Session.getDocument
              >>= Document.getAllLines

          -- Save the current content of the editor
          let
            contentLines =
              fromMaybe { title: "", contentText: "" } do
                { head, tail } <- uncons =<< allLines
                pure { title: head, contentText: intercalate "\n" tail }
            title = contentLines.title
            contentText = contentLines.contentText

            -- place it in contentDto
            newContent = ContentDto.setContentText contentText content

            -- extract the current TOC entry
            entry = case state.mTocEntry of
              Nothing -> emptyTOCEntry
              Just e -> e

          -- Since the ids and postions in liveMarkers are changing constantly,
          -- extract them now and store them
          updatedMarkers <- H.liftEffect do
            for entry.markers \m -> do
              case find (\lm -> lm.annotedMarkerID == m.id) state.liveMarkers of
                -- TODO Should we add other markers in liveMarkers such as errors?
                Nothing -> pure m
                Just lm -> do
                  start <- Anchor.getPosition lm.startAnchor
                  end <- Anchor.getPosition lm.endAnchor
                  pure m
                    { startRow = Types.getRow start
                    , startCol = Types.getColumn start
                    , endRow = Types.getRow end
                    , endCol = Types.getColumn end
                    }
          -- update the markers in entry
          let newEntry = entry { markers = updatedMarkers }

          -- Try to upload
          handleAction $ Upload newEntry title newContent

    Upload newEntry title newContent -> do
      state <- H.get
      let jsonContent = ContentDto.encodeContent newContent

      -- send the new content as POST to the server
      response <- Request.postJson (ContentDto.extractNewParent newContent)
        ("/docs/" <> show state.docID <> "/text/" <> show newEntry.id <> "/rev")
        jsonContent

      -- handle errors in pos and decodeJson
      case response of
        Left _ -> handleAction $ LostParentID newEntry title newContent
        -- extract and insert new parentID into newContent
        Right updatedContent -> do
          -- Update the tree to backend, when title was really changed
          let oldTitle = state.title
          H.raise (SavedSection (oldTitle /= title) title newEntry)

          H.modify_ \st -> st
            { mTocEntry = Just newEntry
            , title = title
            , mContent = Just updatedContent
            }

          -- Show saved icon
          handleAction SavedIcon

          -- mDirtyRef := false
          for_ state.mDirtyRef \r -> H.liftEffect $ Ref.write false r
          pure unit

    LostParentID newEntry title newContent -> do
      docID <- H.gets _.docID
      loadedContent <- H.liftAff $
        Request.getFromJSONEndpoint
          ContentDto.decodeContent
          ("/docs/" <> show docID <> "/text/" <> show newEntry.id <> "/rev/latest")
      case loadedContent of
        -- TODO: Error handling
        Nothing -> pure unit
        Just res ->
          handleAction $
            Upload
              newEntry
              title
              (ContentDto.setContentText (ContentDto.getContentText newContent) res)

    SavedIcon -> do
      state <- H.get
      -- restart saved icon
      for_ state.mSavedIconF H.kill
      H.modify_ _ { showSavedIcon = true }
      -- start new fiber
      iFib <- H.fork do
        H.liftAff $ delay (Milliseconds 1200.0)
        H.modify_ _ { showSavedIcon = false, mSavedIconF = Nothing }
      H.modify_ _ { mSavedIconF = Just iFib }

    AutoSaveTimer -> do
      state <- H.get
      -- restart 2 sec timer after every new input
      -- first kill the maybe running fiber (kinda like a thread)
      for_ state.mPendingDebounceF H.kill

      -- start a new fiber
      dFib <- H.fork do
        H.liftAff $ delay (Milliseconds 2000.0)
        isDirty <- EC.liftEffect $ Ref.read =<< case state.mDirtyRef of
          Just r -> pure r
          Nothing -> EC.liftEffect $ Ref.new false
        when isDirty $ handleAction AutoSave
      H.modify_ _ { mPendingDebounceF = Just dFib }

      -- This is a seperate 20 sec timer, which forces to save, in case of a long edit
      -- does not reset with new input
      case state.mPendingMaxWaitF of
        -- timer already running
        Just _ -> pure unit
        -- no timer there
        Nothing -> do
          mFib <- H.fork do
            H.liftAff $ delay (Milliseconds 20000.0)
            isDirty <- EC.liftEffect $ Ref.read =<< case state.mDirtyRef of
              Just r -> pure r
              Nothing -> EC.liftEffect $ Ref.new false
            when isDirty $ handleAction AutoSave
          H.modify_ _ { mPendingMaxWaitF = Just mFib }

    AutoSave -> do
      -- only save, if dirty
      isDirty <- maybe (pure false) (H.liftEffect <<< Ref.read) =<< H.gets _.mDirtyRef
      when isDirty do
        handleAction Save
        -- after Save: dirty false + stop timer
        mRef <- H.gets _.mDirtyRef
        for_ mRef \r -> H.liftEffect $ Ref.write false r
        st <- H.get
        for_ st.mPendingDebounceF H.kill
        for_ st.mPendingMaxWaitF H.kill
        H.modify_ _ { mPendingDebounceF = Nothing, mPendingMaxWaitF = Nothing }

    Comment -> do
      userWithError <- getUser
      case userWithError of
        Left _ -> pure unit -- TODO error handling 
        Right user -> do
          H.gets _.mEditor >>= traverse_ \ed -> do
            state <- H.get
            session <- H.liftEffect $ Editor.getSession ed
            range <- H.liftEffect $ Editor.getSelectionRange ed
            start <- H.liftEffect $ Range.getStart range
            end <- H.liftEffect $ Range.getEnd range

            let
              sRow = Types.getRow start
              sCol = Types.getColumn start
              eRow = Types.getRow end
              eCol = Types.getColumn end
              userName = getUserName user
              newMarkerID = case state.mTocEntry of
                Nothing -> 0
                Just tocEntry -> tocEntry.newMarkerNextID
              newCommentSection =
                { markerID: newMarkerID
                , comments: []
                , resolved: false
                }
              newMarker =
                { id: newMarkerID
                , type: "info"
                , startRow: sRow
                , startCol: sCol
                , endRow: eRow
                , endCol: eCol
                , markerText: userName
                , mCommentSection: Just newCommentSection
                }

            mLiveMarker <- H.liftEffect $ addAnchor newMarker session

            let
              newLiveMarkers = case mLiveMarker of
                Nothing -> state.liveMarkers
                Just lm -> lm : state.liveMarkers

            case state.mTocEntry of
              Just entry -> do
                let
                  newEntry =
                    { id: entry.id
                    , name: entry.name
                    , paraID: entry.paraID
                    , newMarkerNextID: entry.newMarkerNextID + 1
                    , markers: sortMarkers (newMarker : entry.markers)
                    }
                H.modify_ \st ->
                  st
                    { mTocEntry = Just newEntry
                    , liveMarkers = newLiveMarkers
                    }
                H.raise (SavedSection false state.title newEntry)
                H.raise
                  (SelectedCommentSection entry.id newMarker.id)
              Nothing -> pure unit

    DeleteComment -> do
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        case state.mTocEntry of
          Nothing -> pure unit
          Just tocEntry -> do
            session <- H.liftEffect $ Editor.getSession ed
            cursor <- H.liftEffect $ Editor.getCursorPosition ed

            -- remove the marker at the cursor position and return the remaining markers
            foundLM <- H.liftEffect $ cursorInRange state.liveMarkers cursor
            case foundLM of
              Nothing -> pure unit
              Just lm -> do
                let
                  foundID = lm.annotedMarkerID
                  newMarkers = filter (\m -> not (m.id == foundID)) tocEntry.markers
                  newLiveMarkers =
                    filter (\l -> not (l.annotedMarkerID == foundID))
                      state.liveMarkers
                  newTOCEntry = tocEntry { markers = newMarkers }
                H.liftEffect $ removeLiveMarker lm session
                H.modify_ \st ->
                  st { mTocEntry = Just newTOCEntry, liveMarkers = newLiveMarkers }
                H.raise (DeletedComment newTOCEntry [ foundID ])

    SelectComment -> do
      H.gets _.mEditor >>= traverse_ \ed -> do
        state <- H.get
        cursor <- H.liftEffect $ Editor.getCursorPosition ed
        foundLM <- H.liftEffect $ cursorInRange state.liveMarkers cursor
        let
          foundID = case foundLM of
            Nothing -> -1
            Just lm -> lm.annotedMarkerID

          -- extract markers from the current TOC entry
          tocEntry = case state.mTocEntry of
            Nothing -> emptyTOCEntry
            Just e -> e
          markers = tocEntry.markers

        when (foundID >= 0)
          case (find (\m -> m.id == foundID) markers) of
            Nothing -> pure unit
            Just foundMarker -> H.raise
              (SelectedCommentSection tocEntry.id foundMarker.id)

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

    HandleResize width -> do
      -- Decides whether to show button text based on the width.
      -- Because german labels are longer, we need to adjust the cutoff
      -- threshold dynamically. Pretty sure this is not the best solution,
      -- but it works.

      lang <- liftEffect $ Store.loadLanguage
      let cutoff = if lang == Just "de-DE" then 573.0 else 520.0

      H.modify_ _ { showButtonText = width >= cutoff }

    Finalize -> do
      state <- H.get
      win <- H.liftEffect window
      let
        tgt = Win.toEventTarget win
        beforeunload = EventType "beforeunload"
      -- Cleanup observer and subscription
      H.liftEffect $ case state.resizeObserver of
        Just obs -> RO.disconnect obs
        Nothing -> pure unit
      case state.resizeSubscription of
        Just subscription -> H.unsubscribe subscription
        Nothing -> pure unit
      case state.mBeforeUnloadL of
        Just l -> H.liftEffect $ removeEventListener beforeunload l false tgt
        _ -> pure unit
      for_ state.mPendingDebounceF H.kill
      for_ state.mPendingMaxWaitF H.kill

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ChangeSection title entry rev a -> do
      H.modify_ \state -> state { mTocEntry = Just entry, title = title }
      state <- H.get

      -- Put the content of the section into the editor and update markers
      H.gets _.mEditor >>= traverse_ \ed -> do

        let
          version = case rev of
            Nothing -> "latest"
            Just v -> show v
        -- Get the content from server here
        -- We need Aff for that and thus cannot go inside Eff
        -- TODO: After creating a new Leaf, we get Nothing in loadedContent
        -- See, why and fix it
        loadedContent <- H.liftAff $
          Request.getFromJSONEndpoint
            ContentDto.decodeContent
            ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
                <> "/rev/"
                <> version
            )
        let
          content = case loadedContent of
            Nothing -> ContentDto.failureContent
            Just res -> res
        H.modify_ \st -> st { mContent = Just content }

        newLiveMarkers <- H.liftEffect do
          session <- Editor.getSession ed
          document <- Session.getDocument session

          -- Set the content of the editor
          Document.setValue (title <> "\n" <> ContentDto.getContentText content)
            document

          -- Reset Undo history
          undoMgr <- Session.getUndoManager session
          UndoMgr.reset undoMgr

          -- Remove existing markers
          for_ state.liveMarkers \lm -> do
            removeLiveMarker lm session
          -- existingMarkers <- Session.getMarkers session
          -- for_ existingMarkers \marker -> do
          --   id <- Marker.getId marker
          --   Session.removeMarker id session

          -- Clear annotations
          Session.clearAnnotations session

          -- Add annotations from marker
          tmp <- for (entry.markers) \marker -> do
            addAnchor marker session

          pure (catMaybes tmp)

        -- Update state with new marker IDs
        H.modify_ \st ->
          st { liveMarkers = newLiveMarkers }
      -- reset Ref, because loading new content is considered 
      -- changing the existing content, which would set the flag
      for_ state.mDirtyRef \r -> H.liftEffect $ Ref.write false r
      pure (Just a)

    SaveSection a -> do
      handleAction Save
      pure (Just a)

    -- Because Session does not provide a way to get all lines directly,
    -- we need to take another indirect route to get the lines.
    -- Notice that this extra step is not needed for all js calls.
    -- For example, `Session.getLine` can be called directly.
    QueryEditor a -> do
      allLines <- H.gets _.mEditor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines
      H.raise (ClickedQuery $ fromMaybe [] allLines)
      pure (Just a)

    SendCommentSections a -> do
      state <- H.get
      -- Send the current comment sections to the splitview
      case state.mTocEntry of
        Nothing -> pure unit
        Just tocEntry -> do
          H.raise (SendingTOC tocEntry)
      pure (Just a)

-- | Change listener for the editor.
--
--   This function should implement stuff like parsing and syntax analysis,
--   linting, code completion, etc.
--   For now, it puts "  " in front of "#", if it is placed at the
--   beginning of a line
--  Update: it also detects, when content is changed and set dref flag
addChangeListenerWithRef
  :: Types.Editor
  -> Ref Boolean
  -> HS.Listener Action
  -> Effect Unit
addChangeListenerWithRef editor_ dref listener = do
  session <- Editor.getSession editor_
  -- in order to prevent an ifinite loop with this listener
  guardRef <- Ref.new false
  Session.onChange session \(Types.DocumentEvent { action, start, end: _, lines }) ->
    do
      -- set dirty flag
      Ref.write true dref
      HS.notify listener AutoSaveTimer

      -- '#' → '  #' at beginning of a line with Reentrancy-Guard
      let isInsert = (unsafeCoerce action :: String) == "insert"
      when isInsert do
        let sCol = Types.getColumn start
        when (sCol == 0 && lines == [ "#" ]) do
          busy <- Ref.read guardRef
          when (not busy) do
            Ref.write true guardRef
            let sRow = Types.getRow start
            range <- Range.create sRow sCol sRow (sCol + 1)
            Session.replace range "  #" session
            Ref.write false guardRef

-- | Helper function to find all indices of a substring in a string
--   in reverse order.
--
--   Naive and slow pattern matching, but it works for small strings and is
--   good enough for our marker placement experiments.
findAllIndicesOf :: String -> String -> Array Int
findAllIndicesOf needle haystack = go 0 []
  where
  go startIndex acc =
    case String.indexOf (String.Pattern needle) (String.drop startIndex haystack) of
      Just relativeIndex ->
        let
          absoluteIndex = startIndex + relativeIndex
        in
          go (absoluteIndex + 1) (absoluteIndex : acc)
      Nothing -> acc

-- | Adds an annotation to the editor session.
addAnnotation
  :: Types.Annotation
  -> Types.EditSession
  -> Effect Unit
addAnnotation annotation session = do
  anns <- Session.getAnnotations session
  Session.setAnnotations (annotation : anns) session

addAnchor :: AnnotatedMarker -> Types.EditSession -> Effect (Maybe LiveMarker)
addAnchor marker session =
  if (marker.startRow == marker.endRow && marker.startCol == marker.endCol) then
    pure Nothing -- No valid range, so no marker
  else do
    document <- Session.getDocument session
    startAnchor <- Document.createAnchor marker.startRow marker.startCol document
    endAnchor <- Document.createAnchor marker.endRow marker.endCol document
    Anchor.setInsertRight true endAnchor

    range <- createMarkerRange marker
    id <- Session.addMarker range "my-marker" "string" false session
    markerRef <- Ref.new id

    let
      rerenderMarker _ = do
        Ref.read markerRef >>= flip Session.removeMarker session
        Types.Position { row: startRow, column: startColumn } <- Anchor.getPosition
          startAnchor
        Types.Position { row: endRow, column: endColumn } <- Anchor.getPosition
          endAnchor
        markRange <- Range.create
          startRow
          startColumn
          endRow
          endColumn
        newId <- Session.addMarker
          markRange
          "my-marker"
          "string"
          false
          session

        Ref.write newId markerRef
        pure unit

    Anchor.onChange startAnchor rerenderMarker
    Anchor.onChange endAnchor rerenderMarker
    addAnnotation (markerToAnnotation marker) session
    pure
      ( Just
          { annotedMarkerID: marker.id
          , startAnchor: startAnchor
          , endAnchor: endAnchor
          , ref: markerRef
          }
      )

removeLiveMarker :: LiveMarker -> Types.EditSession -> Effect Unit
removeLiveMarker lm session = do
  -- Marker entfernen
  markerId <- Ref.read lm.ref
  Session.removeMarker markerId session

  -- Anchors vom Dokument lösen
  Anchor.detach lm.startAnchor
  Anchor.detach lm.endAnchor

createMarkerRange :: AnnotatedMarker -> Effect Types.Range
createMarkerRange marker = do
  range <- Range.create marker.startRow marker.startCol marker.endRow marker.endCol
  pure range

-- Gets all markers from this session. Then check, if the Position is in
-- range one of the markers. Because the markers are sorted by start Position
-- we can use the
--findLocalMarkerID

cursorInRange :: Array LiveMarker -> Types.Position -> Effect (Maybe LiveMarker)
cursorInRange [] _ = pure Nothing
cursorInRange lms cursor =
  case uncons lms of
    Just { head: l, tail: ls } -> do
      start <- Anchor.getPosition l.startAnchor
      end <- Anchor.getPosition l.endAnchor
      range <- Range.create
        (Types.getRow start)
        (Types.getColumn start)
        (Types.getRow end)
        (Types.getColumn end)
      found <- Range.contains
        (Types.getRow cursor)
        (Types.getColumn cursor)
        range
      if found then
        pure (Just l)
      else
        cursorInRange ls cursor
    Nothing -> pure Nothing

makeEditorToolbarButton
  :: forall m. String -> Action -> String -> H.ComponentHTML Action () m
makeEditorToolbarButton tooltip action biName = HH.button
  [ HP.classes [ HB.btn, HB.p0, HB.m0 ]
  , HP.title tooltip
  , HE.onClick \_ -> action
  ]
  [ HH.i
      [ HP.classes [ HB.bi, H.ClassName biName ]
      ]
      []
  ]

-- Here, no tooltip is needed as the text is shown in the button
makeEditorToolbarButtonWithText
  :: forall m. Boolean -> Action -> String -> String -> H.ComponentHTML Action () m
makeEditorToolbarButtonWithText asText action biName smallText = HH.button
  ( prependIf (not asText) (HP.title smallText)
      [ HP.classes [ HB.btn, HB.btnOutlineDark, HB.px1, HB.py0, HB.m0 ]
      , HP.style "white-space: nowrap;"
      , HE.onClick \_ -> action
      ]
  )
  ( prependIf asText
      (HH.small [ HP.style "margin-right: 0.25rem;" ] [ HH.text smallText ])
      [ HH.i
          [ HP.classes [ HB.bi, H.ClassName biName ]
          ]
          []
      ]
  )

buttonDivisor :: forall m. H.ComponentHTML Action () m
buttonDivisor = HH.div
  [ HP.classes [ HB.vr, HB.mx1 ] ]
  []
