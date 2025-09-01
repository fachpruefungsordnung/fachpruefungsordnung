module FPO.Components.Editor
  ( Action(..)
  , DragHandle(..)
  , ElementData
  , Input
  , LiveMarker
  , Output(..)
  , Query(..)
  , Path
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
import Data.Array (catMaybes, intercalate, mapMaybe, snoc, uncons, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (find, for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith)
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
import FPO.Dto.ContentDto (Content, ContentWrapper)
import FPO.Dto.ContentDto as ContentDto
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID)
import FPO.Dto.UserDto (getUserName)
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( AnnotatedMarker
  , CommentSection
  , FirstComment
  , TOCEntry
  , emptyTOCEntry
  )
import FPO.Util (prependIf)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (classes, enabled, ref, style, title) as HP
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

import Data.HashMap (HashMap, empty, toArrayBy, insert, delete, lookup, size)
import Effect.Console (log)
import FPO.Components.Editor.AceExtra 
  ( addClass
  , clearSelection
  , removeClass
  , screenToText
  , setAnchorPosition)
import Data.Int (toNumber)
import Web.UIEvent.MouseEvent as ME

type Path = Array Int

type ElementData = Maybe { tocEntry :: TOCEntry, revID :: Int, title :: String }

data DragHandle = DragStart | DragEnd

type State = FPOState
  ( docID :: DocumentID
  , mEditor :: Maybe Types.Editor
  , mTocEntry :: Maybe TOCEntry
  , mNodePath :: Maybe Path
  , title :: String
  , mContent :: Maybe Content
  -- comments
  -- Hashmaps for Annotations
  -- Row line -> Hashmap of Username -> how many times the use has comments in the line
  , markerAnnoHS :: HashMap Int (HashMap String Int)
  -- markerID -> old row position in Annotation
  , oldMarkerAnnoPos :: HashMap Int Int
  -- to move comment anchors
  , dragState :: Maybe { which :: DragHandle, lm :: LiveMarker }
  , startHandleMarkerId :: Maybe Int
  , endHandleMarkerId :: Maybe Int
  -- tmpLiveMarker is a temporary comment and marker. Only set, if first comment is sent in Comment component
  -- Otherwise delete them later
  , tmpLiveMarker :: Maybe LiveMarker
  , selectedLiveMarker :: Maybe LiveMarker
  -- comment markers and its corresponding livemarker
  -- mostly going to use livemarkers. Markers are used for API requests
  , markers :: Array AnnotatedMarker
  , liveMarkers :: Array LiveMarker
  , fontSize :: Int
  , mListener :: Maybe (HS.Listener Action)
  , resizeObserver :: Maybe RO.ResizeObserver
  , resizeSubscription :: Maybe SubscriptionId
  , showButtonText :: Boolean
  , showButtons :: Boolean
  -- for saving when closing window
  , mDirtyRef :: Maybe (Ref Boolean)
  , mBeforeUnloadL :: Maybe EventListener
  -- saved icon
  , showSavedIcon :: Boolean
  , mSavedIconF :: Maybe H.ForkId
  -- for periodically saving the content
  , mPendingDebounceF :: Maybe H.ForkId -- 2s-Timer
  , mPendingMaxWaitF :: Maybe H.ForkId -- 20s-Max-Timer
  -- note: this value is only used for initialisation and won't necessarily stay up to date
  -- it stores the needed input for the Init action. Receive did not work, as the page
  -- get's rendered over and over, meaning receive get's triggered over and over and the
  -- number of requests to the backend would be ridiculous
  , compareToElement :: ElementData

  , isEditorReadonly :: Boolean
  )

-- For tracking the comment markers live
-- Only store the values in save
type LiveMarker =
  { annotedMarkerID :: Int
  , startAnchor :: Types.Anchor
  , endAnchor :: Types.Anchor
  , markerText :: String
  , ref :: Ref Int
  }

type Input = { docID :: DocumentID, elementData :: ElementData }

data Output
  = AddComment Int Int
  | ClickedQuery (Array String)
  | DeletedComment TOCEntry (Array Int)
  | PostPDF String
  -- SavedSection toBePosted title TOCEntry
  | SavedSection Boolean String TOCEntry
  | RenamedNode String Path
  | RequestComments Int Int
  | SelectedCommentSection Int Int
  | ShowAllCommentsOutput Int Int

data Action
  = Init
  | Comment
  | ChangeToSection String TOCEntry (Maybe Int)
  | ContinueChangeToSection (Array FirstComment)
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
  | Upload TOCEntry String ContentWrapper
  -- Subsection of Upload
  | LostParentID TOCEntry String ContentWrapper
  | SavedIcon
  -- new change in editor -> reset timer
  | AutoSaveTimer
  -- called by AutoSaveTimer subscription
  | AutoSave
  | TryStartDrag Number Number -- clientX, clientY
  | StartDrag DragHandle LiveMarker Number Number -- mouse down: which, lm, clientX, clientY
  | DragMove Number Number -- mouse move: clientX, clientY
  | EndDrag -- mouse up
  | ShowHandles LiveMarker -- set Handles
  | HideHandles -- remove Handles
  | SetAnnotations
  | AddAnnotation LiveMarker Boolean
  | DeleteAnnotation LiveMarker Boolean Boolean
  | UpdateAnnotation LiveMarker
  | RenderHTML
  | PDF
  | ShowAllComments
  | Receive (Connected FPOTranslator Input)
  | HandleResize Number
  | Finalize

-- We use a query to get the content of the editor
data Query a
  -- = RequestContent (Array String -> a)
  = QueryEditor a
  -- | save the current content and send it to splitview
  | SaveSection a
  -- | receive the selected TOC and put its content into the editor
  | ChangeSection String TOCEntry (Maybe Int) a
  | ContinueChangeSection (Array FirstComment) a
  -- | Open and edit a raw string outside the TOCEntry structure.
  --   This is used to make the editor available for editing
  --   the section names of non-leaf nodes.
  | ChangeToNode String Path a
  -- | Update the position of a node in the editor, if existing.
  | UpdateNodePosition Path a
  | UpdateComment CommentSection a
  | SelectCommentSection Int a
  | ToDeleteComment a

-- | UpdateCompareToElement ElementData a

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
    { docID: input.docID
    , translator: fromFpoTranslator context
    , mEditor: Nothing
    , mTocEntry: Nothing
    , mNodePath: Nothing
    , title: ""
    , mContent: Nothing
    , tmpLiveMarker: Nothing
    , selectedLiveMarker: Nothing
    , markers: []
    , dragState: Nothing
    , startHandleMarkerId: Nothing
    , endHandleMarkerId: Nothing
    , markerAnnoHS: empty
    , oldMarkerAnnoPos: empty
    , liveMarkers: []
    , fontSize: 12
    , mListener: Nothing
    , resizeObserver: Nothing
    , resizeSubscription: Nothing
    , showButtonText: true
    , showButtons: true
    , mDirtyRef: Nothing
    , mBeforeUnloadL: Nothing
    , showSavedIcon: false
    , mSavedIconF: Nothing
    , mPendingDebounceF: Nothing
    , mPendingMaxWaitF: Nothing
    , compareToElement: input.elementData
    , isEditorReadonly: false
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn, HB.flexGrow1 ] ]
      [ HH.div
          -- toolbar
          [ HP.classes [ HB.dFlex, HB.justifyContentBetween ] ]
          if (not state.showButtons) then
            -- keep the toolbar even though there is not space, so that the screen doesnt pop higher
            [ HH.div
                [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
                [ HH.div
                    [ HP.style
                        "visibility: hidden; height: 1.5rem; min-height: 1.5rem;"
                    ]
                    []
                ]
            ]
          else
            [ HH.div
                [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
                [ makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_textBold") state.translator)
                    Bold
                    "bi-type-bold"
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_textItalic") state.translator)
                    Italic
                    "bi-type-italic"
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_textUnderline") state.translator)
                    Underline
                    "bi-type-underline"

                , buttonDivisor
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_fontSizeUp") state.translator)
                    FontSizeUp
                    "bi-plus-square"
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_fontSizeDown") state.translator)
                    FontSizeDown
                    "bi-dash-square"

                , buttonDivisor
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_undo") state.translator)
                    Undo
                    "bi-arrow-counterclockwise"
                , makeEditorToolbarButton
                    true
                    (translate (label :: _ "editor_redo") state.translator)
                    Redo
                    "bi-arrow-clockwise"

                , buttonDivisor
                , makeEditorToolbarButton
                    fullFeatures
                    (translate (label :: _ "editor_comment") state.translator)
                    Comment
                    "bi-chat-square-text"
                , makeEditorToolbarButton
                    fullFeatures
                    (translate (label :: _ "editor_deleteComment") state.translator)
                    DeleteComment
                    "bi-chat-square-text-fill"

                ]
            , HH.div
                [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ]
                , HP.style "min-width: 0;"
                ]
                [ makeEditorToolbarButtonWithText
                    true
                    state.showButtonText
                    Save
                    "bi-floppy"
                    (translate (label :: _ "editor_save") state.translator)
                , makeEditorToolbarButtonWithText
                    true
                    state.showButtonText
                    RenderHTML
                    "bi-file-richtext"
                    (translate (label :: _ "editor_preview") state.translator)
                , makeEditorToolbarButtonWithText
                    true
                    state.showButtonText
                    PDF
                    "bi-filetype-pdf"
                    (translate (label :: _ "editor_pdf") state.translator)
                , makeEditorToolbarButtonWithText
                    fullFeatures
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
          [ -- Add overlay when readonly
            if state.isEditorReadonly then
              HH.div
                [ HP.classes
                    [ HB.positionAbsolute
                    , HB.top0
                    , HB.start0
                    , HB.w100
                    , HB.h100
                    , HB.dFlex
                    , HB.justifyContentCenter
                    , HB.alignItemsEnd
                    , HB.peNone
                    ]
                , HP.style
                    "background: rgba(0,0,0,0.1); z-index: 20; padding-bottom: 1.5rem;"
                ]
                [ HH.div
                    [ HP.classes
                        [ HB.bgLight
                        , HB.border
                        , HB.rounded
                        , HB.px3
                        , HB.py2
                        , HB.shadow
                        ]
                    , HP.style "pointer-events: auto;"
                    ]
                    [ HH.i
                        [ HP.classes [ HB.bi, H.ClassName "bi-lock-fill", HB.me2 ] ]
                        []
                    , HH.text
                        (translate (label :: _ "editor_readonly") state.translator)
                    ]
                ]
            else
              HH.text ""

          ]
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
    where
    fullFeatures = isJust state.mTocEntry

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Init -> do
      -- create subscription for later use
      state <- H.get
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
          Editor.setReadOnly state.isEditorReadonly editor_

      -- New Ref for keeping track, if the content in editor has changed
      -- since last save
      dref <- H.liftEffect $ Ref.new false
      H.modify_ _ { mDirtyRef = Just dref }

      win <- H.liftEffect window
      let
        winTarget = Win.toEventTarget win
        -- creating EventTypes
        beforeunload = EventType "beforeunload"

      -- create eventListener for preventing the tab from closing
      -- when content has not been saved (Not changing through Navbar)
      buL <- H.liftEffect $ eventListener \ev -> do
        readRef <- traverse Ref.read (Just dref)
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
      compareTo <- H.gets _.compareToElement
      case compareTo of
        Nothing
        -> pure unit
        Just { tocEntry: tocEntry, revID: revID, title: title }
        -> handleAction (ChangeToSection title tocEntry (Just revID))

      -- add and start Editor listeners
      H.gets _.mEditor >>= traverse_ \ed -> do

        -- change Editor content listener
        H.liftEffect $ addChangeListenerWithRef ed dref listener
        container <- H.liftEffect $ Editor.getContainer ed

        -- Mouse events
        
        downL <- H.liftEffect $ eventListener \ev -> do
          case ME.fromEvent ev of
            Just mev -> do
              let x = toNumber (ME.clientX mev)
                  y = toNumber (ME.clientY mev)
              -- try to drag comment section dragger marker
              HS.notify listener (TryStartDrag x y)
            Nothing ->
              pure unit
        H.liftEffect $ addEventListener (EventType "mousedown") downL true (toEventTarget $ toElement container)

        moveL <- H.liftEffect $ eventListener \ev -> do
          case ME.fromEvent ev of
            Just mev -> do
              let x = toNumber (ME.clientX mev)
                  y = toNumber (ME.clientY mev)
              HS.notify listener (DragMove x y)
            Nothing -> pure unit
        H.liftEffect $ addEventListener (EventType "mousemove") moveL true (toEventTarget $ toElement container)

        upL <- H.liftEffect $ eventListener \_ -> do
          -- find potentially selected Comment
          HS.notify listener SelectComment
          -- stop dragging the comment dragger
          HS.notify listener EndDrag
          -- set dirty flag and autosave
          Ref.write true dref
          HS.notify listener AutoSaveTimer
        H.liftEffect $ addEventListener (EventType "mouseup") upL true (toEventTarget $ toElement container)

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

    Bold -> do
      state <- H.get
      when (not state.isEditorReadonly) $ do
        H.gets _.mEditor >>= traverse_ \ed ->
          H.liftEffect $ do
            makeBold ed
            Editor.focus ed

    Italic -> do
      state <- H.get
      when (not state.isEditorReadonly) $ do
        H.gets _.mEditor >>= traverse_ \ed ->
          H.liftEffect $ do
            makeItalic ed
            Editor.focus ed

    Underline -> do
      state <- H.get
      when (not state.isEditorReadonly) $ do
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
      state <- H.get
      when (not state.isEditorReadonly) $ do
        H.gets _.mEditor >>= traverse_ \ed -> do
          H.liftEffect $ do
            Editor.undo ed
            Editor.focus ed

    Redo -> do
      state <- H.get
      when (not state.isEditorReadonly) $ do
        H.gets _.mEditor >>= traverse_ \ed -> do
          H.liftEffect $ do
            Editor.redo ed
            Editor.focus ed

    RenderHTML -> do
      _ <- handleQuery (QueryEditor unit)
      pure unit

    PDF -> do
      allLines <- H.gets _.mEditor >>= traverse \ed -> do
        H.liftEffect $ Editor.getSession ed
          >>= Session.getDocument
          >>= Document.getAllLines
      let
        content = case allLines of
          Nothing -> ""
          Just ls -> intercalate "\n" ls
      H.raise (PostPDF content)

    ShowAllComments -> do
      state <- H.get
      let tocID = maybe (-1) _.id state.mTocEntry
      H.raise $ ShowAllCommentsOutput state.docID tocID

    -- Save section

    Save -> do
      state <- H.get
      when (not state.isEditorReadonly) $ do
        isDirty <- EC.liftEffect $ Ref.read =<< case state.mDirtyRef of
          Just r -> pure r
          Nothing -> EC.liftEffect $ Ref.new false
        when isDirty $ do
          allLines <- H.gets _.mEditor >>= traverse \ed -> do
            H.liftEffect $ Editor.getSession ed
              >>= Session.getDocument
              >>= Document.getAllLines

          let
            contentLines =
              fromMaybe { title: "", contentText: "" } do
                { head, tail } <- uncons =<< allLines
                pure { title: head, contentText: intercalate "\n" tail }

          case state.mTocEntry of
            Nothing -> do
              -- No leaf entity was selected, so if a nodePath is set,
              -- we can emit an event to rename the node.
              case state.mNodePath of
                Nothing -> do
                  pure unit -- Nothing to do
                Just path -> do
                  H.raise $ RenamedNode (contentLines.title) path
            Just _ ->
              case state.mContent of
                Nothing -> pure unit
                Just content -> do
                  -- Save the current content of the editor and send it to the server
                  let
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
                    for state.markers \m -> do
                      case
                        find (\lm -> lm.annotedMarkerID == m.id) state.liveMarkers
                        of
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

                  -- convert into a newtype for encodeJson
                  let
                    comments = map ContentDto.convertToCommentAnchor updatedMarkers
                    newWrapper = ContentDto.setWrapper newContent comments
                  -- Try to upload
                  handleAction $ Upload entry title newWrapper

    Upload newEntry title newWrapper -> do
      state <- H.get
      let
        jsonContent = ContentDto.encodeWrapper newWrapper
        newContent = ContentDto.getWrapperContent newWrapper
      -- send the new content as POST to the server
      response <- Request.postJson (ContentDto.extractNewParent newContent)
        ("/docs/" <> show state.docID <> "/text/" <> show newEntry.id <> "/rev")
        jsonContent

      -- handle errors in pos and decodeJson
      case response of
        Left _ -> handleAction $ LostParentID newEntry title newWrapper
        -- extract and insert new parentID into newContent
        Right updatedContent -> do
          -- Update the tree to backend, when title was really changed
          let oldTitle = state.title
          H.raise (SavedSection (oldTitle /= title) title newEntry)

          H.modify_ _
            { mTocEntry = Just newEntry
            , title = title
            , mContent = Just updatedContent
            }

          -- Show saved icon
          handleAction SavedIcon

          -- mDirtyRef := false
          for_ state.mDirtyRef \r -> H.liftEffect $ Ref.write false r
          pure unit

    LostParentID newEntry title newWrapper -> do
      let newContent = ContentDto.getWrapperContent newWrapper
      docID <- H.gets _.docID
      loadedContent <- H.liftAff $
        Request.getFromJSONEndpoint
          ContentDto.decodeContent
          ("/docs/" <> show docID <> "/text/" <> show newEntry.id <> "/rev/latest")
      case loadedContent of
        -- TODO: Error handling
        Nothing -> pure unit
        Just res ->
          let
            newContent' = ContentDto.setContentText
              (ContentDto.getContentText newContent)
              res
            newWrapper' = ContentDto.setWrapperContent newContent' newWrapper
          in
            handleAction $ Upload newEntry title newWrapper'

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
      state <- H.get
      userWithError <- getUser
      case userWithError, state.mListener of
        Right user, Just listener -> do
          H.gets _.mEditor >>= traverse_ \ed -> do
            session <- H.liftEffect $ Editor.getSession ed
            range <- H.liftEffect $ Editor.getSelectionRange ed
            start <- H.liftEffect $ Range.getStart range
            end <- H.liftEffect $ Range.getEnd range

            -- delete temporary live marker, as the first comment has not been sent
            case state.tmpLiveMarker of
              Just lm -> do
                H.liftEffect $ removeLiveMarker lm session
                handleAction $ DeleteAnnotation lm false false
              Nothing -> pure unit

            let
              sRow = Types.getRow start
              sCol = Types.getColumn start
              eRow = Types.getRow end
              eCol = Types.getColumn end
              userName = getUserName user
              newMarker =
                { id: -360
                , type: "info"
                , startRow: sRow
                , startCol: sCol
                , endRow: eRow
                , endCol: eCol
                , markerText: userName
                , mCommentSection: Nothing
                }

            mLiveMarker <- H.liftEffect $ addAnchor newMarker session listener true
            handleAction SetAnnotations

            case state.mTocEntry of
              Just entry -> do
                H.modify_ \st ->
                  st { tmpLiveMarker = mLiveMarker, selectedLiveMarker = mLiveMarker }
                H.raise (AddComment state.docID entry.id)
              Nothing -> pure unit
            -- show comment dragger handles
            case mLiveMarker of
              Nothing -> pure unit
              Just lm -> do
                H.liftEffect $ highlightSelection ed (snoc state.liveMarkers lm) lm
                handleAction (ShowHandles lm)
            -- remove the selection
            H.liftEffect $ clearSelection ed

        _, _ -> pure unit -- TODO error handling 

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
                --   newMarkers = filter (\m -> not (m.id == foundID)) tocEntry.markers
                --   newLiveMarkers =
                --     filter (\l -> not (l.annotedMarkerID == foundID))
                --       state.liveMarkers
                --  newTOCEntry = tocEntry { markers = newMarkers }
                H.liftEffect $ removeLiveMarker lm session
                H.modify_ \st ->
                  st { mTocEntry = Just tocEntry } --, liveMarkers = newLiveMarkers }
                H.raise (DeletedComment tocEntry [ foundID ])

    SelectComment -> do
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        let
          liveMarkers = case state.tmpLiveMarker of
            Nothing -> state.liveMarkers
            Just lm -> snoc state.liveMarkers lm
        cursor <- H.liftEffect $ Editor.getCursorPosition ed
        session <- H.liftEffect $ Editor.getSession ed
        foundLM <- H.liftEffect $ cursorInRange liveMarkers cursor
        -- comment section dragger handles
        case foundLM of
          Nothing -> do
            -- remove selection and remove handles
            H.modify_ _ { selectedLiveMarker = Nothing, dragState = Nothing }
            case state.selectedLiveMarker of
              Nothing -> pure unit
              Just lm  -> H.liftEffect $ setMarkerSelectedClass session lm false
            handleAction HideHandles
          Just lm -> do
            -- set selection and highlight it
            H.modify_ _ { selectedLiveMarker = Just lm }
            H.liftEffect $ highlightSelection ed liveMarkers lm
            handleAction (ShowHandles lm)
        let
          lm = case foundLM of
            Nothing -> failureLiveMarker
            Just found -> found
          foundID = lm.annotedMarkerID

          -- need it only for its ID
          tocEntry = case state.mTocEntry of
            Nothing -> emptyTOCEntry
            Just e -> e
        H.modify_ \st -> st {selectedLiveMarker = Just lm}
        when (foundID >= 0 || foundID == -360) $
          H.raise (SelectedCommentSection tocEntry.id foundID)

    -- Comment Section Dragger Actions

    -- Try to get mouse position and maybe selected handle
    TryStartDrag clientX clientY -> do
      st <- H.get
      case st.mEditor, st.selectedLiveMarker of
        Just ed, Just lm -> do
          -- Mauspos -> Textpos
          pos  <- H.liftEffect $ screenToText ed clientX clientY
          sPos <- H.liftEffect $ Anchor.getPosition lm.startAnchor
          ePos <- H.liftEffect $ Anchor.getPosition lm.endAnchor

          -- help function
          let near a b =
                Types.getRow a == Types.getRow b
                && abs (Types.getColumn a - Types.getColumn b) <= 1

          if near pos sPos then
            handleAction (StartDrag DragStart lm clientX clientY)
          else if near pos ePos then
            handleAction (StartDrag DragEnd   lm clientX clientY)
          else
            pure unit
        _, _ -> pure unit
    
    StartDrag which lm _clientX _clientY -> do
      st <- H.get
      when (not st.isEditorReadonly) do
        case st.mEditor of
          Just ed -> do
            session <- H.liftEffect $ Editor.getSession ed
            container <- H.liftEffect $ Editor.getContainer ed
            -- For CSS identification
            H.liftEffect do
              addClass container "fpo-no-select"
              addClass container "fpo-dragging"
            -- remove old Handles
            H.liftEffect $ hideHandlesFrom session st.startHandleMarkerId st.endHandleMarkerId
            -- set new Handles
            ids <- H.liftEffect $ showHandlesFor session lm 
            H.modify_ _ { startHandleMarkerId = ids.startId
                        , endHandleMarkerId   = ids.endId
                        }
          Nothing -> pure unit
        -- dtart drag mode: remember which handle and liveMarker
        H.modify_ _ { dragState = Just { which, lm } }
    
    DragMove clientX clientY -> do
      st <- H.get
      case st.dragState, st.mEditor of
        Just { which, lm }, Just ed -> do
          -- screen -> Textcoord.
          pos <- H.liftEffect $ screenToText ed clientX clientY
          let row = Types.getRow pos
          let col = Types.getColumn pos
          -- set drag anchor
          case which of
            DragStart -> H.liftEffect $ setAnchorPosition lm.startAnchor row col
            DragEnd -> H.liftEffect $ setAnchorPosition lm.endAnchor row col

          -- draw new Handles (current position)
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ hideHandlesFrom session st.startHandleMarkerId st.endHandleMarkerId
          ids <- H.liftEffect $ showHandlesFor session lm
          H.modify_ _ { startHandleMarkerId = ids.startId, endHandleMarkerId = ids.endId }
        _, _ -> pure unit


    EndDrag -> do
      dragState <- H.gets _.dragState
      when (isJust dragState) do
        H.modify_ _ { dragState = Nothing }
        st <- H.get
        case st.mEditor of
          Just ed -> do
            container <- H.liftEffect $ Editor.getContainer ed
            -- For CSS styling
            H.liftEffect do
              removeClass container "fpo-no-select"
              removeClass container "fpo-dragging"
              -- remove the selected text in editor
              clearSelection ed
          Nothing -> pure unit

    ShowHandles lm -> do
      st <- H.get
      case st.mEditor of
        Nothing  -> pure unit
        Just ed -> do
          session <- H.liftEffect $ Editor.getSession ed
          -- remove old markers
          H.liftEffect $ hideHandlesFrom session st.startHandleMarkerId st.endHandleMarkerId
          -- set new ones
          ids <- H.liftEffect $ showHandlesFor session lm  -- :: { startId :: Maybe Int, endId :: Maybe Int }
          H.modify_ _ { startHandleMarkerId = ids.startId
                      , endHandleMarkerId   = ids.endId
                      }

    HideHandles -> do
      st <- H.get
      case st.mEditor of
        Nothing  -> pure unit
        Just ed -> do
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ hideHandlesFrom session st.startHandleMarkerId st.endHandleMarkerId
          H.modify_ _ { startHandleMarkerId = Nothing
                      , endHandleMarkerId   = Nothing
                      }

    -- Convert hash map into annotations and set them into editor
    SetAnnotations -> do
      markerAnnoHS <- H.gets _.markerAnnoHS
      H.gets _.mEditor >>= traverse_ \ed -> H.liftEffect do
        session <- Editor.getSession ed
        let
          -- extract information from markerAnnoHS
          tmp = toArrayBy (\k v -> {line: k, text: joinWith ", " (toArrayBy addNames v)}) markerAnnoHS
          -- map it to correct Annotation type
          anns =
            map (\{line, text} -> { row: line, column: 1, text: text, type: "info"}) tmp
        Session.setAnnotations anns session
        
    AddAnnotation lm setAnn-> do
      state <- H.get
      pos <- H.liftEffect $ Anchor.getPosition lm.startAnchor
      let 
        startRow = Types.getRow pos
        newOldMarkerAnnoPos = insert lm.annotedMarkerID startRow state.oldMarkerAnnoPos
        newMarkerAnnoHS = case lookup startRow state.markerAnnoHS of
          Nothing -> 
            let 
              newEntry = insert lm.markerText 1 empty 
            in
              insert startRow newEntry state.markerAnnoHS
          Just entry -> 
            let 
              oldValue = fromMaybe 0 (lookup lm.markerText entry)
              newEntry = insert lm.markerText (oldValue+1) entry
            in
              insert startRow newEntry state.markerAnnoHS
      H.modify_ \st -> st 
        { markerAnnoHS = newMarkerAnnoHS
        , oldMarkerAnnoPos = newOldMarkerAnnoPos
        }
      when setAnn (handleAction SetAnnotations)

    DeleteAnnotation lm reAdd setAnn -> do
      state <- H.get
      let
        -- get old row number for this marker
        oldRow = fromMaybe 0 (lookup lm.annotedMarkerID state.oldMarkerAnnoPos)
        -- update Annotation HashMap
        newMarkerAnnoHS = case lookup oldRow state.markerAnnoHS of
          -- should not happen
          Nothing -> state.markerAnnoHS
          -- update this HashMap entry
          Just entry -> do
            let
              -- if 1 then delete it, bigger just decreament the value
              oldValue = fromMaybe 0 (lookup lm.markerText entry)
              newEntry =
                if oldValue <= 1 then
                  delete lm.markerText entry
                else
                  insert lm.markerText (oldValue-1) entry
            -- delete entry if empty
            if ((size newEntry) == 0) then
              delete oldRow state.markerAnnoHS
            else
              insert oldRow newEntry state.markerAnnoHS
      H.modify_ \st -> st {markerAnnoHS = newMarkerAnnoHS}
      -- if we want to update the live marker annotation
      if reAdd then
        handleAction $ AddAnnotation lm setAnn
      else
        when setAnn $
          handleAction SetAnnotations
    
    -- delete and readd Annotation if the startRow of live marker has changed
    UpdateAnnotation lm -> do
      oldMarkerAnnoPos <- H.gets _.oldMarkerAnnoPos
      -- get startRow from live marker
      pos <- H.liftEffect $ Anchor.getPosition lm.startAnchor
      let startRow = Types.getRow pos

      -- checking if the startRow has changed
      case lookup lm.annotedMarkerID oldMarkerAnnoPos of
        Nothing -> pure unit
        Just oldRow -> 
          when (startRow /= oldRow) $
            handleAction $ DeleteAnnotation lm true true

    HandleResize width -> do
      -- Decides whether to show button text based on the width.
      -- Because german labels are longer, we need to adjust the cutoff
      -- threshold dynamically. Pretty sure this is not the best solution,
      -- but it works.

      lang <- liftEffect $ Store.loadLanguage
      let cutoff = if lang == Just "de-DE" then 690.0 else 592.0
      let noButtonsCutoff = 350.0

      H.modify_ _
        { showButtonText = width >= cutoff, showButtons = width >= noButtonsCutoff }

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

    ChangeToSection title entry rev -> do
      state <- H.get
      -- Prevent of loading the same Section from backend again
      when (Just entry /= state.mTocEntry) do
        H.modify_ \st -> st { mTocEntry = Just entry, title = title }
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
            ContentDto.decodeContentWrapper
            ( "/docs/" <> show state.docID <> "/text/" <> show entry.id
                <> "/rev/"
                <> version
            )
        let
          wrapper = case loadedContent of
            Nothing -> ContentDto.failureContentWrapper
            Just res -> res
          content = ContentDto.getWrapperContent wrapper
          comments = ContentDto.getWrapperComments wrapper
          -- convert markers
          markers = map ContentDto.convertToAnnotetedMarker comments

        H.modify_ \st -> st
          { mContent = Just content
          , selectedLiveMarker = Nothing
          , markerAnnoHS = empty
          , oldMarkerAnnoPos = empty
          , markers = markers
          , isEditorReadonly = version /= "latest"
          }
        -- Get comments information from Comment Child
        H.raise (RequestComments state.docID entry.id)
      pure unit
    
    -- After getting information from from Comment
    ContinueChangeToSection fCs -> do
      state <- H.get
      case state.mListener of
        Nothing -> pure unit
        Just listener -> do
          -- Put the content of the section into the editor and update markers
          H.gets _.mEditor >>= traverse_ \ed -> do
            let
              filMarkers = updateMarkers fCs state.markers
              content = case state.mContent of
                Nothing -> ""
                Just c -> ContentDto.getContentText c

            newLiveMarkers <- H.liftEffect do
              session <- Editor.getSession ed
              document <- Session.getDocument session

              -- Set the content of the editor
              Document.setValue (state.title <> "\n" <> content)
                document
              Editor.setReadOnly state.isEditorReadonly ed

              -- reset Ref, because loading new content is considered 
              -- changing the existing content, which would set the flag
              for_ state.mDirtyRef \r -> H.liftEffect $ Ref.write false r

              -- Reset Undo history
              undoMgr <- Session.getUndoManager session
              UndoMgr.reset undoMgr

              -- Remove existing markers
              for_ state.liveMarkers \lm -> do
                removeLiveMarker lm session

              -- Clear annotations
              Session.clearAnnotations session

              -- Add annotations from marker
              tmp <- for filMarkers \marker -> do
                addAnchor marker session listener true

              pure (catMaybes tmp)

            -- Update state with new marker IDs
            H.modify_ \st ->
              st { liveMarkers = newLiveMarkers }
              
      -- convert Hashmap to Annotations and show them
      handleAction SetAnnotations

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ChangeSection title entry rev a -> do
      handleAction (ChangeToSection title entry rev)
      pure (Just a)

    ContinueChangeSection fCs a -> do
      handleAction (ContinueChangeToSection fCs)
      pure (Just a)

    ChangeToNode title path a -> do
      -- Change the editor to a raw string outside the TOCEntry structure.
      H.modify_ _ { mTocEntry = Nothing, mNodePath = Just path }
      state <- H.get
      H.gets _.mEditor >>= traverse_ \ed -> do
        -- Set the content of the editor
        H.liftEffect $ do
          session <- Editor.getSession ed
          document <- Session.getDocument session
          Document.setValue title document

          -- Reset Undo history
          undoMgr <- Session.getUndoManager session
          UndoMgr.reset undoMgr

          -- Clear annotations
          Session.clearAnnotations session

      -- reset Ref, because loading new content is considered
      -- changing the existing content, which would set the flag
      for_ state.mDirtyRef \r -> H.liftEffect $ Ref.write false r
      pure (Just a)

    UpdateNodePosition path a -> do
      H.modify_ \state ->
        case state.mNodePath of
          Just _ -> state { mNodePath = Just path }
          Nothing -> state
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

    -- Repurpose it to confirm, that the first comment was sent
    UpdateComment newCommentSection a -> do
      state <- H.get
      case state.tmpLiveMarker, state.mEditor, state.mListener, newCommentSection.first of
        Just lm, Just ed, Just listener, Just first -> do
          start <- H.liftEffect $ Anchor.getPosition lm.startAnchor
          end <- H.liftEffect $ Anchor.getPosition lm.endAnchor
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ removeLiveMarker lm session
          handleAction $ DeleteAnnotation lm false false
          let
            newMarker =
              { id: newCommentSection.markerID
              , type: "info"
              , startRow: Types.getRow start
              , startCol: Types.getColumn start
              , endRow: Types.getRow end
              , endCol: Types.getColumn end
              , markerText: first.author
              , mCommentSection: Just newCommentSection
              }
            newMarkers = snoc state.markers newMarker
            -- delete temp id from hash map
            newOldMarkerAnnoPos = delete (-360) state.oldMarkerAnnoPos
            -- add the real id instead
            newOldMarkerAnnoPos' = insert newMarker.id newMarker.startRow newOldMarkerAnnoPos
          newLiveMarker <- H.liftEffect $ addAnchor newMarker session listener true
          let
            newLM = case newLiveMarker of
              Nothing -> failureLiveMarker
              Just lm' -> lm'
            newLiveMarkers = case newLiveMarker of
              Nothing -> state.liveMarkers
              Just lm' -> snoc state.liveMarkers lm'
          H.modify_ \st -> st
            { oldMarkerAnnoPos = newOldMarkerAnnoPos'
            , tmpLiveMarker = Nothing
            , selectedLiveMarker = Just newLM
            , markers = newMarkers
            , liveMarkers = newLiveMarkers
            }
          -- set dirty to true to be able to save
          for_ state.mDirtyRef \r -> H.liftEffect $ Ref.write true r
          handleAction Save
        _, _, _, _ -> pure unit
      pure (Just a)
    
    -- Comes from CommentOverview
    SelectCommentSection markerID a -> do
      lms <- H.gets _.liveMarkers
      editor_ <- H.gets _.mEditor
      case (find (\m -> m.annotedMarkerID == markerID) lms) of
        Nothing -> pure unit
        Just lm -> do
          H.modify_ \st -> st { selectedLiveMarker = Just lm }
          -- show comment drag handles
          case editor_ of
            Nothing -> pure unit
            Just ed -> H.liftEffect $ highlightSelection ed lms lm
          handleAction (ShowHandles lm)
      pure (Just a)
    
    ToDeleteComment a -> do
      state <- H.get
      H.liftEffect $ log "ToDeleteComment"
      case state.mEditor, state.selectedLiveMarker of
        Just ed, Just lm -> do
          H.liftEffect $ log $ "passed case: " <> show lm.annotedMarkerID
          session <- H.liftEffect $ Editor.getSession ed
          H.liftEffect $ removeLiveMarker lm session
          handleAction $ DeleteAnnotation lm false true
          H.modify_ \st -> st {selectedLiveMarker = Nothing}
          case state.tmpLiveMarker of
            Nothing -> pure unit
            Just tmpLM -> when (tmpLM.annotedMarkerID == lm.annotedMarkerID) $
              H.modify_ \st -> st {tmpLiveMarker = Nothing}
        _, _ -> pure unit
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

addAnchor 
  :: AnnotatedMarker 
  -> Types.EditSession
  -> HS.Listener Action 
  -> Boolean
  -> Effect (Maybe LiveMarker)
addAnchor marker session listener action =
  if (marker.startRow == marker.endRow && marker.startCol == marker.endCol) then
    pure Nothing -- No valid range, so no marker
  else do
    document <- Session.getDocument session
    startAnchor <- Document.createAnchor marker.startRow marker.startCol document
    endAnchor <- Document.createAnchor marker.endRow marker.endCol document
    Anchor.setInsertRight true endAnchor

    range <- createMarkerRange marker
    id <- Session.addMarker range "my-marker" "text" false session
    markerRef <- Ref.new id

    let
      lm = 
        { annotedMarkerID: marker.id
        , startAnchor: startAnchor
        , endAnchor: endAnchor
        , markerText: marker.markerText
        , ref: markerRef
        }
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
          "text"
          false
          session

        Ref.write newId markerRef
        HS.notify listener (UpdateAnnotation lm)
        pure unit

    Anchor.onChange startAnchor rerenderMarker
    Anchor.onChange endAnchor rerenderMarker
    when action $
      HS.notify listener (AddAnnotation lm false)
    --addAnnotation (markerToAnnotation marker) session
    pure (Just lm)

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
  :: forall m. Boolean -> String -> Action -> String -> H.ComponentHTML Action () m
makeEditorToolbarButton enabled tooltip action biName = HH.button
  [ HP.classes
      ( prependIf (not enabled) HB.opacity25
          [ HB.btn, HB.p0, HB.m0, HB.border0 ]
      )
  , HP.title tooltip
  , HE.onClick \_ -> action
  , HP.enabled enabled
  ]
  [ HH.i
      [ HP.classes [ HB.bi, H.ClassName biName ]
      ]
      []
  ]

-- Here, no tooltip is needed as the text is shown in the button
makeEditorToolbarButtonWithText
  :: forall m
   . Boolean
  -> Boolean
  -> Action
  -> String
  -> String
  -> H.ComponentHTML Action () m
makeEditorToolbarButtonWithText enabled asText action biName smallText = HH.button
  ( prependIf (not asText) (HP.title smallText)
      [ HP.classes
          ( prependIf (not enabled) HB.opacity25
              [ HB.btn, HB.btnOutlineDark, HB.px1, HB.py0, HB.m0 ]
          )
      , HP.style "white-space: nowrap;"
      , HE.onClick \_ -> action
      , HP.enabled enabled
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

updateMarkers :: Array FirstComment -> Array AnnotatedMarker -> Array AnnotatedMarker
updateMarkers firsts markers =
  mapMaybe (\m ->
      case Array.find (\fs -> fs.markerID == m.id && not fs.resolved) firsts of
        Just fs -> Just (m { markerText = fs.first.author })
        Nothing -> Nothing
    ) 
    markers

-- Help function for markerAnnoHS
addNames :: String -> Int -> String
addNames name occurence = 
  if occurence == 1 then
    name
  else 
    name <> "+" <> show occurence

failureLiveMarker :: LiveMarker
failureLiveMarker =
  { annotedMarkerID: -1
  , startAnchor: unsafeCoerce unit  -- Fake Anchor
  , endAnchor: unsafeCoerce unit    -- Fake Anchor
  , markerText: ""
  , ref: unsafeCoerce (-1 :: Int)   -- Fake Ref
  }

showHandlesFor :: Types.EditSession -> LiveMarker -> Effect { startId :: Maybe Int, endId :: Maybe Int }
showHandlesFor session lm = do
  -- Start-Handle
  Types.Position { row: sRow, column: sCol } <- Anchor.getPosition lm.startAnchor
  r1 <- Range.create sRow sCol sRow (sCol + 1)
  hid1 <- Session.addMarker r1 "fpo-handle-start" "text" false session
  -- End-Handle
  Types.Position { row: eRow, column: eCol } <- Anchor.getPosition lm.endAnchor
  r2 <- Range.create (eRow-1) eCol (eRow-1) (eCol + 1)
  hid2 <- Session.addMarker r2 "fpo-handle-end" "text" false session
  pure { startId: Just hid1, endId: Just hid2 }


hideHandlesFrom :: Types.EditSession -> Maybe Int -> Maybe Int -> Effect Unit
hideHandlesFrom session m1 m2 = do
  for_ m1 \i -> Session.removeMarker i session
  for_ m2 \i -> Session.removeMarker i session

abs :: Int -> Int
abs x = if x < 0 then (x*(-1)) else x

-- Give the marker the correct class depending on isSelected
setMarkerSelectedClass
  :: Types.EditSession
  -> LiveMarker
  -> Boolean            -- ^ isSelected?
  -> Effect Unit
setMarkerSelectedClass session lm isSelected = do
  -- 1) remove old Ace-Marker entfernen
  oldId <- Ref.read lm.ref
  Session.removeMarker oldId session

  -- 2) calculate new Range from Anchors
  Types.Position { row: sRow, column: sCol } <- Anchor.getPosition lm.startAnchor
  Types.Position { row: eRow, column: eCol } <- Anchor.getPosition lm.endAnchor
  range <- Range.create sRow sCol eRow eCol

  -- 3) set new class
  let cls =
        if isSelected then "my-marker selected"
        else "my-marker"
  newId <- Session.addMarker range cls "text" false session

  -- 4) new Marker-ID
  Ref.write newId lm.ref

highlightSelection
  :: Types.Editor
  -> Array LiveMarker
  -> LiveMarker      -- ^ selectedLiveMarker
  -> Effect Unit
highlightSelection ed lms mSel = do
  session <- Editor.getSession ed
  for_ lms \lm ->
    setMarkerSelectedClass session lm (lm.annotedMarkerID == mSel.annotedMarkerID)