module FPO.Components.Comment where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (filter, find, findIndex, modifyAt, snoc)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Formatter.DateTime (Formatter)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import FPO.Components.UI.RenderComment (renderComment, renderFirstComment)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.CommentDto as CD
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( CommentSection
  , FirstComment
  , cdCommentToComment
  , extractFirst
  , hasProblems
  , sectionDtoToCS
  , updateFirstCommentProblem
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import FPO.UI.Css as HB
import Simple.I18n.Translator (label, translate)

type Input = Unit

data Output
  = CloseCommentSection
  | ConfirmComment CommentSection
  | CommentOverview (Array FirstComment)
  | SendAbstractedComments (Array FirstComment) Boolean
  | ToDeleteComment Int Boolean
  | UpdatedComments (Array FirstComment) Boolean
  | SetReAnchor (Maybe CommentSection)
  | ShowResolveButton Boolean

data Action
  = Init
  | Receive (Connected FPOTranslator Input)
  | DoNothing
  | UpdateDraft String
  | SendComment
  | ResolveComment
  | DeleteComment
  | SelectingCommentSection Int
  | RequestModal Mode
  | CancelModal
  | ShowDiscardPopover
  | HideDiscardPopover

data Query a
  = AddComment a
  | ReceiveTimeFormatter (Maybe Formatter) a
  | RequestComments Int Int (Array Int) Boolean a
  | SelectedCommentSection Int a
  | Overview a
  | UpdateComment (Array Int) a
  | ReaddedAnchor a
  | UpdateCommentProblem Int a
  | RequestResolve a

type State = FPOState
  ( docID :: Int
  , tocID :: Int
  , markerID :: Int
  , commentSections :: Array CommentSection
  , commentSectionsLoaded :: Boolean
  , mCommentSection :: Maybe CommentSection
  , newComment :: Boolean
  , commentDraft :: String
  , mTimeFormatter :: Maybe Formatter
  , mCurrentTime :: Maybe DateTime
  , requestModal :: Maybe Mode
  , showDiscardPopover :: Boolean
  , inLatest :: Boolean
  )

data Mode = Delete | Resolve

derive instance eqMode :: Eq Mode

commentview
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component Query Input Output m
commentview = connect selectTranslator $ H.mkComponent
  { initialState: \{ context } ->
      { translator: fromFpoTranslator context
      , docID: -1
      , tocID: -1
      , markerID: -1
      , commentSections: []
      , commentSectionsLoaded: false
      , mCommentSection: Nothing
      , newComment: false
      , commentDraft: ""
      , mTimeFormatter: Nothing
      , mCurrentTime: Nothing
      , requestModal: Nothing
      , showDiscardPopover: false
      , inLatest: true
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }
  where

  render :: State -> H.ComponentHTML Action () m
  render state = case state.mCommentSection of
    Nothing ->
      if state.newComment then
        HH.div [ HP.classes [ H.ClassName "fpo-comment-input" ] ]
          [ renderInput state ]
      else
        HH.text ""
    Just commentSection ->
      HH.div [ HP.style "padding: 0 0.25rem;" ]
        ( renderComments state commentSection
            <>
              -- "Resolved" notice when thread is resolved
              ( if commentSection.resolved then
                  [ HH.div [ HP.classes [ H.ClassName "fpo-comment-resolved-notice" ] ]
                      [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check-circle-fill" ] ] []
                      , HH.span_ [ HH.text
                          (translate (label :: _ "comment_resolved_notice") state.translator) ]
                      ]
                  ]
                else
                  [ renderInput state ]
              )
            <>
              -- End of conversation hint
              [ HH.div [ HP.classes [ H.ClassName "fpo-comment-end" ] ]
                  [ HH.span [ HP.classes [ H.ClassName "fpo-comment-end__text" ] ]
                      [ HH.text
                          (translate (label :: _ "comment_end_of_conversation") state.translator)
                      ]
                  ]
              ]
        )

  renderComments
    :: State
    -> CommentSection
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  renderComments state commentSection = case commentSection.first of
    Nothing -> [ HH.text "" ]
    Just first ->
      [ HH.div
          [ HP.classes [ HB.m1, HB.dFlex, HB.alignItemsCenter, HB.gap1 ] ]
          [ HH.text
              ( case state.mCommentSection of
                  Nothing ->
                    ""
                  Just cs ->
                    if cs.hasProblem then
                      if state.inLatest then
                        translate (label :: _ "comment_reanchor") state.translator
                      else
                        translate (label :: _ "comment_predated") state.translator
                    else
                      ""
              )
          ]
      , renderFirstComment state.translator state.mTimeFormatter state.mCurrentTime state.inLatest false
          DoNothing
          first
      ]
        <> map (renderComment state.translator state.mTimeFormatter state.mCurrentTime)
          commentSection.replies

  renderInput :: State -> forall slots. H.ComponentHTML Action slots m
  renderInput state =
    HH.div
      [ HP.classes [ H.ClassName "fpo-comment-input" ] ]
      ( renderModal <>
          [ HH.textarea
              [ HP.classes [ H.ClassName "fpo-comment-input__textarea" ]
              , HP.rows 3
              , HP.value state.commentDraft
              , HP.placeholder
                  (translate (label :: _ "comment_placeholder") state.translator)
              , HE.onValueChange UpdateDraft
              ]
          , HH.div
              [ HP.classes [ H.ClassName "fpo-comment-input__actions" ] ]
              [ -- Discard button (new comment, left side) — uses popover confirmation
                if state.newComment then
                  HH.div [ HP.style "position: relative;" ]
                    ( [ HH.button
                          [ HP.classes [ HB.btn, HB.btnSm, H.ClassName "btn-outline-danger" ]
                          , HP.style "white-space: nowrap;"
                          , HE.onClick \_ -> ShowDiscardPopover
                          ]
                          [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-x-lg" ], HP.style "margin-right: 0.35rem;" ] []
                          , HH.span [ HP.classes [ H.ClassName "fpo-btn-label" ] ]
                              [ HH.text
                                  (translate (label :: _ "comment_discard") state.translator)
                              ]
                          ]
                      ] <>
                        if state.showDiscardPopover then
                          [ HH.div [ HP.classes [ H.ClassName "fpo-popover" ] ]
                              [ HH.div [ HP.classes [ H.ClassName "fpo-popover__text" ] ]
                                  [ HH.text
                                      (translate (label :: _ "comment_discard_phrase") state.translator)
                                  ]
                              , HH.div [ HP.classes [ H.ClassName "fpo-popover__actions" ] ]
                                  [ HH.button
                                      [ HP.classes [ HB.btn, HB.btnSm, HB.btnSecondary ]
                                      , HE.onClick \_ -> HideDiscardPopover
                                      ]
                                      [ HH.text
                                          (translate (label :: _ "common_cancel") state.translator)
                                      ]
                                  , HH.button
                                      [ HP.classes [ HB.btn, HB.btnSm, HB.btnDanger ]
                                      , HE.onClick \_ -> DeleteComment
                                      ]
                                      [ HH.text
                                          (translate (label :: _ "comment_discard") state.translator)
                                      ]
                                  ]
                              ]
                          ]
                        else []
                    )
                else
                  HH.text ""

              -- Send button (right side)
              , HH.button
                  [ HP.classes [ HB.btn, HB.btnSm, HB.btnPrimary ]
                  , HP.style "white-space: nowrap; margin-left: auto;"
                  , HE.onClick \_ -> SendComment
                  ]
                  [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-send" ], HP.style "margin-right: 0.35rem;" ] []
                  , HH.span [ HP.classes [ H.ClassName "fpo-btn-label" ] ]
                      [ HH.text
                          (translate (label :: _ "comment_send") state.translator)
                      ]
                  ]
              ]
          ]
      )
    where
    renderModal = case state.requestModal of
      Nothing -> []
      Just Resolve -> []
      Just Delete -> []  -- Delete no longer uses modal, uses popover instead

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      now <- liftEffect nowDateTime
      H.modify_ _ { mCurrentTime = Just now }

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

    DoNothing -> do
      pure unit

    UpdateDraft draft -> do
      H.modify_ \state ->
        state { commentDraft = draft }

    SendComment -> do
      state <- H.get
      -- only send, if the the user has wrote something
      when (state.commentDraft /= "") do
        let pText = encodeJson { text: state.commentDraft }
        -- Is it the first comment sent?
        if state.newComment then do
          res <- Request.postJson CD.decodeSection
            ( "/docs/" <> show state.docID <> "/text/" <> show state.tocID <>
                "/comments"
            )
            pText
          case res of
            Left _ -> pure unit
            Right r -> do
              let
                cs = sectionDtoToCS r
                newCommentSections = snoc state.commentSections cs
              H.modify_ _
                { markerID = cs.markerID
                , commentSections = newCommentSections
                , mCommentSection = Just cs
                , newComment = false
                , commentDraft = ""
                }
              H.raise (ConfirmComment cs)
        else do
          res <- Request.postJson CD.decodeComment
            ( "/docs/" <> show state.docID <> "/text/" <> show state.tocID
                <> "/comments/"
                <> show state.markerID
                <> "/replies"
            )
            pText
          case res of
            Left _ -> pure unit
            Right com -> do
              -- update the current commentSection in the list of other commentSections
              case state.mCommentSection of
                Nothing -> pure unit
                Just cs -> do
                  let
                    com' = cdCommentToComment com
                    newCs = cs { replies = snoc cs.replies com' }
                    newCSs = updateCommentSection newCs state.commentSections
                  H.modify_ _
                    { commentSections = newCSs
                    , mCommentSection = Just newCs
                    , commentDraft = ""
                    }

    ResolveComment -> do
      state <- H.get
      _ <- Request.postIgnore
        ( "/docs/" <> show state.docID <> "/text/" <> show state.tocID
            <> "/comments/"
            <> show state.markerID
            <> "/resolve"
        )
        jsonEmptyObject
      case state.mCommentSection of
        -- should not happen
        Nothing -> pure unit
        Just cs -> do
          let
            newCS = updateFirstCommentProblem $ cs
              { resolved = true, hasProblem = false }
            newCSs = updateCommentSection newCS state.commentSections
            hasProblem = hasProblems newCSs
          H.modify_ _
            { commentSections = newCSs
            , mCommentSection = Just newCS
            , commentDraft = ""
            , requestModal = Nothing
            }
          -- Delete it from Editor
          H.raise (ToDeleteComment state.markerID hasProblem)
          H.raise (CommentOverview (map extractFirst newCSs))
          H.raise (ShowResolveButton false)

    DeleteComment -> do
      state <- H.get
      let
        -- Remove the current comment section from the list
        updatedSections = case state.mCommentSection of
          Nothing -> state.commentSections
          Just cs -> filter (\s -> s.markerID /= cs.markerID) state.commentSections
        hasProblem = hasProblems updatedSections
      H.modify_ _
        { markerID = -1
        , newComment = false
        , commentDraft = ""
        , requestModal = Nothing
        , showDiscardPopover = false
        , commentSections = updatedSections
        , mCommentSection = Nothing
        }
      H.raise (ToDeleteComment state.markerID hasProblem)
      H.raise (ShowResolveButton false)

    SelectingCommentSection markerID -> do
      if markerID == -360 then do
        H.modify_ _
          { markerID = -360
          , mCommentSection = Nothing
          , newComment = true
          }
        H.raise (SetReAnchor Nothing)
        H.raise (ShowResolveButton false)
      else do
        state <- H.get
        let commentSections = state.commentSections
        if (markerID /= state.markerID) then do
          case (find (\cs -> cs.markerID == markerID) commentSections) of
            Nothing -> pure unit
            Just section -> do
              H.modify_ _
                { markerID = markerID
                , mCommentSection = Just section
                }
              let reAnchor = if section.hasProblem then Just section else Nothing
              H.raise (SetReAnchor reAnchor)
              H.raise (ShowResolveButton (not section.resolved))
        else do
          let
            reAnchor =
              state.mCommentSection >>= \cs ->
                if cs.hasProblem then Just cs else Nothing
            showResolve = case state.mCommentSection of
              Just cs -> not cs.resolved
              Nothing -> false
          H.raise (SetReAnchor reAnchor)
          H.raise (ShowResolveButton showResolve)

    RequestModal mode -> do
      H.modify_ _ { requestModal = Just mode }

    CancelModal -> do
      H.modify_ _ { requestModal = Nothing }

    ShowDiscardPopover -> do
      H.modify_ _ { showDiscardPopover = true }

    HideDiscardPopover -> do
      H.modify_ _ { showDiscardPopover = false }


  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    -- add temporary comment
    AddComment a -> do
      H.modify_ _
        { markerID = -360
        , mCommentSection = Nothing
        , newComment = true
        }
      H.raise (ShowResolveButton false)
      pure (Just a)

    ReceiveTimeFormatter mTimeFormatter a -> do
      H.modify_ \state -> state { mTimeFormatter = mTimeFormatter }
      pure (Just a)

    RequestComments docID tocID markerIDs inLatest a -> do
      state <- H.get
      if (state.docID /= docID || tocID /= state.tocID) then do
        H.modify_ _ { commentSectionsLoaded = false }
        fetchedCommentSections <- fetchCommentSections docID tocID
        let
          commentSections = map
            (updateProblemStatus markerIDs)
            fetchedCommentSections
          hasProblem = hasProblems commentSections
          css = map updateFirstCommentProblem commentSections
          fs = map extractFirst css
        H.modify_ _
          { docID = docID
          , tocID = tocID
          , commentSections = css
          , commentSectionsLoaded = true
          , mCommentSection = Nothing
          , markerID = -1
          , inLatest = inLatest
          }
        H.raise (SendAbstractedComments fs hasProblem)
        H.raise (ShowResolveButton false)
      else do
        let
          commentSections = map
            (updateProblemStatus markerIDs)
            state.commentSections
          hasProblem = hasProblems commentSections
          css = map updateFirstCommentProblem commentSections
          fs = map extractFirst css
        H.modify_ _
          { commentSections = css
          , commentSectionsLoaded = true
          , mCommentSection = Nothing
          , markerID = -1
          , inLatest = inLatest
          }
        H.raise (SendAbstractedComments fs hasProblem)
        H.raise (ShowResolveButton false)
      pure (Just a)

    SelectedCommentSection markerID a -> do
      handleAction $ SelectingCommentSection markerID
      pure (Just a)

    Overview a -> do
      state <- H.get
      let
        css = state.commentSections
        fs = map extractFirst css
      H.raise (CommentOverview fs)
      pure (Just a)

    UpdateComment markerIDs a -> do
      state <- H.get
      let
        commentSections = map (updateProblemStatus markerIDs) state.commentSections
        hasProblem = hasProblems commentSections
        css = map updateFirstCommentProblem commentSections
        fs = map extractFirst css
      H.modify_ _
        { commentSections = css
        , mCommentSection = find (\sec -> sec.markerID == state.markerID) css
        }
      H.raise $ UpdatedComments fs hasProblem
      pure (Just a)

    ReaddedAnchor a -> do
      state <- H.get
      case state.mCommentSection of
        -- should not happen
        Nothing -> pure unit
        Just cs -> do
          let
            newCS = updateFirstCommentProblem $ cs
              { hasProblem = false }
            newCSs = updateCommentSection newCS state.commentSections
            hasProblem = hasProblems newCSs
          H.modify_ _
            { commentSections = newCSs
            , mCommentSection = Just newCS
            }
          H.raise (UpdatedComments (map extractFirst newCSs) hasProblem)
      pure (Just a)

    UpdateCommentProblem markerID a -> do
      state <- H.get
      if state.commentSectionsLoaded then do
        let
          newCS = markProblemForMarker markerID state.commentSections
          css = map updateFirstCommentProblem newCS
          fs = map extractFirst css
          hasProblem = hasProblems css
          mUpdated = find (\sec -> sec.markerID == markerID) css
          mCS = case (find (\sec -> sec.markerID == state.markerID) css) of
            Just cs -> Just cs
            Nothing -> state.mCommentSection
        H.modify_ _ { commentSections = css, mCommentSection = mCS }
        H.raise (UpdatedComments fs hasProblem)
        when (markerID == state.markerID) do
          case mUpdated of
            Just cs ->
              H.raise (SetReAnchor (if cs.hasProblem then Just cs else Nothing))
            Nothing -> pure unit
        pure $ Just a
      else
        pure (Just a)

    RequestResolve a -> do
      handleAction ResolveComment
      pure (Just a)

  -- Retrieves the comment sections for a given document ID and TOC ID. If
  -- the request fails, an empty array is returned.
  fetchCommentSections
    :: forall slots
     . Int
    -> Int
    -> H.HalogenM State Action slots Output m (Array CommentSection)
  fetchCommentSections docID tocID = do
    cs <- Request.getCommentSections docID tocID
    case cs of
      Left _ -> pure []
      Right cms ->
        pure $ map sectionDtoToCS $ CD.getCommentSections cms

  updateCommentSection
    :: CommentSection -> Array CommentSection -> Array CommentSection
  updateCommentSection cs = map \c ->
    if c.markerID == cs.markerID then cs else c

  updateProblemStatus :: Array Int -> CommentSection -> CommentSection
  updateProblemStatus markerIDs cs =
    cs
      { hasProblem = (not cs.resolved) &&
          (not (any (\id -> cs.markerID == id) markerIDs))
      }

  markProblemForMarker :: Int -> Array CommentSection -> Array CommentSection
  markProblemForMarker markerID css =
    let
      f cs = if cs.resolved then cs else cs { hasProblem = true }
    in
      case findIndex (\cs -> cs.markerID == markerID) css of
        Nothing -> css
        Just ix -> modifyAt ix f css # maybe css identity
