module FPO.Components.Comment where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (filter, find, snoc)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Formatter.DateTime (Formatter)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
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
import FPO.UI.HTML (addModal)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (label, translate)

type Input = Unit

data Output
  = CloseCommentSection
  | ConfirmComment CommentSection
  | CommentOverview (Array FirstComment)
  | SendAbstractedComments (Array FirstComment) Boolean
  | ToDeleteComment Boolean
  | UpdatedComments (Array FirstComment) Boolean
  | SetReAnchor (Maybe CommentSection)

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

data Query a
  = AddComment a
  | ReceiveTimeFormatter (Maybe Formatter) a
  | RequestComments Int Int (Array Int) Boolean a
  | SelectedCommentSection Int a
  | Overview a
  | UpdateComment (Array Int) a
  | ReaddedAnchor a

type State = FPOState
  ( docID :: Int
  , tocID :: Int
  , markerID :: Int
  , commentSections :: Array CommentSection
  , mCommentSection :: Maybe CommentSection
  , newComment :: Boolean
  , commentDraft :: String
  , mTimeFormatter :: Maybe Formatter
  , requestModal :: Maybe Mode
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
      , mCommentSection: Nothing
      , newComment: false
      , commentDraft: ""
      , mTimeFormatter: Nothing
      , requestModal: Nothing
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
        HH.div [ HP.style "comment-section space-y-3" ]
          [ renderInput state ]
      else
        HH.text ""
    Just commentSection ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( renderComments state commentSection
            <>
              ( if commentSection.resolved then []
                else [ renderInput state ]
              )
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
      , renderFirstComment state.translator state.mTimeFormatter state.inLatest false
          DoNothing
          first
      ]
        <> map (renderComment state.translator state.mTimeFormatter)
          commentSection.replies

  renderInput :: State -> forall slots. H.ComponentHTML Action slots m
  renderInput state =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn ]
      , HP.style
          "gap: .5rem; padding-left: 0.5rem; padding-right: 0.5rem; padding-top: 0.75rem; padding-bottom: 1rem;"
      ]
      ( renderModal <>
          [ HH.textarea
              [ HP.style
                  "border-radius: .375rem; padding: .5rem; resize: none; min-height: 5rem;"
              , HP.rows 3
              , HP.value state.commentDraft
              , HE.onValueChange UpdateDraft
              ]
          , HH.div
              [ HP.classes [ HB.dFlex, HB.alignItemsCenter, HB.w100 ]
              , HP.style "gap: .5rem; padding-left: 0.5rem; padding-right: 0.5rem;"
              ]
              [ -- Senden (links)
                HH.button
                  [ HP.classes [ HB.btn, HB.btnPrimary, HB.px3, HB.py1, HB.m0 ]
                  , HP.style "white-space: nowrap;"
                  , HE.onClick \_ -> SendComment
                  ]
                  [ HH.small [ HP.style "margin-right: 0.25rem;" ]
                      [ HH.text
                          (translate (label :: _ "comment_send") state.translator)
                      ]
                  , HH.i [ HP.classes [ HB.bi, H.ClassName "bi-send" ] ] []
                  ]

              -- Resolve (rechts)
              , if state.newComment then
                  HH.button
                    [ HP.classes
                        [ HB.btn, HB.btnDanger, HB.px3, HB.py1, HB.m0, HB.msAuto ]
                    , HP.style "white-space: nowrap;"
                    , HE.onClick \_ -> RequestModal Delete -- DeleteComment
                    ]
                    [ HH.small [ HP.style "margin-right: 0.25rem;" ]
                        [ HH.text
                            (translate (label :: _ "comment_delete") state.translator)
                        ]
                    , HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check2-circle" ] ] []
                    ]
                else
                  HH.button
                    [ HP.classes
                        [ HB.btn, HB.btnSuccess, HB.px3, HB.py1, HB.m0, HB.msAuto ]
                    , HP.style "white-space: nowrap;"
                    , HE.onClick \_ -> RequestModal Resolve --ResolveComment
                    ]
                    [ HH.small [ HP.style "margin-right: 0.25rem;" ]
                        [ HH.text
                            ( translate (label :: _ "comment_resolve")
                                state.translator
                            )
                        ]
                    , HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check2-circle" ] ] []
                    ]
              ]
          ]
      )
    where
    renderModal = case state.requestModal of
      Nothing -> []
      Just mode ->
        let
          { titel, phrase, confirmButton, action } =
            case mode of
              Delete ->
                { titel: translate (label :: _ "comment_modal_delete_titel")
                    state.translator
                , phrase: translate (label :: _ "comment_delete_phrase")
                    state.translator
                , confirmButton:
                    (translate (label :: _ "common_delete") state.translator)
                , action: DeleteComment
                }
              Resolve ->
                { titel: translate (label :: _ "comment_modal_resolve_titel")
                    state.translator
                , phrase: translate (label :: _ "comment_resolve_phrase")
                    state.translator
                , confirmButton:
                    (translate (label :: _ "common_resolve") state.translator)
                , action: ResolveComment
                }
        in
          [ addModal
              titel
              CancelModal
              DoNothing
              [ HH.div
                  [ HP.classes [ HB.modalBody ] ]
                  [ HH.text phrase ]
              , HH.div
                  [ HP.classes [ HB.modalFooter ] ]
                  [ HH.button
                      [ HP.type_ HP.ButtonButton
                      , HP.classes [ HB.btn, HB.btnSecondary ]
                      , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                      , HE.onClick (const CancelModal)
                      ]
                      [ HH.text
                          (translate (label :: _ "common_cancel") state.translator)
                      ]
                  , HH.button
                      [ HP.type_ HP.ButtonButton
                      , HP.classes
                          [ HB.btn
                          , if mode == Delete then HB.btnDanger else HB.btnSuccess
                          ]
                      , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                      , HE.onClick (const action)
                      ]
                      [ HH.text confirmButton ]
                  ]
              ]
          ]

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

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
          H.raise (ToDeleteComment hasProblem)
          H.raise (CommentOverview (map extractFirst newCSs))

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
        , commentSections = updatedSections
        , mCommentSection = Nothing
        }
      H.raise (ToDeleteComment hasProblem)

    SelectingCommentSection markerID -> do
      if markerID == -360 then do
        H.modify_ _
          { markerID = -360
          , mCommentSection = Nothing
          , newComment = true
          }
        H.raise (SetReAnchor Nothing)
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
        else do
          let
            reAnchor =
              state.mCommentSection >>= \cs ->
                if cs.hasProblem then Just cs else Nothing
          H.raise (SetReAnchor reAnchor)

    RequestModal mode -> do
      H.modify_ _ { requestModal = Just mode }

    CancelModal -> do
      H.modify_ _ { requestModal = Nothing }

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
      pure (Just a)

    ReceiveTimeFormatter mTimeFormatter a -> do
      H.modify_ \state -> state { mTimeFormatter = mTimeFormatter }
      pure (Just a)

    RequestComments docID tocID markerIDs inLatest a -> do
      state <- H.get
      if (state.docID /= docID || tocID /= state.tocID) then do
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
          , mCommentSection = Nothing
          , markerID = -1
          , inLatest = inLatest
          }
        H.raise (SendAbstractedComments fs hasProblem)
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
          , mCommentSection = Nothing
          , markerID = -1
          , inLatest = inLatest
          }
        H.raise (SendAbstractedComments fs hasProblem)
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

