module FPO.Components.Comment where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, find, snoc)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.CommentDto as CD
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types
  ( Comment
  , CommentSection
  , FirstComment
  , cdCommentToComment
  , emptyComment
  , sectionDtoToCS
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
  | UpdateComment CommentSection
  | CommentOverview Int (Array FirstComment)
  | SendAbstractedComments (Array FirstComment)
  | ToDeleteComment

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
  = AddComment Int Int a
  | DeletedComment (Array Int) a
  | ReceiveTimeFormatter (Maybe Formatter) a
  | RequestComments Int Int a
  | SelectedCommentSection Int Int Int a
  | Overview Int Int a

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
    Just c ->
      [ renderFirstComment state c commentSection.resolved ]
        <> map (renderComment state) commentSection.replies

  renderFirstComment
    :: State
    -> Comment
    -> Boolean
    -> forall slots
     . H.ComponentHTML Action slots m
  renderFirstComment state c resolved =
    HH.div
      [ HP.classes
          [ HB.p2
          , HB.mb2
          , HB.border
          , HB.rounded
          , HB.shadowSm
          , HB.dFlex
          , HB.flexColumn
          ]
      , HP.style $ "background-color:"
          <> (if resolved then "rgba(66, 250, 0, 0.9)" else "rgba(246, 250, 0, 0.9)")
          <> ";"
      ]
      [ HH.div_
          [ HH.div
              [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
              , HP.style "font-weight: 500; font-size: 1rem;"
              ]
              ( [ HH.span_ [ HH.text c.author ] ]
                  <>
                    if resolved then
                      [ HH.i
                          [ HP.classes
                              [ HB.bi
                              , H.ClassName "bi-check-circle-fill"
                              , HB.msAuto
                              , H.ClassName "fs-4"
                              ]
                          ]
                          []
                      ]
                    else []
              )
          , HH.div
              [ HP.classes [ HB.mt1 ]
              , HP.style "font-size: 1rem;"
              ]
              [ HH.text c.content ]
          ]
      , HH.div
          [ HP.classes [ HB.mt2 ]
          , HP.style "align-self: flex-end; font-size: 0.75rem; color: #555;"
          ]
          [ HH.text $ maybe
              (translate (label :: _ "comment_no_timestamp") state.translator)
              (\formatter -> format formatter c.timestamp)
              state.mTimeFormatter
          ]
      ]

  renderComment
    :: State -> Comment -> forall slots. H.ComponentHTML Action slots m
  renderComment state c =
    HH.div
      [ HP.classes
          [ HB.p1
          , HB.mb1
          , HB.mx2
          , HB.border
          , HB.rounded
          , HB.shadowSm
          , HB.dFlex
          , HB.flexColumn
          ]
      , HP.style "background-color: #fff9c4;"
      ]
      [ HH.div_
          [ HH.div
              [ HP.style "font-weight: 500; font-size: 1rem;" ]
              [ HH.text c.author ]
          , HH.div
              [ HP.classes [ HB.mt1 ]
              , HP.style "font-size: 0.875rem;"
              ]
              [ HH.text c.content ]
          ]
      , HH.div
          [ HP.classes [ HB.mt2 ]
          , HP.style "align-self: flex-end; font-size: 0.75rem; color: #555;"
          ]
          [ HH.text $ maybe
              (translate (label :: _ "comment_no_timestamp") state.translator)
              (\formatter -> format formatter c.timestamp)
              state.mTimeFormatter
          ]
      ]

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
              H.raise (UpdateComment cs)
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
            newCs = cs { resolved = true }
            newCSs = updateCommentSection newCs state.commentSections
          H.modify_ _
            { commentSections = newCSs
            , mCommentSection = Just newCs
            , commentDraft = ""
            , requestModal = Nothing
            }
          -- Delete it from Editor
          H.raise (ToDeleteComment)

    DeleteComment -> do
      H.modify_ _
        { markerID = -1
        , newComment = false
        , commentDraft = ""
        , requestModal = Nothing
        }
      H.raise (ToDeleteComment)

    SelectingCommentSection markerID -> do
      if markerID == -360 then do
        H.modify_ _
          { markerID = -360
          , mCommentSection = Nothing
          , newComment = true
          }
      else do
        state <- H.get
        let commentSections = state.commentSections
        when (markerID /= state.markerID) do
          case (find (\cs -> cs.markerID == markerID) commentSections) of
            Nothing -> pure unit
            Just section -> do
              H.modify_ _
                { markerID = markerID
                , mCommentSection = Just section
                }

    RequestModal mode -> do
      H.modify_ _ { requestModal = Just mode }

    CancelModal -> do
      H.modify_ _ { requestModal = Nothing }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    AddComment docID tocID a -> do
      -- load comments, when section was changed
      state <- H.get
      when (state.docID /= docID || tocID /= state.tocID) do
        recComs <- Request.getCommentSections docID tocID
        let
          commentSections = case recComs of
            Left _ -> []
            Right cms -> map sectionDtoToCS $ CD.getCommentSections cms
        H.modify_ _
          { docID = docID
          , tocID = tocID
          , commentSections = commentSections
          }
      -- Always set this
      H.modify_ _
        { markerID = -360
        , mCommentSection = Nothing
        , newComment = true
        }
      pure (Just a)

    DeletedComment deletedIDs a -> do
      state <- H.get
      when (elem state.markerID deletedIDs) $
        H.raise CloseCommentSection
      pure (Just a)

    ReceiveTimeFormatter mTimeFormatter a -> do
      H.modify_ \state -> state { mTimeFormatter = mTimeFormatter }
      pure (Just a)

    RequestComments docID tocID a -> do
      state <- H.get
      if (state.docID /= docID || tocID /= state.tocID) then do
        commentSections <- fetchCommentSections docID tocID
        H.modify_ _
          { docID = docID, tocID = tocID, commentSections = commentSections }
        let cs = map extractFirst commentSections
        H.raise (SendAbstractedComments cs)
      else do
        let
          css = state.commentSections
          cs = map extractFirst css
        H.raise (SendAbstractedComments cs)
      pure (Just a)

    SelectedCommentSection docID tocID markerID a -> do
      state <- H.get
      if (state.docID /= docID || tocID /= state.tocID) then do
        commentSections <- fetchCommentSections docID tocID
        H.modify_ _
          { docID = docID, tocID = tocID, commentSections = commentSections }
        handleAction $ SelectingCommentSection markerID
      else do
        handleAction $ SelectingCommentSection markerID
      pure (Just a)

    Overview docID tocID a -> do
      state <- H.get
      if (state.docID /= docID || tocID /= state.tocID) then do
        commentSections <- fetchCommentSections docID tocID
        H.modify_ _
          { docID = docID, tocID = tocID, commentSections = commentSections }
        let cs = map extractFirst commentSections
        H.raise (CommentOverview state.tocID cs)
      else do
        let
          css = state.commentSections
          cs = map extractFirst css
        H.raise (CommentOverview state.tocID cs)
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

  extractFirst :: CommentSection -> FirstComment
  extractFirst { markerID, resolved, first } = case first of
    Nothing -> { markerID: -1, resolved: true, first: emptyComment }
    Just c -> { markerID, resolved, first: c }

