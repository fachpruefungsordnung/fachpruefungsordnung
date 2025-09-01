module FPO.Components.Comment where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, find, snoc)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.CommentDto as CD
import FPO.Types
  ( Comment
  , CommentSection
  , FirstComment
  , cdCommentToComment
  , emptyComment
  , sectionDtoToCS
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output
  = CloseCommentSection
  | UpdateComment CommentSection
  | CommentOverview Int (Array FirstComment)
  | SendAbstractedComments (Array FirstComment)
  | ToDeleteComment

data Action
  = Init
  | UpdateDraft String
  | SendComment
  | ResolveComment
  | DeleteComment
  | SelectingCommentSection Int

data Query a
  = AddComment Int Int a
  | DeletedComment (Array Int) a
  | ReceiveTimeFormatter (Maybe Formatter) a
  | RequestComments Int Int a
  | SelectedCommentSection Int Int Int a
  | Overview Int Int a

type State =
  { docID :: Int
  , tocID :: Int
  , markerID :: Int
  , commentSections :: Array CommentSection
  , mCommentSection :: Maybe CommentSection
  , newComment :: Boolean
  , commentDraft :: String
  , mTimeFormatter :: Maybe Formatter
  }

commentview
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component Query Input Output m
commentview = H.mkComponent
  { initialState: \_ ->
      { docID: -1
      , tocID: -1
      , markerID: -1
      , commentSections: []
      , mCommentSection: Nothing
      , newComment: false
      , commentDraft: ""
      , mTimeFormatter: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state = case state.mCommentSection of
    Nothing ->
      if state.newComment then
        HH.div [ HP.style "comment-section space-y-3" ]
          [ renderInput state.commentDraft state.newComment ]
      else
        HH.text ""
    Just commentSection ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( renderComments state.mTimeFormatter commentSection
            <>
              ( if commentSection.resolved then []
                else [ renderInput state.commentDraft state.newComment ]
              )
        )

  renderComments
    :: Maybe Formatter
    -> CommentSection
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  renderComments mFormatter commentSection = case commentSection.first of
    Nothing -> [ HH.text "" ]
    Just c ->
      [ renderFirstComment mFormatter c commentSection.resolved ]
        <> map (renderComment mFormatter) commentSection.replies

  renderFirstComment
    :: Maybe Formatter
    -> Comment
    -> Boolean
    -> forall slots
     . H.ComponentHTML Action slots m
  renderFirstComment mFormatter c resolved =
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
              [ HP.style "font-weight: 600; font-size: 1.2rem;" ]
              [ HH.text c.author ]
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
          [ HH.text $ maybe "No timestamp found."
              (\formatter -> format formatter c.timestamp)
              mFormatter
          ]
      ]

  renderComment
    :: Maybe Formatter -> Comment -> forall slots. H.ComponentHTML Action slots m
  renderComment mFormatter c =
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
          [ HH.text $ maybe "No timestamp found."
              (\formatter -> format formatter c.timestamp)
              mFormatter
          ]
      ]

  renderInput :: String -> Boolean -> forall slots. H.ComponentHTML Action slots m
  renderInput draft newComment =
    HH.div
      [ HP.classes [ HB.dFlex, HB.flexColumn ]
      , HP.style
          "gap: .5rem; padding-left: 0.5rem; padding-right: 0.5rem; padding-top: 0.75rem; padding-bottom: 1rem;"
      ]
      [ HH.textarea
          [ HP.style
              "border-radius: .375rem; padding: .5rem; resize: none; min-height: 5rem;"
          , HP.rows 3
          , HP.value draft
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
              [ HH.small [ HP.style "margin-right: 0.25rem;" ] [ HH.text "Senden" ]
              , HH.i [ HP.classes [ HB.bi, H.ClassName "bi-send" ] ] []
              ]

          -- Resolve (rechts)
          , if newComment then
              HH.button
                [ HP.classes
                    [ HB.btn, HB.btnDanger, HB.px3, HB.py1, HB.m0, HB.msAuto ]
                , HP.style "white-space: nowrap;"
                , HE.onClick \_ -> DeleteComment
                ]
                [ HH.small [ HP.style "margin-right: 0.25rem;" ] [ HH.text "Delete" ]
                , HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check2-circle" ] ] []
                ]
            else
              HH.button
                [ HP.classes
                    [ HB.btn, HB.btnSuccess, HB.px3, HB.py1, HB.m0, HB.msAuto ]
                , HP.style "white-space: nowrap;"
                , HE.onClick \_ -> ResolveComment
                ]
                [ HH.small [ HP.style "margin-right: 0.25rem;" ] [ HH.text "Resolve" ]
                , HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check2-circle" ] ] []
                ]
          ]
      ]

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
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
            }
          -- Delete it from Editor
          H.raise (ToDeleteComment)

    DeleteComment -> do
      H.modify_ _
        { markerID = -1
        , newComment = false
        , commentDraft = ""
        }
      H.raise (ToDeleteComment)

    SelectingCommentSection markerID -> do
      if markerID == -360 then do
        H.modify_ \st -> st
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
              H.modify_ \st -> st
                { markerID = markerID
                , mCommentSection = Just section
                }

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    AddComment docID tocID a -> do
      -- load comments, when section was changed
      state <- H.get
      when (state.docID /= docID || tocID /= state.tocID) do
        recComs <- H.liftAff
          $ Request.getFromJSONEndpoint CD.decodeCommentSection
          $ "/docs/" <> show docID <> "/text/" <> show tocID <> "/comments"
        let
          commentSections = case recComs of
            Nothing -> []
            Just cms -> map sectionDtoToCS $ CD.getCommentSections cms
        H.modify_ \st -> st
          { docID = docID
          , tocID = tocID
          , commentSections = commentSections
          }
      -- Always set this
      H.modify_ \st -> st
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
        commentSections <- H.liftAff $ fetchCommentSections docID tocID
        H.modify_ \st -> st
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
        commentSections <- H.liftAff $ fetchCommentSections docID tocID
        H.modify_ \st -> st
          { docID = docID, tocID = tocID, commentSections = commentSections }
        handleAction $ SelectingCommentSection markerID
      else do
        handleAction $ SelectingCommentSection markerID
      pure (Just a)

    Overview docID tocID a -> do
      state <- H.get
      if (state.docID /= docID || tocID /= state.tocID) then do
        commentSections <- H.liftAff $ fetchCommentSections docID tocID
        H.modify_ \st -> st
          { docID = docID, tocID = tocID, commentSections = commentSections }
        let cs = map extractFirst commentSections
        H.raise (CommentOverview state.tocID cs)
      else do
        let
          css = state.commentSections
          cs = map extractFirst css
        H.raise (CommentOverview state.tocID cs)
      pure (Just a)

  fetchCommentSections :: Int -> Int -> Aff (Array CommentSection)
  fetchCommentSections docID tocID =
    (maybe [] (map sectionDtoToCS <<< CD.getCommentSections))
      <$> Request.getFromJSONEndpoint CD.decodeCommentSection url
    where
    url = "/docs/" <> show docID <> "/text/" <> show tocID <> "/comments"

  updateCommentSection
    :: CommentSection -> Array CommentSection -> Array CommentSection
  updateCommentSection cs = map \c ->
    if c.markerID == cs.markerID then cs else c

  extractFirst :: CommentSection -> FirstComment
  extractFirst { markerID, resolved, first } = case first of
    Nothing -> { markerID: -1, resolved: true, first: emptyComment }
    Just c -> { markerID, resolved, first: c }

