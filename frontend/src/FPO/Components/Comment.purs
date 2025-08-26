module FPO.Components.Comment where

import Prelude

import Data.Array (cons, elem, find, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Request (getUser)
import FPO.Data.Store as Store
import FPO.Dto.UserDto (getUserName)
import FPO.Dto.CommentDto as CD
import FPO.Types (Comment, CommentSection, cdCommentToComment, sectionDtoToCS)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB
import FPO.Data.Request as Request
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

type Input = Unit

data Output
  = CloseCommentSection
  | UpdateComment CommentSection

data Action
  = Init
  | UpdateDraft String
  | SendComment

data Query a
  = AddComment Int Int a
  | DeletedComment (Array Int) a
  | ReceiveTimeFormatter (Maybe Formatter) a
  | SelectedCommentSection Int Int Int a

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
          [ renderInput state.commentDraft ]
      else
        HH.text ""
    Just commentSection ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( renderComments state.mTimeFormatter commentSection
            <> [ renderInput state.commentDraft ]
        )

  renderComments
    :: Maybe Formatter
    -> CommentSection
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  renderComments mFormatter commentSection = case commentSection.first of
    Nothing -> [ HH.text "" ]
    Just cs ->
      [ renderFirstComment mFormatter cs ]
        <> map (renderComment mFormatter) commentSection.replies

  renderFirstComment
    :: Maybe Formatter -> Comment -> forall slots. H.ComponentHTML Action slots m
  renderFirstComment mFormatter c =
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
      , HP.style "background-color:rgba(246, 250, 0, 0.9);"
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

  renderInput :: String -> forall slots. H.ComponentHTML Action slots m
  renderInput draft =
    HH.div [ HP.style "flex flex-col space-y-2" ]
      [ HH.textarea
          [ HP.style "border rounded-md p-2 resize-none"
          , HP.rows 3
          , HP.value draft
          , HE.onValueChange UpdateDraft
          ]
      , HH.button
          [ HP.style "bg-blue-600 text-white px-3 py-1 rounded hover:bg-blue-700"
          , HE.onClick \_ -> SendComment
          ]
          [ HH.text "Senden" ]
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
      when (state.commentDraft /= "") do
        let pText = encodeJson { text: state.commentDraft }
        if state.newComment then do
          res <- Request.postJson CD.decodeSection
            ("/docs/" <> show state.docID <> "/text/" <> show state.tocID <> "/comments")
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
                , commentDraft = ""}
              H.raise (UpdateComment cs)
        else do
          res <- Request.postJson CD.decodeComment 
            ("/docs/" <> show state.docID <> "/text/" <> show state.tocID <> "/comments/" <> show state.markerID <> "/replies")
            pText
          case res of 
            Left _ -> pure unit
            Right com -> do
              case state.mCommentSection of
                Nothing -> pure unit
                Just cs -> do
                  let 
                    com' = cdCommentToComment com
                    newCs = cs { replies = snoc cs.replies com'}
                    newCSs = updateCommentSection newCs state.commentSections
                  H.modify_ _ {commentSections = newCSs, mCommentSection = Just newCs, commentDraft = ""}

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    AddComment docID tocID a -> do
      state <- H.get
      -- let newCommentSections = snoc state.commentSections newComment
      H.modify_ \st -> st 
        { docID = docID
        , tocID = tocID
        , markerID = -360
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

    SelectedCommentSection docID tocID markerID a -> do
      state <- H.get
      when (state.docID /= docID || tocID /= state.tocID) do
        recComs <- H.liftAff
          $ Request.getFromJSONEndpoint CD.decodeCommentSection
          $ "/docs/" <> show docID <> "/text/" <> show tocID <> "/comments"
        let 
          commentSections = case recComs of 
            Nothing -> []
            Just cms -> map sectionDtoToCS $ CD.getCommentSections cms
        H.modify_ \st -> st { docID = docID, commentSections = commentSections }

      when ( markerID /= state.markerID) do
        let commentSections = state.commentSections
        case (find (\cs -> cs.markerID == markerID) commentSections) of
          Nothing -> pure unit
          Just section -> do
            H.modify_ \st -> st
              { markerID = markerID
              , mCommentSection = Just section
              }
      pure (Just a)

  updateCommentSection :: CommentSection -> Array CommentSection -> Array CommentSection
  updateCommentSection cs = map \c ->
    if c.markerID == cs.markerID then cs else c

