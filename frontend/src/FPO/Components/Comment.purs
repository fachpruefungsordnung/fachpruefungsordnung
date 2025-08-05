module FPO.Components.Comment where

import Prelude

import Data.Array (elem, snoc, uncons)
import Data.Foldable (for_)
import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDateTime)
import FPO.Data.Request (getUser)
import FPO.Dto.UserDto (getUserName)
import FPO.Types (Comment, CommentSection)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output
  = CloseCommentSection
  | UpdateComment Int Int CommentSection

data Action
  = Init
  | UpdateDraft String
  | SendComment

data Query a
  = DeletedComment Int (Array Int) a
  | ReceiveTimeFormatter (Maybe Formatter) a
  | SelectedCommentSection Int Int CommentSection a

type State =
  { tocID :: Int
  , markerID :: Int
  , mCommentSection :: Maybe CommentSection
  , commentDraft :: String
  , mTimeFormatter :: Maybe Formatter
  }

commentview :: forall m. MonadAff m => H.Component Query Input Output m
commentview = H.mkComponent
  { initialState: \_ ->
      { tocID: -1
      , markerID: -1
      , mCommentSection: Nothing
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
    Nothing -> HH.text ""
    Just commentSection ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( renderComments state.mTimeFormatter commentSection.comments
            <> [ renderInput state.commentDraft ]
        )

  renderComments
    :: Maybe Formatter
    -> Array Comment
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  renderComments mFormatter comments = case uncons comments of
    Nothing -> [ HH.text "" ]
    Just { head: c, tail: cs } ->
      [ renderFirstComment mFormatter c ]
        <> map (renderComment mFormatter) cs

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
      when (state.commentDraft /= "") $
        for_ state.mCommentSection \commentSection -> do
          now <- H.liftEffect nowDateTime
          user <- H.liftAff getUser
          let
            author = maybe "Guest" getUserName user
            comments = commentSection.comments
            newComment =
              { author: author, timestamp: now, content: state.commentDraft }
            newCommentSection = commentSection
              { comments = snoc comments newComment }
          H.modify_ \st -> st
            { mCommentSection = Just newCommentSection, commentDraft = "" }
          H.raise (UpdateComment state.tocID state.markerID newCommentSection)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    DeletedComment changedTocID deletedIDs a -> do
      state <- H.get
      when (changedTocID == state.tocID && elem state.markerID deletedIDs) $
        H.raise CloseCommentSection
      pure (Just a)

    ReceiveTimeFormatter mTimeFormatter a -> do
      H.modify_ \state -> state { mTimeFormatter = mTimeFormatter }
      pure (Just a)

    SelectedCommentSection tocID markerID section a -> do
      H.modify_ \state -> state
        { tocID = tocID
        , markerID = markerID
        , mCommentSection = Just section
        }
      pure (Just a)

