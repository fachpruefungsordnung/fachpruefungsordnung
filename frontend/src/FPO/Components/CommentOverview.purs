module FPO.Components.CommentOverview where

import Prelude

import Data.Array (head, mapMaybe)
import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Types (Comment, CommentSection, TOCEntry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

-- DeleteComment later
data Output = JumpToCommentSection Int Int CommentSection

data Action
  = Init
  | SelectCommentSection Int Int CommentSection

data Query a
  = ReceiveTimeFormatter (Maybe Formatter) a
  | ReceiveTOC TOCEntry a

type State =
  { mTocEntry :: Maybe TOCEntry
  , mTimeFormatter :: Maybe Formatter
  }

commentOverviewview :: forall m. MonadAff m => H.Component Query Input Output m
commentOverviewview = H.mkComponent
  { initialState: \_ -> { mTocEntry: Nothing, mTimeFormatter: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state = case state.mTocEntry of
    Nothing ->
      HH.div [ HP.style "padding: 1rem;" ]
        [ HH.text "No comments in this section." ]
    Just tocEntry ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( mapMaybe
            ( \m -> case m.mCommentSection of
                Nothing -> Nothing
                Just cs -> case head cs.comments of
                  Nothing -> Nothing
                  Just c -> Just
                    (renderFirstComment state.mTimeFormatter c tocEntry.id m.id cs)
            )
            [] --tocEntry.markers
        )

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

    SelectCommentSection tocID markerID commentSection -> do
      H.raise (JumpToCommentSection tocID markerID commentSection)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTimeFormatter mTimeFormatter a -> do
      H.modify_ \state -> state { mTimeFormatter = mTimeFormatter }
      pure (Just a)

    ReceiveTOC entry a -> do
      H.modify_ \state -> state { mTocEntry = Just entry }
      pure (Just a)

  renderFirstComment
    :: Maybe Formatter
    -> Comment
    -> Int
    -> Int
    -> CommentSection
    -> forall slots
     . H.ComponentHTML Action slots m
  renderFirstComment mFormatter c tocID markerID cs =
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
      , HE.onClick \_ -> SelectCommentSection tocID markerID cs
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