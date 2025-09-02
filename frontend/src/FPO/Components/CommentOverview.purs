module FPO.Components.CommentOverview where

import Prelude

import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Types (FirstComment)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

-- DeleteComment later
data Output = JumpToCommentSection Int Int

data Action
  = Init
  | SelectCommentSection Int Int

data Query a
  = ReceiveTimeFormatter (Maybe Formatter) a
  | ReceiveComments Int (Array FirstComment) a

type State =
  { tocID :: Int
  , comments :: Array FirstComment
  , mTimeFormatter :: Maybe Formatter
  }

commentOverviewview :: forall m. MonadAff m => H.Component Query Input Output m
commentOverviewview = H.mkComponent
  { initialState: \_ -> { tocID: -1, comments: [], mTimeFormatter: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state = case state.comments of
    [] ->
      HH.div [ HP.style "padding: 1rem;" ]
        [ HH.text "No comments in this section." ]
    _ ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( map
            ( \first ->
                (renderFirstComment state.mTimeFormatter first state.tocID)
            )
            state.comments
        )

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

    SelectCommentSection tocID markerID -> do
      H.raise (JumpToCommentSection tocID markerID)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTimeFormatter mTimeFormatter a -> do
      H.modify_ \state -> state { mTimeFormatter = mTimeFormatter }
      pure (Just a)

    ReceiveComments tocID cs a -> do
      H.modify_ \state -> state { tocID = tocID, comments = cs }
      pure (Just a)

  renderFirstComment
    :: Maybe Formatter
    -> FirstComment
    -> Int
    -> forall slots
     . H.ComponentHTML Action slots m
  renderFirstComment mFormatter c tocID =
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
          <>
            (if c.resolved then "rgba(66, 250, 0, 0.9)" else "rgba(246, 250, 0, 0.9)")
          <> ";"
      , HE.onClick \_ -> SelectCommentSection tocID c.markerID
      ]
      [ HH.div_
          [ HH.div
              [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
              , HP.style "font-weight: 500; font-size: 1rem;"
              ]
              [ HH.span_ [ HH.text c.first.author ]
              , HH.i [ HP.classes [ HB.bi
                                  , H.ClassName "bi-check-circle-fill"
                                  , HB.msAuto
                                  , H.ClassName "fs-4"
                                  ]
                    ] []
              ]
          , HH.div
              [ HP.classes [ HB.mt1 ]
              , HP.style "font-size: 1rem;"
              ]
              [ HH.text c.first.content ]
          ]
      , HH.div
          [ HP.classes [ HB.mt2 ]
          , HP.style "align-self: flex-end; font-size: 0.75rem; color: #555;"
          ]
          [ HH.text $ maybe "No timestamp found."
              (\formatter -> format formatter c.first.timestamp)
              mFormatter
          ]
      ]