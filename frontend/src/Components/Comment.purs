module FPO.Components.Comment where

import Prelude

import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Types (Comment, CommentSection)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output = UpdateComment Int Int CommentSection

data Action
  = Init
  | UpdateDraft String
  | SendComment

data Query a 
  = ReceiveTimeFormatter (Maybe Formatter) a
  | SelectedCommentSection Int Int CommentSection a

type State =
  { tocID :: Int
  , markerID :: Int
  , commentsection :: Maybe CommentSection
  , commentDraft :: String
  , timeFormatter :: Maybe Formatter
  }

commentview :: forall m. MonadAff m => H.Component Query Input Output m
commentview = H.mkComponent
  { initialState: \_ -> 
  { tocID: -1
  , markerID: -1
  , commentsection: Nothing
  , commentDraft: "" 
  , timeFormatter: Nothing
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
  render state = case state.commentsection of
    Nothing -> HH.text ""  
    Just commentSection ->
      HH.div [ HP.style "comment-section space-y-3" ]
        ( map (renderComment state.timeFormatter) commentSection.comments 
            <> [ renderInput state.commentDraft ]
        )
  
  renderComment :: Maybe Formatter -> Comment -> forall slots. H.ComponentHTML Action slots m
  renderComment mFormatter c =
    HH.div
      [ HP.classes
          [ HB.p3
          , HB.mb3
          , HB.border
          , HB.rounded
          , HB.shadowSm
          , HB.dFlex
          , HB.flexColumn
          ]
      , HP.style "background-color: #fff9c4;"
      ]
      [ -- Inhalt
        HH.div_
          [ HH.div_ [ HH.text c.author ]
          , HH.div [ HP.classes [ HB.mt1 ] ] [ HH.text c.content ]
          ]
        -- Zeitstempel klein und unten rechts
      , HH.div
          [ HP.classes [ HB.mt2 ]
          , HP.style "align-self: flex-end; font-size: 0.75rem; color: #555;" ]
          [ HH.text $
              case mFormatter of
                Nothing      -> "No timestamp found."
                Just formatr -> format formatr c.timestamp
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
      pure unit

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTimeFormatter timeFormatter a -> do
      H.modify_ \state -> state  { timeFormatter = timeFormatter }
      pure (Just a)

    SelectedCommentSection tocID markerID section a -> do
      H.modify_ \state -> state 
        { tocID = tocID
        , markerID = markerID
        , commentsection = Just section }
      pure (Just a)

