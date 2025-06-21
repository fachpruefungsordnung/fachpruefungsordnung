module FPO.Components.Comment where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Types (Comment, CommentSection)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.DateTime (time)

type Input = Unit

data Output = UpdateComment CommentSection

data Action
  = Init
  | UpdateDraft String
  | SendComment

data Query a = SelectedCommentSection CommentSection a

type State =
  { commentsection :: Maybe CommentSection
  , commentDraft :: String
  }

tocview :: forall m. MonadAff m => H.Component Query Input Output m
tocview = H.mkComponent
  { initialState: \_ -> { commentsection: Nothing, commentDraft: "" }
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
        ( map renderComment commentSection.comments
            <> [ renderInput state.commentDraft ]
        )
  
  renderComment :: Comment -> forall slots. H.ComponentHTML Action slots m
  renderComment c =
    HH.div [ HP.style "rounded-md shadow-md p-3 bg-white border border-gray-300" ]
      [ HH.div [HP.style "text-sm font-semibold text-gray-700" ] [ HH.text c.author ]
      , HH.div [ HP.style "mt-1 text-gray-800" ] [ HH.text c.content ]
      , HH.div [ HP.style "mt-2 text-xs text-gray-500" ] [ HH.text (show (time c.timestamp)) ]
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

    SelectedCommentSection section a -> do
      H.modify_ \state ->
        state { commentsection = Just section }
      pure (Just a)

