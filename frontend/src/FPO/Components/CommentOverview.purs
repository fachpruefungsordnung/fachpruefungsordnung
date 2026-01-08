module FPO.Components.CommentOverview where

import Prelude

import Data.Array (partition)
import Data.Formatter.DateTime (Formatter)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.UI.RenderComment (renderFirstComment)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Store as Store
import FPO.Translations.Translator (FPOTranslator, fromFpoTranslator)
import FPO.Translations.Util (FPOState, selectTranslator)
import FPO.Types (FirstComment)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)

type Input = Unit

-- DeleteComment later
data Output = JumpToCommentSection Int Int

data Action
  = Init
  | Receive (Connected FPOTranslator Input)
  | SelectCommentSection Int Int

data Query a
  = ReceiveTimeFormatter (Maybe Formatter) a
  | ReceiveComments Int Boolean (Array FirstComment) a

type State = FPOState
  ( tocID :: Int
  , comments :: Array FirstComment
  , mTimeFormatter :: Maybe Formatter
  , inLatest :: Boolean
  )

commentOverviewview
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component Query Input Output m
commentOverviewview = connect selectTranslator $ H.mkComponent
  { initialState: \{ context } ->
      { translator: fromFpoTranslator context
      , tocID: -1
      , comments: []
      , mTimeFormatter: Nothing
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

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state = case state.comments of
    [] ->
      HH.div [ HP.style "padding: 1rem;" ]
        [ HH.text "No comments in this section." ]
    _ ->
      let
        shortendRenderFirstComment
          :: FirstComment -> forall slots. H.ComponentHTML Action slots m
        shortendRenderFirstComment first = renderFirstComment state.translator
          state.mTimeFormatter
          state.inLatest
          true
          (SelectCommentSection state.tocID first.markerID)
          first

        parts = partition _.resolved state.comments
        -- parts.no  = unresolved comments
        -- parts.yes = resolved comments
        sorted = parts.no <> parts.yes
      in
        HH.div [ HP.style "comment-section space-y-3" ]
          (map shortendRenderFirstComment sorted)

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

    Receive { context } -> do
      H.modify_ _ { translator = fromFpoTranslator context }

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

    ReceiveComments tocID inLatest cs a -> do
      H.modify_ \state -> state { tocID = tocID, comments = cs, inLatest = inLatest }
      pure (Just a)