module FPO.Components.TOC where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Types (ShortendTOCEntry, TOCEntry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output = ChangeSection ShortendTOCEntry

data Action
  = Init
  | JumpToSection ShortendTOCEntry

data Query a = ReceiveTOCs (Array TOCEntry) a

type State =
  { tocEntries :: Array ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  }

tocview :: forall m. MonadAff m => H.Component Query Input Output m
tocview = H.mkComponent
  { initialState: \_ -> { tocEntries: [], mSelectedTocEntry: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  render :: State -> forall slots. H.ComponentHTML Action slots m
  render state =
    HH.div_
      ( map
          ( \{ id, name } ->
              HH.div
                [ HP.title ("Jump to section " <> name)
                , HP.style
                    "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0;"
                ]
                [ HH.span
                    [ HE.onClick \_ -> JumpToSection { id, name }
                    , HP.classes
                        ( [ HB.textTruncate ]
                            <>
                              if Just id == state.mSelectedTocEntry then
                                [ HB.fwBold ]
                              else []
                        )
                    , HP.style
                        "cursor: pointer; display: inline-block; min-width: 6ch;"
                    ]
                    [ HH.text name ]
                ]
          )
          state.tocEntries
      )

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

    JumpToSection entry -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just entry.id }
      H.raise (ChangeSection entry)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTOCs entries a -> do
      let
        shortendEntries = map
          ( \e ->
              { id: e.id
              , name: e.name
              }
          )
          entries
      H.modify_ \state -> state { tocEntries = shortendEntries }
      pure (Just a)

