module FPO.Components.TOC where

import Prelude

import Ace.Range as Range
import Data.Array (intercalate, range)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Types (TOCEntry)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output = ChangeSection TOCEntry

data Action
  = Init
  | JumpToSection TOCEntry

data Query a = UpdateTOC TOCEntry a

type State =
  { tocEntries :: Array TOCEntry
  , slectedTocEntry :: Maybe Int
  }

tocview :: forall m. MonadAff m => H.Component Query Input Output m
tocview = H.mkComponent
  { initialState: \_ -> { tocEntries: [], slectedTocEntry: Nothing }
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
          ( \{ id, name, content, markers } ->
              HH.div
                [ HP.title ("Jump to section " <> name)
                , HP.style
                    "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0;"
                ]
                [ HH.span
                    [ HE.onClick \_ -> JumpToSection { id, name, content, markers }
                    , HP.classes
                        ( [ HB.textTruncate ]
                            <>
                              if Just id == state.slectedTocEntry then
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
      -- Since all example entries are similar, we create the same markers for all
      mark <- H.liftEffect $ Range.create 7 3 7 26
      let
        -- Create initial TOC entries
        entries = map
          ( \n ->
              { id: n
              , name: "§" <> show n <> " This is Paragraph " <> show n
              , content: Just
                  ( intercalate "\n" $
                      [ "# This is content of §" <> show n
                      , ""
                      , "-- This is a developer comment."
                      , ""
                      , "## To-Do List"
                      , ""
                      , "1. Document initial setup."
                      , "2. <*Define the API*>                        % LTML: bold"
                      , "3. <_Underline important interface items_>   % LTML: underline"
                      , "4. </Emphasize optional features/>           % LTML: italic"
                      , ""
                      , "/* Note: Nested styles are allowed,"
                      , "   but not transitively within the same tag type!"
                      , "   Written in a code block."
                      , "*/"
                      , ""
                      , "<*This is </allowed/>*>                      % valid nesting"
                      , "<*This is <*not allowed*>*>                  % invalid, but still highlighted"
                      , ""
                      , "## Status"
                      , ""
                      , "Errors can no longer be marked as such, see error!"
                      , "Comment this section out of the code."
                      , ""
                      , "TODO: Write the README file."
                      , "FIXME: The parser fails on nested blocks."
                      , "NOTE: We're using this style as a placeholder."
                      ]
                  )
              , markers: Just
                  [ { id: 1
                    , type: "info"
                    , range: mark
                    , startRow: 7
                    , startCol: 3
                    -- TODO make this a real comment
                    , comment: Nothing
                    }
                  ]
              }
          )
          (range 1 11)
      -- Comment it out for now, to let the other text show up first in editor
      -- head has to be imported from Data.Array
      -- Put first entry in editor
      --   firstEntry = case head entries of
      --     Nothing -> { id: -1, name: "No Entry", content: Just [ "" ] }
      --     Just entry -> entry
      -- H.tell _editor unit (Editor.ChangeSection firstEntry)
      H.modify_ \st -> do
        st
          { tocEntries = entries }

    JumpToSection entry -> do
      H.modify_ \state ->
        state { slectedTocEntry = Just entry.id }
      H.raise (ChangeSection entry)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    UpdateTOC entry a -> do
      H.modify_ \state ->
        state
          { tocEntries =
              map (\e -> if e.id == entry.id then entry else e) state.tocEntries
          }
      pure (Just a)

