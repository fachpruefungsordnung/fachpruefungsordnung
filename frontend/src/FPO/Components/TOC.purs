module FPO.Components.TOC where

import Prelude

import Data.Array (concat, mapWithIndex)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Request as Request
import FPO.Data.Store as Store
import FPO.Dto.DocumentDto (DocumentID)
import FPO.Dto.DocumentDto as DocumentDto
import FPO.Dto.PostTextDto (PostTextDto(..))
import FPO.Dto.PostTextDto as PostTextDto
import FPO.Dto.TreeDto (Edge(..), RootTree(..), Tree(..))
import FPO.Types (ShortendTOCEntry, TOCEntry, TOCTree, shortenTOC)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output
  = ChangeSection Int
  | AddNode (Array Int) (Tree TOCEntry)

data Action
  = Init
  | JumpToSection Int
  | ToggleAddMenu (Array Int)
  | CreateNewSubsection (Array Int)
  | CreateNewSection (Array Int)

data Query a = ReceiveTOCs (TOCTree) a

type State =
  { documentName :: String
  , tocEntries :: RootTree ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  , showAddMenu :: Array Int
  }

tocview
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => DocumentID
  -> H.Component Query Input Output m
tocview docID = H.mkComponent
  { initialState: \_ ->
      { documentName: ""
      , tocEntries: Empty
      , mSelectedTocEntry: Nothing
      , showAddMenu: [ -1 ]
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
  render state =
    HH.div_
      ( rootTreeToHTML state.documentName state.showAddMenu state.mSelectedTocEntry
          state.tocEntries
      )

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      mDoc <- H.liftAff $ Request.getNewDocumentHeader docID
      let
        docName = case mDoc of
          Nothing -> ""
          Just doc -> DocumentDto.getNDHName doc
      H.modify_ \st -> do
        st { documentName = docName }

    JumpToSection id -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just id }
      H.raise (ChangeSection id)

    ToggleAddMenu path -> do
      H.modify_ \state ->
        state
          { showAddMenu =
              if state.showAddMenu == [ -1 ] || state.showAddMenu /= path then path
              else [ -1 ]
          }

    CreateNewSubsection path -> do
      H.modify_ \st ->
        st { showAddMenu = [ -1 ] }
      gotRes <- H.liftAff $
        Request.postJson ("/docs/" <> show docID <> "/text")
          ( PostTextDto.encodePostTextDto
              (PostTextDto { identifier: 0, kind: "new Text" })
          )
      case gotRes of
        Left _ -> pure unit
        Right res -> do
          let
            textDto = PostTextDto.decodePostTextDto res.body
          case textDto of
            Left _ -> pure unit
            Right dto -> do
              let
                newEntry =
                  Leaf
                    { title: "New Subsection"
                    , node:
                        { id: PostTextDto.getID dto
                        , name: "New Subsection"
                        , newMarkerNextID: 0
                        , markers: []
                        }
                    }
              H.raise (AddNode path newEntry)

    CreateNewSection path -> do
      H.modify_ \st ->
        st { showAddMenu = [ -1 ] }
      let
        newEntry = Node
          { title: "New Section"
          , children: []
          , header: { headerKind: "section", headerType: "section" }
          }
      H.raise (AddNode path newEntry)

  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTOCs entries a -> do
      let
        shortendEntries = map shortenTOC entries
      H.modify_ \state ->
        state
          { tocEntries = shortendEntries
          , mSelectedTocEntry = Nothing
          }
      pure (Just a)

  rootTreeToHTML
    :: String
    -> Array Int
    -> Maybe Int
    -> RootTree ShortendTOCEntry
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ _ Empty = []
  rootTreeToHTML docName menuPath mSelectedTocEntry (RootTree { children }) =
    [ HH.div
        [ HP.style
            "white-space: nowrap; text-overflow: ellipsis; padding: 0.25rem 0; display: flex; align-items: center;"
        ]
        [ HH.span
            ( [ HP.classes [ HB.textTruncate ]
              , HP.style " font-size: 2rem;"
              ]
            )
            --TODO: use the actual document name
            [ HH.text docName ]
        -- Wrapper für Button + Dropdown
        , HH.div
            [ HP.style "position: relative; margin-left: 0.5rem;" ]
            [ -- ➕ Button
              HH.button
                [ HE.onClick \_ -> ToggleAddMenu []
                , HP.style
                    "font-size: 1.5rem; cursor: pointer; background: none; border: none;"
                ]
                [ HH.text "➕" ]

            -- Dropdown-Menü
            , if menuPath == [] then
                HH.div
                  [ HP.style
                      "position: absolute; top: 100%; left: 0; background: white; border: 1px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.1); z-index: 1000;"
                  ]
                  [ HH.button
                      [ HE.onClick \_ -> CreateNewSubsection []
                      , HP.style
                          "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;"
                      ]
                      [ HH.text "➕ Unterabschnitt" ]
                  , HH.button
                      [ HE.onClick \_ -> CreateNewSection []
                      , HP.style
                          "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;"
                      ]
                      [ HH.text "➕ Abschnitt" ]
                  ]
              else
                HH.text ""
            ]
        ]
    ] <> concat
      ( mapWithIndex
          ( \ix (Edge child) ->
              treeToHTML menuPath 1 mSelectedTocEntry [ ix ] child
          )
          children
      )

  treeToHTML
    :: Array Int
    -> Int
    -> Maybe Int
    -- Path to the current section, used for adding new sections
    -> Array Int
    -> Tree ShortendTOCEntry
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  treeToHTML menuPath n mSelectedTocEntry path (Node { title, children }) =
    [ HH.div
        [ HP.style
            ( "white-space: nowrap; text-overflow: ellipsis; padding: 0.25rem 0; display: flex; align-items: center; padding-left: "
                <> (show (1.5 * toNumber n))
                <> "rem;"
            )
        ]
        [ HH.span
            [ HP.classes [ HB.textTruncate ]
            , HP.style $ if n == 1 then " font-size: 1.25rem;" else ""
            ]
            [ HH.text title ]
        -- Wrapper für Button + Dropdown
        , HH.div
            [ HP.style "position: relative; margin-left: 0.5rem;" ]
            [ -- ➕ Button
              HH.button
                [ HE.onClick \_ -> ToggleAddMenu path
                , HP.style
                    "font-size: 1.5rem; cursor: pointer; background: none; border: none;"
                ]
                [ HH.text "➕" ]

            -- Dropdown-Menü
            , if menuPath == path then
                HH.div
                  [ HP.style
                      "position: absolute; top: 100%; left: 0; background: white; border: 1px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.1); z-index: 1000;"
                  ]
                  [ HH.button
                      [ HE.onClick \_ -> CreateNewSubsection path
                      , HP.style
                          "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;"
                      ]
                      [ HH.text "➕ Unterabschnitt" ]
                  , HH.button
                      [ HE.onClick \_ -> CreateNewSection path
                      , HP.style
                          "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;"
                      ]
                      [ HH.text "➕ Abschnitt" ]
                  ]
              else
                HH.text ""
            ]
        ]
    ] <> concat
      ( mapWithIndex
          ( \ix (Edge child) ->
              treeToHTML menuPath (n + 1) mSelectedTocEntry (path <> [ ix ]) child
          )
          children
      )
  treeToHTML _ n mSelectedTocEntry _ (Leaf { title, node }) =
    [ HH.div
        [ HP.title ("Jump to section " <> title)
        , HP.style
            ( "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 0.25rem 0; padding-left: "
                <> (show (1.5 * toNumber n))
                <> "rem;"
            )
        ]
        [ HH.span
            ( ( if n == 0 then []
                else [ HE.onClick \_ -> JumpToSection id ]
              )
                <>
                  [ HP.classes
                      ( [ HB.textTruncate ]
                          <>
                            if Just id == mSelectedTocEntry then
                              [ HB.fwBold ]
                            else []
                      )
                  , HP.style
                      ( "cursor: pointer; display: inline-block; min-width: 6ch;"
                          <>
                            if n == 1 then " font-size: 1.25rem;"
                            else ""
                      )
                  ]
            )
            [ HH.text (title) ]
        ]
    ]
    where
    { id, name: _ } = node

