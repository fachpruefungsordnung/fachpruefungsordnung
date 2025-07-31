module FPO.Components.TOC where

import Prelude

import Data.Array (concat, mapWithIndex, snoc, uncons, updateAt, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import FPO.Dto.TreeDto (Edge(..), RootTree(..), Tree(..))
import FPO.Types (ShortendTOCEntry, TOCTree, shortenTOC)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

type Input = Unit

data Output = ChangeSection Int

data Action
  = Init
  | JumpToSection Int
  | ToggleAddMenu (Array Int)
  | CreateNewSubsection (Array Int)
  | CreateNewSection (Array Int)

data Query a = ReceiveTOCs (TOCTree) a

type State =
  { tocEntries :: RootTree ShortendTOCEntry
  , mSelectedTocEntry :: Maybe Int
  , showAddMenu :: Array Int
  }

tocview :: forall m. MonadAff m => H.Component Query Input Output m
tocview = H.mkComponent
  { initialState: \_ -> 
    { tocEntries: Empty
    , mSelectedTocEntry: Nothing
    , showAddMenu: [-1] }
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
      (rootTreeToHTML state.showAddMenu state.mSelectedTocEntry state.tocEntries)

  handleAction :: Action -> forall slots. H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Init -> do
      pure unit

    JumpToSection id -> do
      H.modify_ \state ->
        state { mSelectedTocEntry = Just id }
      H.raise (ChangeSection id)
    
    ToggleAddMenu path -> do
      H.modify_ \state ->
        state { showAddMenu = 
          if state.showAddMenu == [-1] || state.showAddMenu /= path
            then path 
            else [-1] }
    
    CreateNewSubsection path -> do
      state <- H.get
      let 
        newEntry = Leaf { title: "New Subsection", node: { id: -1, name: "New Subsection" } }
        newTree = addRootNode path newEntry state.tocEntries
      H.modify_ \st ->
        st { tocEntries = newTree, showAddMenu = [-1] }

    CreateNewSection path -> do
      state <- H.get
      let
        newEntry = Node { title: "New Section", children: [], header: { headerKind: "section", headerType: "section" } }
        newTree = addRootNode path newEntry state.tocEntries
      H.modify_ \st ->
        st { tocEntries = newTree, showAddMenu = [-1] }



  handleQuery
    :: forall slots a
     . Query a
    -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of

    ReceiveTOCs entries a -> do
      let
        shortendEntries = map shortenTOC entries
      H.modify_ \state ->
        state { tocEntries = shortendEntries, mSelectedTocEntry = Nothing }
      pure (Just a)

  rootTreeToHTML
    :: Array Int
    -> Maybe Int
    -> RootTree ShortendTOCEntry
    -> forall slots
     . Array (H.ComponentHTML Action slots m)
  rootTreeToHTML _ _ Empty = []
  rootTreeToHTML menuPath mSelectedTocEntry (RootTree { children }) =
    [ HH.div
        [ HP.style
            "white-space: nowrap; text-overflow: ellipsis; padding: 0.25rem 0; display: flex; align-items: center;"
        ]
        [ HH.span
            ( 
              [ HP.classes [ HB.textTruncate ]
              , HP.style " font-size: 2rem;"
              ]
            )
            --TODO: use the actual document name
            [ HH.text "Documentname" ]
        -- Wrapper für Button + Dropdown
        , HH.div
          [ HP.style "position: relative; margin-left: 0.5rem;" ]
          [ -- ➕ Button
            HH.button
              [ HE.onClick \_ -> ToggleAddMenu []
              , HP.style "font-size: 1.5rem; cursor: pointer; background: none; border: none;" ]
              [ HH.text "➕" ]

            -- Dropdown-Menü
          , if menuPath == [] then
              HH.div
                [ HP.style
                    "position: absolute; top: 100%; left: 0; background: white; border: 1px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.1); z-index: 1000;"
                ]
                [ HH.button
                    [ HE.onClick \_ -> CreateNewSubsection []
                    , HP.style "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;" ]
                    [ HH.text "➕ Unterabschnitt" ]
                , HH.button
                    [ HE.onClick \_ -> CreateNewSection []
                    , HP.style "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;" ]
                    [ HH.text "➕ Abschnitt" ]
                ]
            else
              HH.text ""
          ]
        ]
    ] <> concat (mapWithIndex
          (\ix (Edge child) ->
            treeToHTML menuPath 1 mSelectedTocEntry [ix] child
          )
          children)

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
          ( "white-space: nowrap; text-overflow: ellipsis; padding: 0.25rem 0; display: flex; align-items: center;"
              <> (show (1.5 * toNumber n))
              <> "rem;"
          )
      ]
      [ HH.span
        [ HP.classes [ HB.textTruncate ]
        , HP.style $ if n == 1 then " font-size: 1.25rem;" else ""
        ]
        [ HH.text (title) ]
        -- Wrapper für Button + Dropdown
        , HH.div
          [ HP.style "position: relative; margin-left: 0.5rem;" ]
          [ -- ➕ Button
            HH.button
              [ HE.onClick \_ -> ToggleAddMenu path
              , HP.style "font-size: 1.5rem; cursor: pointer; background: none; border: none;" ]
              [ HH.text "➕" ]

            -- Dropdown-Menü
          , if menuPath == path then
              HH.div
                [ HP.style
                    "position: absolute; top: 100%; left: 0; background: white; border: 1px solid #ccc; box-shadow: 0 2px 5px rgba(0,0,0,0.1); z-index: 1000;"
                ]
                [ HH.button
                    [ HE.onClick \_ -> CreateNewSubsection path
                    , HP.style "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;" ]
                    [ HH.text "➕ Unterabschnitt" ]
                , HH.button
                    [ HE.onClick \_ -> CreateNewSection path
                    , HP.style "display: block; padding: 0.5rem 1rem; width: 100%; text-align: left; background: white; border: none; cursor: pointer;" ]
                    [ HH.text "➕ Abschnitt" ]
                ]
            else
              HH.text ""
          ]
      ]
    ] <> concat (mapWithIndex
          (\ix (Edge child) ->
            treeToHTML menuPath (n + 1) mSelectedTocEntry (path <> [ix]) child
          )
          children)
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
      { id, name:_ } = node

addRootNode 
  :: Array Int 
  -> Tree ShortendTOCEntry
  -> RootTree ShortendTOCEntry 
  -> RootTree ShortendTOCEntry
addRootNode [] entry (RootTree { children, header }) = 
  RootTree { children: snoc children (Edge entry), header }
addRootNode _ entry Empty =
  RootTree { children: [Edge entry], header: { headerKind: "root", headerType: "root" } }
addRootNode path entry (RootTree {children, header}) = 
  case uncons path of
    Nothing -> 
      RootTree { children: snoc children (Edge entry), header }
    Just { head, tail } -> 
      let
        child = 
          fromMaybe 
            (Edge (Leaf { title: "Error", node: {id: (-1), name: "error"} })) 
            (children !! head)
        newChildren = 
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        RootTree { children: newChildren, header }
        
addNode 
  :: Array Int 
  -> Tree ShortendTOCEntry
  -> Edge ShortendTOCEntry 
  -> Edge ShortendTOCEntry
addNode _ _ (Edge (Leaf { title, node })) =
  Edge (Leaf { title, node }) -- Cannot add to a leaf
addNode [] entry (Edge (Node { title, children, header })) =
  Edge (Node { title, children: snoc children (Edge entry), header })
addNode path entry (Edge (Node { title, children, header })) =
  case uncons path of
    Nothing -> 
      Edge (Node { title, children: snoc children (Edge entry), header })
    Just { head, tail } -> 
      let
        child = 
          fromMaybe 
            (Edge (Leaf { title: "Error", node: {id: (-1), name: "error"} })) 
            (children !! head)
        newChildren' = 
          case updateAt head (addNode tail entry child) children of
            Nothing -> children
            Just res -> res
      in
        Edge (Node { title, children: newChildren', header })

