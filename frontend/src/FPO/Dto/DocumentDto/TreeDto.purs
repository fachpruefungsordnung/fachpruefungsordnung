module FPO.Dto.DocumentDto.TreeDto
  ( Edge(..)
  , RootTree(..)
  , Tree(..)
  , TreeHeader(..)
  , findRootTree
  , findTitleRootTree
  , getEdgeTree
  , replaceNodeRootTree
  , modifyNodeRootTree
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

newtype TreeHeader = TreeHeader
  { headerKind :: String
  , headerType :: String
  , heading :: String
  }

data RootTree a
  = Empty
  | RootTree
      { children :: Array (Edge a)
      , header :: TreeHeader
      }

data Tree a
  = Node { title :: String, children :: Array (Edge a), header :: TreeHeader }
  | Leaf { title :: String, node :: a }

data Edge a = Edge (Tree a)

getEdgeTree :: forall a. Edge a -> Tree a
getEdgeTree (Edge tree) = tree

-- automatically derive instances for Functor

derive instance functorRootTree :: Functor RootTree
derive instance functorTree :: Functor Tree
derive instance functorEdge :: Functor Edge

-- derive instance newtypeTree :: Newtype (Tree a) _
-- derive instance newtypeEdge :: Newtype (Edge a) _

-- DecodeJson instances

instance decodeJsonTreeHeader :: DecodeJson TreeHeader where
  decodeJson json = do
    obj <- decodeJson json
    headerKind <- obj .: "headerKind"
    headerType <- obj .: "headerType"
    -- `heading` might be `null`, so we provide a default value:
    heading <- obj .: "heading" <|> pure ""
    pure $ TreeHeader { headerKind, headerType, heading }

instance decodeJsonRootTree :: DecodeJson a => DecodeJson (RootTree a) where
  decodeJson json = do
    obj <- decodeJson json
    header <- obj .: "header"
    childrenArr <- obj .: "children"
    children <- traverse (map Edge <<< decodeJson) childrenArr
    pure $ RootTree { children, header }

instance decodeJsonEdge :: DecodeJson a => DecodeJson (Edge a) where
  decodeJson json = Edge <$> decodeJson json

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    content <- obj .: "content"
    typ <- content .: "type"

    case typ of
      "leaf" -> do
        leafVal <- content .: "leaf"
        node <- decodeJson leafVal
        pure $ Leaf { title, node }

      "tree" -> do
        node <- content .: "node"
        childrenArr <- node .: "children"
        children <- traverse (map Edge <<< decodeJson) childrenArr
        header <- node .: "header"
        pure $ Node { title, children, header }

      _ -> throwError $ TypeMismatch $ "Unknown node type: " <> typ

-- EncodeJson instances

instance encodeJsonTreeHeader :: EncodeJson TreeHeader where
  encodeJson (TreeHeader { headerKind, headerType, heading }) =
    encodeJson
      { headerKind
      , headerType
      , heading
      }

instance encodeJsonRootTree :: EncodeJson a => EncodeJson (RootTree a) where
  encodeJson Empty = encodeJson {}
  encodeJson (RootTree root) = encodeJson root

instance encodeJsonEdge :: EncodeJson a => EncodeJson (Edge a) where
  encodeJson (Edge child) = encodeJson child

instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson (Node { title, children, header }) =
    encodeJson
      { title
      , content:
          { type: "tree"
          , node:
              { children: map encodeJson children
              , header: encodeJson header
              }
          }
      }
  encodeJson (Leaf { title, node }) =
    encodeJson
      { title
      , content:
          { type: "leaf"
          , leaf: node
          }
      }

-- Show instances

instance showTreeHeader :: Show TreeHeader where
  show (TreeHeader { headerKind, headerType, heading }) =
    "TreeHeader { headerKind: " <> headerKind <> ", headerType: " <> headerType
      <> ", heading: "
      <> heading
      <> " }"

instance showRootTree :: Show a => Show (RootTree a) where
  show Empty = "Empty"
  show (RootTree children) =
    "RootTree { children: " <> show children <> " }"

instance showEdge :: Show a => Show (Edge a) where
  show (Edge child) =
    "Edge { child: " <> show child <> " }"

instance showTree :: Show a => Show (Tree a) where
  show (Node { title, children }) =
    "Tree { title: " <> title <> ", children: " <> show children <> " }"
  show (Leaf { title, node }) =
    "Tree { title: " <> title <> ", node: " <> show node <> " }"

-- TODO: DFS. Maybe use a different search method? But maybe not necessary,
-- because the document tree may never be that large to notice.
findRootTree :: forall a. (a -> Boolean) -> RootTree a -> Maybe a
findRootTree _ Empty = Nothing
findRootTree predicate (RootTree { children }) =
  foldr
    (\(Edge child) acc -> acc <|> findTree predicate child)
    Nothing
    children

findTree :: forall a. (a -> Boolean) -> Tree a -> Maybe a
findTree predicate (Node { children }) =
  foldr
    (\(Edge child) acc -> acc <|> findTree predicate child)
    Nothing
    children
findTree predicate (Leaf { node }) =
  if predicate node then Just node else Nothing

-- find the title of the found node
findTitleRootTree :: forall a. (a -> Boolean) -> RootTree a -> Maybe String
findTitleRootTree _ Empty = Nothing
findTitleRootTree predicate (RootTree { children }) =
  foldr
    (\(Edge child) acc -> acc <|> findTitleTree predicate child)
    Nothing
    children

findTitleTree :: forall a. (a -> Boolean) -> Tree a -> Maybe String
findTitleTree predicate (Node { children }) =
  foldr
    (\(Edge child) acc -> acc <|> findTitleTree predicate child)
    Nothing
    children
findTitleTree predicate (Leaf { title, node }) =
  if predicate node then Just title else Nothing

modifyNodeRootTree
  :: forall a
   . (a -> Boolean)
  -> (String -> String)
  -> (a -> a)
  -> RootTree a
  -> RootTree a
modifyNodeRootTree _ _ _ Empty = Empty
modifyNodeRootTree predicate newTitle newNode (RootTree { children, header }) =
  let
    newChildren =
      map (\(Edge child) -> Edge $ modifyNodeTree predicate newTitle newNode child)
        children
  in
    RootTree { children: newChildren, header }

modifyNodeTree
  :: forall a
   . (a -> Boolean)
  -> (String -> String)
  -> (a -> a)
  -> Tree a
  -> Tree a
modifyNodeTree pred newTitle newNode (Leaf { title, node }) =
  if pred node then
    Leaf { title: (newTitle title), node: (newNode node) }
  else
    Leaf { title, node }
modifyNodeTree pred newTitle newNode (Node { title, children, header }) =
  let
    newChildren =
      map (\(Edge child) -> Edge $ modifyNodeTree pred newTitle newNode child)
        children
  in
    Node { title, children: newChildren, header }

replaceNodeRootTree
  :: forall a
   . (a -> Boolean)
  -> String
  -> a
  -> RootTree a
  -> RootTree a
replaceNodeRootTree predicate newTitle newNode tree =
  modifyNodeRootTree predicate (const newTitle) (const newNode) tree

