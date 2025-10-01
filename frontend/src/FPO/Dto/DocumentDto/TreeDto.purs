-- | This module defines the document tree structure with meta information.
-- | The meta map is not part of this (one layer above).
module FPO.Dto.DocumentDto.TreeDto
  ( Edge(..)
  , Meta(..)
  , Result(..)
  , RootTree(..)
  , Tree(..)
  , TreeHeader(..)
  , errorMeta
  , findRootTree
  , findTitleRootTree
  , getContent
  , getContentOr
  , getEdgeTree
  , getFullTitle
  , getHeaderKind
  , getHeaderType
  , getHeading
  , getShortTitle
  , modifyNodeRootTree
  , replaceNodeRootTree
  , unspecifiedMeta
  , updateHeading
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length, splitAt)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)

newtype TreeHeader = TreeHeader
  { headerKind :: String
  , headerType :: String
  , heading :: String
  }

-- | Updates the heading of a `TreeHeader`.
updateHeading :: String -> TreeHeader -> TreeHeader
updateHeading newHeading (TreeHeader header) =
  TreeHeader $ header { heading = newHeading }

getHeading :: TreeHeader -> String
getHeading (TreeHeader header) = header.heading

getHeaderKind :: TreeHeader -> String
getHeaderKind (TreeHeader header) = header.headerKind

getHeaderType :: TreeHeader -> String
getHeaderType (TreeHeader header) = header.headerType

-- | Metadata for a tree node. `title` is a html-escaped string that represents
-- | the title of the node. `label` is an optional html-escaped string that
-- | represents the label of the node (e.g. "§x", etc.).
-- | It can be `Nothing` if the node has no label.
-- |
-- | Note: Altough the label and title are html-escaped strings, they should not
-- |       contain any html tags except for <span> and </span>. Because there is no convenient
-- |       way to insert raw html into Halogen rendering, we can drop these tags before
-- |       rendering (using, e.g., `getFullTitle`). This isn't a perfect solution,
-- |       but easier than other approaches. We could use `FPO.UI.HTML.rawHtml`, but
-- |       this would not work for tooltips...
-- |       TODO: This workaround causes a bug where some special characters are not rendered
-- |             correctly, see issue #656.
newtype Meta = Meta
  { label :: Maybe String
  , title :: Result (Maybe String)
  }

-- | Signals if a generated TOC title was parsed successfully or not.
data Result a = Success a | Error a

getContent :: ∀ a. Result a -> a
getContent (Success x) = x
getContent (Error x) = x

getContentOr :: ∀ a. a -> Result (Maybe a) -> a
getContentOr b = fromMaybe b <<< getContent

errorMeta :: Meta
errorMeta = Meta { label: Nothing, title: Error $ Just "(error)" }

unspecifiedMeta :: Meta
unspecifiedMeta = Meta { label: Nothing, title: Error $ Just "(unspecified)" }

-- | Returns the full title of the node, including the label if it exists.
-- | Removes HTML tags.
getFullTitle :: Meta -> String
getFullTitle (Meta { label, title }) =
  case label of
    Just l -> removeHTMLTags l <> " " <> removeHTMLTags
      (getContentOr "(missing)" title)
    Nothing -> removeHTMLTags $ getContentOr "(missing)" title

-- | Returns the short title of the node, i.e. the title without the label.
-- | Removes HTML tags.
getShortTitle :: Meta -> String
getShortTitle (Meta { title }) = removeHTMLTags $ getContentOr "(missing)" title

-- | Drops the <span> and </span> tags from a string, if they exist.
-- | Otherwise, the string is returned unchanged.
removeHTMLTags :: String -> String
removeHTMLTags str = if a == "<span>" && c == "</span>" then middle else str
  where
  { before: a, after: b } = splitAt 6 str
  { before: middle, after: c } = splitAt (length b - 7) b

data RootTree a
  = Empty
  | RootTree
      { children :: Array (Edge a)
      , header :: TreeHeader
      }

data Tree a
  = Node { meta :: Meta, children :: Array (Edge a), header :: TreeHeader }
  | Leaf { meta :: Meta, node :: a }

data Edge a = Edge (Tree a)

getEdgeTree :: forall a. Edge a -> Tree a
getEdgeTree (Edge tree) = tree

derive instance functorRootTree :: Functor RootTree
derive instance functorTree :: Functor Tree
derive instance functorEdge :: Functor Edge

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
    tree <- obj .: "tree"
    con <- tree .: "contents"
    childrenArr <- con .: "children"
    header <- con .: "header"
    children <- traverse (map Edge <<< decodeJson) childrenArr
    pure $ RootTree { children, header }

instance decodeJsonEdge :: DecodeJson a => DecodeJson (Edge a) where
  decodeJson json = Edge <$> decodeJson json

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson json = do
    obj <- decodeJson json
    meta <- obj .: "meta"
    tree <- obj .: "tree"
    contents <- tree .: "contents"
    tag <- tree .: "tag"

    case tag of
      "MetaLeaf" -> do
        node <- decodeJson (encodeJson contents)
        pure $ Leaf { meta, node }

      "MetaTree" -> do
        childrenArr <- contents .: "children"
        children <- traverse (map Edge <<< decodeJson) childrenArr
        header <- contents .: "header"
        pure $ Node { meta, children, header }

      _ -> throwError $ TypeMismatch $ "Unknown node type: " <> tag

instance decodeJsonMeta :: DecodeJson Meta where
  decodeJson json = do
    obj <- decodeJson json
    label <- obj .:? "label"
    title <- obj .: "title"
    pure $ Meta { label, title: title }

instance decodeJsonResult :: DecodeJson a => DecodeJson (Result a) where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    contents <- obj .: "contents"
    case tag of
      "Success" -> pure $ Success contents
      "Error" -> pure $ Error contents
      _ -> throwError $ TypeMismatch $ "Unknown result type: " <> tag

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
  encodeJson (Node { children, header }) =
    encodeJson
      { type: "tree"
      , node:
          { children: map encodeJson children
          , header: encodeJson header
          }
      }
  encodeJson (Leaf { node }) =
    encodeJson
      { type: "leaf"
      , leaf: node
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
  show (Node { meta, children }) =
    "Tree { meta: " <> show meta <> ", children: " <> show children <> " }"
  show (Leaf { meta, node }) =
    "Tree { meta: " <> show meta <> ", node: " <> show node <> " }"

instance showMeta :: Show Meta where
  show (Meta { label, title }) =
    "Meta { label: " <> show label <> ", title: " <> show title <> " }"

instance showResult :: Show a => Show (Result a) where
  show r = "Result { contents: " <> fst contentsAndTag <> ", tag: "
    <> snd contentsAndTag
    <> " }"
    where
    contentsAndTag =
      case r of
        Success a -> Tuple (show a) "Success"
        Error a -> Tuple (show a) "Error"

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

-- | Returns `Nothing` if no matching node is found or if the found node has no title(!).
findTitleTree :: forall a. (a -> Boolean) -> Tree a -> Maybe String
findTitleTree predicate (Node { children }) =
  foldr
    (\(Edge child) acc -> acc <|> findTitleTree predicate child)
    Nothing
    children
findTitleTree predicate (Leaf { meta: Meta meta, node }) =
  if predicate node then getContent meta.title else Nothing

modifyNodeRootTree
  :: forall a
   . (a -> Boolean)
  -> (a -> a)
  -> RootTree a
  -> RootTree a
modifyNodeRootTree _ _ Empty = Empty
modifyNodeRootTree predicate newNode (RootTree { children, header }) =
  let
    newChildren =
      map (\(Edge child) -> Edge $ modifyNodeTree predicate newNode child)
        children
  in
    RootTree { children: newChildren, header }

modifyNodeTree
  :: forall a
   . (a -> Boolean)
  -> (a -> a)
  -> Tree a
  -> Tree a
modifyNodeTree pred newNode (Leaf { meta, node }) =
  if pred node then
    Leaf { meta, node: (newNode node) }
  else
    Leaf { meta, node }
modifyNodeTree pred newNode (Node { meta, children, header }) =
  let
    newChildren =
      map (\(Edge child) -> Edge $ modifyNodeTree pred newNode child)
        children
  in
    Node { meta, children: newChildren, header }

replaceNodeRootTree
  :: forall a
   . (a -> Boolean)
  -> a
  -> RootTree a
  -> RootTree a
replaceNodeRootTree predicate newNode tree =
  modifyNodeRootTree predicate (const newNode) tree

