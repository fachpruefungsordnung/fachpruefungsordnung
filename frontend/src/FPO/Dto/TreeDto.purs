module FPO.Dto.TreeDto
    ( Tree(..)
    , Edge(..)
    , findTree
    ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)

data Tree a 
  = Empty
  | Node { node :: a, children :: Array (Edge a) }

data Edge a = Edge
  { title :: String
  , child :: Tree a
  }

derive instance functorTree :: Functor Tree 
  -- map f (Tree { node, children }) = Tree { node: f node, children: map (map f) children }
derive instance functorEdge :: Functor Edge 
  -- map f (Edge { title, child }) = Edge { title, child: map f child }


-- derive instance newtypeTree :: Newtype (Tree a) _
-- derive instance newtypeEdge :: Newtype (Edge a) _

instance decodeJsonEdge :: DecodeJson a => DecodeJson (Edge a) where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    child <- obj .: "child"
    pure $ Edge { title, child }

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson json = do
    obj <- decodeJson json
    node <- obj .: "node"
    children <- obj .: "children"
    pure $ Node { node, children }

instance encodeJsonEdge :: EncodeJson a => EncodeJson (Edge a) where
  encodeJson (Edge { title, child }) =
    encodeJson
      { title
      , child
      }

instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson Empty = encodeJson {}
  encodeJson (Node { node, children }) =
    encodeJson
      { node
      , children
      }

instance showEdge :: Show a => Show (Edge a) where
  show (Edge { title, child }) =
    "Edge { title: " <> show title <> ", child: " <> show child <> " }"

instance showTree :: Show a => Show (Tree a) where
  show Empty                     = "Empty"
  show (Node { node, children }) =
    "Tree { node: " <> show node <> ", children: " <> show children <> " }"

-- TODO: DFS. Maybe use a different search method? But maybe not necessary,
-- because the document tree may never be that large to notice. 
findTree :: forall a. (a -> Boolean) -> Tree a -> Maybe a
findTree _ Empty = Nothing
findTree predicate (Node { node, children }) =
  if predicate node then Just node
  else
    foldr
      (\(Edge { child }) acc -> acc <|> findTree predicate child)
      Nothing
      children
