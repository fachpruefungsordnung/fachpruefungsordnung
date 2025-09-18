-- | This module defines the meta map structure for TOC trees.
module FPO.Dto.DocumentDto.MetaTree where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Array (intercalate, length, mapWithIndex, (..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import FPO.Dto.DocumentDto.TreeDto (RootTree)

-- | Specifies the kind; e.g., "document", "section", "appendix-section", ...
type KindName = String
-- | Specifies the type; e.g., "fpo-maindoc", "section", "supersection", ...
type TypeName = String

newtype FullTypeName = FullTypeName { kindName :: KindName, typeName :: TypeName }

newtype DisplayTypeName = DisplayTypeName String

derive instance Generic DisplayTypeName _
derive newtype instance Show DisplayTypeName
derive newtype instance Eq DisplayTypeName

instance DecodeJson DisplayTypeName where
  decodeJson json = DisplayTypeName <$> decodeJson json

data ProperTypeMeta = ProperTypeMeta DisplayTypeName (TreeSyntax FullTypeName)

derive instance Generic ProperTypeMeta _
instance Show ProperTypeMeta where
  show = genericShow

instance DecodeJson ProperTypeMeta where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [ displayName, treeSyntax ] -> do
        name <- decodeJson displayName
        syntax <- decodeJson treeSyntax
        pure $ ProperTypeMeta (DisplayTypeName name) syntax
      _ -> Left $ TypeMismatch
        "Expected array with exactly 2 elements for ProperTypeMeta"

newtype HasEditableHeader = HasEditableHeader Boolean

derive instance Generic HasEditableHeader _
derive newtype instance Show HasEditableHeader
derive newtype instance Eq HasEditableHeader

instance DecodeJson HasEditableHeader where
  decodeJson json = HasEditableHeader <$> decodeJson json

data TreeSyntax a
  -- | A leaf (terminal) node, which has no children.
  = LeafSyntax
  -- | A tree node, which can contain children.
  | TreeSyntax
      HasEditableHeader
      (ChildrenOrder a)

derive instance Generic (TreeSyntax a) _
instance Show a => Show (TreeSyntax a) where
  show = genericShow

instance DecodeJson a => DecodeJson (TreeSyntax a) where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    case tag of
      "LeafSyntax" -> pure LeafSyntax
      "TreeSyntax" -> do
        contents <- obj .: "contents"
        case contents of
          [ hasEditableHeader, childrenOrder ] -> do
            header <- decodeJson hasEditableHeader
            order <- decodeJson childrenOrder
            pure $ TreeSyntax (HasEditableHeader header) order
          _ -> Left $ TypeMismatch
            "Expected array with exactly 2 elements for TreeSyntax contents"
      _ -> Left $ UnexpectedValue json

data ChildrenOrder a
  -- | Children are ordered in a specific sequence.
  = SequenceOrder (Array (Disjunction a))
  -- | Children can appear in any order and quantity (e.g., multiple sections).
  | StarOrder (Disjunction a)

derive instance Generic (ChildrenOrder a) _
instance Show a => Show (ChildrenOrder a) where
  show = genericShow

instance DecodeJson a => DecodeJson (ChildrenOrder a) where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    contents <- obj .: "contents"
    case tag of
      "SequenceOrder" -> SequenceOrder <$> decodeJson contents
      "StarOrder" -> StarOrder <$> decodeJson contents
      _ -> Left $ UnexpectedValue json

newtype Disjunction a = Disjunction (Array a)

derive instance Generic (Disjunction a) _
derive newtype instance Show a => Show (Disjunction a)
derive newtype instance Eq a => Eq (Disjunction a)

instance DecodeJson a => DecodeJson (Disjunction a) where
  decodeJson json = Disjunction <$> decodeJson json

derive instance Generic FullTypeName _
derive newtype instance Show FullTypeName
derive newtype instance Eq FullTypeName

-- Helper instance for FullTypeName
instance DecodeJson FullTypeName where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [ kindName, typeName ] -> do
        kind <- decodeJson kindName
        type_ <- decodeJson typeName
        pure $ FullTypeName { kindName: kind, typeName: type_ }
      _ -> Left $ TypeMismatch
        "Expected array with exactly 2 elements for FullTypeName"

-- | Type alias for the complete meta map
type MetaMap = Array (Tuple FullTypeName ProperTypeMeta)

-- | Helper function to decode the entire meta map
decodeMetaMap :: Json -> Either JsonDecodeError MetaMap
decodeMetaMap json = decodeJson json

newtype DocumentTreeWithMetaMap a = DocumentTreeWithMetaMap
  { metaMap :: MetaMap
  , tree :: RootTree a
  }

-- | This is an extended version of `decodeDocument` that also extracts the `metaMap`,
-- | see `DocumentTreeWithMetaMap`. It would be wise to, at some point, refactor the
-- | documentDto system and everything related to `RootTree`. For now, this is a quick
-- | solution to get the meta map alongside the document tree.
decodeDocumentWithMetaMap
  :: forall a
   . DecodeJson a
  => Json
  -> Either JsonDecodeError (DocumentTreeWithMetaMap a)
decodeDocumentWithMetaMap json = do
  obj <- decodeJson json
  revision <- obj .: "revision"
  root <- revision .: "root"
  metaMap <- revision .: "metaMap"
  pure $ DocumentTreeWithMetaMap
    { metaMap: metaMap
    , tree: root
    }

prettyPrintMetaMap :: MetaMap -> String
prettyPrintMetaMap metaMap =
  intercalate "\n" $ map ppEntry metaMap
  where
  ppEntry (Tuple fullTypeName properTypeMeta) =
    "FullTypeName: " <> ppFullTypeName fullTypeName <> "\n => ProperTypeMeta: " <>
      ppProperTypeMeta properTypeMeta

  ppFullTypeName (FullTypeName { kindName: kind, typeName: type_ }) =
    "(" <> kind <> ", " <> type_ <> ")"

  ppProperTypeMeta (ProperTypeMeta (DisplayTypeName displayName) treeSyntax) =
    "DisplayTypeName: " <> displayName <> ", TreeSyntax: " <> ppTreeSyntax treeSyntax

  ppTreeSyntax = case _ of
    LeafSyntax -> "LeafSyntax"
    TreeSyntax (HasEditableHeader hasEditableHeader) childrenOrder ->
      "TreeSyntax(HasEditableHeader: " <> show hasEditableHeader
        <> ", ChildrenOrder: "
        <> ppChildrenOrder childrenOrder
        <> ")"

  ppChildrenOrder = case _ of
    SequenceOrder disjunctions ->
      "SequenceOrder[" <> intercalate ", " (map ppDisjunction disjunctions) <> "]"
    StarOrder disjunction ->
      "StarOrder[" <> ppDisjunction disjunction <> "]"

  ppDisjunction (Disjunction arr) =
    "[" <> intercalate ", " (map show arr) <> "]"
