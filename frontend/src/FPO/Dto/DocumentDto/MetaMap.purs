-- | This module defines the meta map structure for TOC trees.
module FPO.Dto.DocumentDto.MetaTree where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Array (catMaybes, find, intercalate, length, mapWithIndex, (..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import FPO.Dto.DocumentDto.TreeDto (RootTree, TreeHeader(..))

-- | Specifies the kind; e.g., "document", "section", "appendix-section", ...
type KindName = String
-- | Specifies the type; e.g., "fpo-maindoc", "section", "supersection", ...
type TypeName = String

newtype FullTypeName = FullTypeName { kindName :: KindName, typeName :: TypeName }

getKindName :: FullTypeName -> KindName
getKindName (FullTypeName { kindName: kind }) = kind

getTypeName :: FullTypeName -> TypeName
getTypeName (FullTypeName { typeName: type_ }) = type_

newtype DisplayTypeName = DisplayTypeName String

derive instance Generic DisplayTypeName _
derive newtype instance Show DisplayTypeName
derive newtype instance Eq DisplayTypeName

instance DecodeJson DisplayTypeName where
  decodeJson json = DisplayTypeName <$> decodeJson json

data ProperTypeMeta = ProperTypeMeta DisplayTypeName (TreeSyntax FullTypeName)

isLeaf :: ProperTypeMeta -> Boolean
isLeaf (ProperTypeMeta _ syntax) = case syntax of
  LeafSyntax -> true
  TreeSyntax _ _ -> false

getDisplayNameAsString :: ProperTypeMeta -> String
getDisplayNameAsString (ProperTypeMeta (DisplayTypeName name) _) = name

getTreeSyntax :: ProperTypeMeta -> TreeSyntax FullTypeName
getTreeSyntax (ProperTypeMeta _ syntax) = syntax

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

isEditable :: HasEditableHeader -> Boolean
isEditable (HasEditableHeader b) = b

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

getAllowedItems :: ∀ a. Disjunction a -> Array a
getAllowedItems (Disjunction arr) = arr

instance Functor Disjunction where
  map f (Disjunction arr) = Disjunction (map f arr)

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

-- | An empty meta map.
emptyMetaMap :: MetaMap
emptyMetaMap = []

-- | Lookup a `ProperTypeMeta` in the `MetaMap` using a `TreeHeader`.
lookupWithHeader :: TreeHeader -> MetaMap -> Maybe ProperTypeMeta
lookupWithHeader (TreeHeader header) metaMap = do
  let kind = header.headerKind
  let type_ = header.headerType
  let fullTypeName = FullTypeName { kindName: kind, typeName: type_ }
  lookupWithFullTypeName fullTypeName metaMap

-- | Lookup a `ProperTypeMeta` in the `MetaMap` using a `FullTypeName`.
lookupWithFullTypeName :: FullTypeName -> MetaMap -> Maybe ProperTypeMeta
lookupWithFullTypeName fullTypeName metaMap = do
  Tuple _ properTypeMeta <- find (\(Tuple name _) -> name == fullTypeName) metaMap
  pure properTypeMeta

findDefinition :: FullTypeName -> MetaMap -> Maybe (Tuple FullTypeName ProperTypeMeta)
findDefinition fullTypeName metaMap =
  find (\(Tuple name _) -> name == fullTypeName) metaMap

-- | For a given `TreeHeader`, find all allowed child types based on the `MetaMap`.
-- | Returns an array of tuples containing the `FullTypeName` and corresponding `ProperTypeMeta`.
findAllowedChildren
  :: TreeHeader -> MetaMap -> Array (Tuple FullTypeName ProperTypeMeta)
findAllowedChildren header metaMap = fromMaybe [] $ do
  propertyTypeMeta <- lookupWithHeader header metaMap
  let treeSyntax = getTreeSyntax propertyTypeMeta
  case treeSyntax of
    LeafSyntax -> pure []
    TreeSyntax _ co -> case co of
      StarOrder d ->
        pure $ catMaybes $ map (flip findDefinition metaMap) $
          getAllowedItems d
      SequenceOrder _ -> pure []

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

-- Utility function for creating indentation
indent :: Int -> String
indent n = joinWith "" (map (const "  ") (1 .. n))

-- | Pretty print with nice formatting and indentation
prettyPrintMetaMap :: MetaMap -> String
prettyPrintMetaMap metaMap =
  "MetaMap:\n" <> intercalate "\n\n" (map ppEntry metaMap)
  where
  ppEntry (Tuple fullTypeName properTypeMeta) =
    indent 1 <> "┌─ " <> ppFullTypeName fullTypeName <> "\n"
      <> indent 1
      <> "└─ "
      <> ppProperTypeMeta properTypeMeta 0

  ppFullTypeName (FullTypeName { kindName: kind, typeName: type_ }) =
    "(" <> kind <> ", " <> type_ <> ")"

  ppProperTypeMeta (ProperTypeMeta (DisplayTypeName displayName) treeSyntax) level =
    "\"" <> displayName <> "\"\n"
      <> indent (level + 2)
      <> "└─ "
      <> ppTreeSyntax treeSyntax (level + 1)

  ppTreeSyntax treeSyntax level = case treeSyntax of
    LeafSyntax ->
      "LeafSyntax"
    TreeSyntax (HasEditableHeader hasEditableHeader) childrenOrder ->
      "TreeSyntax\n"
        <> indent (level + 2)
        <> "├─ HasEditableHeader: "
        <> show hasEditableHeader
        <> "\n"
        <> indent (level + 2)
        <> "└─ "
        <> ppChildrenOrder childrenOrder (level + 1)

  ppChildrenOrder childrenOrder level = case childrenOrder of
    SequenceOrder disjunctions ->
      "SequenceOrder\n" <> ppDisjunctions disjunctions (level + 1)
    StarOrder disjunction ->
      "StarOrder\n" <> ppSingleDisjunction disjunction (level + 1)

  ppDisjunctions disjunctions level =
    intercalate "\n" $ mapWithIndex
      ( \i disj ->
          indent (level + 2)
            <> (if i == (length disjunctions - 1) then "└─ " else "├─ ")
            <> "Step "
            <> show (i + 1)
            <> ": "
            <> ppDisjunctionContent disj (level + 2)
      )
      disjunctions

  ppSingleDisjunction (Disjunction arr) level =
    indent (level + 2) <> "└─ Options: " <> ppDisjunctionContent (Disjunction arr)
      (level + 2)

  ppDisjunctionContent (Disjunction arr) level = case arr of
    [ singleItem ] ->
      show singleItem
    multipleItems ->
      "[\n"
        <> intercalate ",\n"
          (map (\item -> indent (level + 2) <> show item) multipleItems)
        <> "\n"
        <> indent (level + 1)
        <> "]"
