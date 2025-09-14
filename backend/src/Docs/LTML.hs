{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.LTML
    ( treeToMeta
    , treeToMeta'
    , treeRevisionToLtmlInputTree
    , nodeToLtmlInputTree
    , nodeToLtmlInputTree'
    , treeToLtmlInputTree
    ) where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Docs.TextElement (TextElement)
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision, revision, textElement)
    )
import Docs.Tree (Node (Node), NodeHeader, Tree (Leaf, Tree))

import qualified Docs.Tree as Tree

import Docs.MetaTree
    ( Meta (Meta)
    , MetaNode (MetaNode)
    , MetaTree (MetaLeaf, MetaTree)
    , TocEntry (TocEntry)
    , TreeWithMetaData (TreeWithMetaData)
    )
import qualified Docs.MetaTree as MetaTree
import qualified Docs.TextElement as TextElement
import qualified Docs.TextRevision as TextRevision
import Docs.TreeRevision (TreeRevision (TreeRevision))
import qualified Language.Lsd.AST.Common as LSD
import qualified Language.Ltml.Common as LTML
import Language.Ltml.Tree (FlaggedInputTree)
import qualified Language.Ltml.Tree as LTML
import qualified Language.Ltml.Tree.ToMeta as LTML

treeToMeta'
    :: Node TextElementRevision
    -> Either LTML.MetaError (TreeWithMetaData TextElement)
treeToMeta' input = (TextRevision.textElement <$>) <$> treeToMeta input

treeToMeta
    :: Node TextElementRevision
    -> Either LTML.MetaError (TreeWithMetaData TextElementRevision)
treeToMeta input =
    let ltmlMeta =
            first treeFromFlaggedMetaTree
                <$> LTML.treeToMeta (nodeToLtmlInputTree input)
     in case ltmlMeta of
            Left err -> Left err
            Right (Just tree, metaMap) ->
                Right $
                    TreeWithMetaData
                        { MetaTree.root = tree
                        , MetaTree.metaMap = show metaMap
                        }
            Right (Nothing, _) -> Left $ LTML.MetaBug "Wurzel ist nich da :/"

data MetaFlag a
    = TreeFlag NodeHeader
    | LeafFlag a

instance Functor MetaFlag where
    fmap f (LeafFlag x) = LeafFlag $ f x
    fmap _ (TreeFlag x) = TreeFlag x

treeFromFlaggedMetaTree
    :: LTML.FlaggedMetaTree (MetaFlag a)
    -> Maybe (Meta a)
treeFromFlaggedMetaTree
    (LTML.Flagged flag (LTML.TypedTree _ _ tree)) = case (flag, tree) of
        (TreeFlag header, LTML.Tree tocEntry xs) ->
            Just $
                Meta
                    { MetaTree.meta = toTocEntry tocEntry
                    , MetaTree.tree =
                        MetaTree $
                            MetaNode
                                { MetaTree.header = header
                                , MetaTree.children =
                                    treeFromFlaggedMetaTree `mapMaybe` xs
                                }
                    }
        (LeafFlag x, LTML.Leaf tocEntry) ->
            Just $
                Meta
                    { MetaTree.meta = toTocEntry tocEntry
                    , MetaTree.tree = MetaLeaf x
                    }
        _ -> Nothing -- Something went terribly wrong, aber ich hab jetzt auch keine lust auf error handling (`.ok()`). diesen case hätte man einfach durch striktere typen verhindern können.
      where
        toTocEntry tocEntry =
            let toText bs =
                    bs >>= either (const Nothing) Just . TE.decodeUtf8' . BL.toStrict
                label = toText $ tocEntry >>= fst
                title = toText $ snd <$> tocEntry
             in TocEntry
                    { MetaTree.label = label
                    , MetaTree.title = title
                    }

treeRevisionToLtmlInputTree
    :: TreeRevision TextElementRevision
    -> FlaggedInputTree (MetaFlag TextElementRevision)
treeRevisionToLtmlInputTree (TreeRevision _ node) = nodeToLtmlInputTree node

nodeToLtmlInputTree'
    :: Node TextElementRevision
    -> FlaggedInputTree (MetaFlag TextElement)
nodeToLtmlInputTree' =
    LTML.flaggedTreeMap
        ((\(TextElementRevision te _) -> te) <$>)
        id
        id
        . nodeToLtmlInputTree

nodeToLtmlInputTree
    :: Node TextElementRevision
    -> FlaggedInputTree (MetaFlag TextElementRevision)
nodeToLtmlInputTree (Node {Tree.header, Tree.children}) =
    let kind = LSD.KindName $ Text.unpack $ Tree.headerKind header
        type_ = LSD.TypeName $ Text.unpack $ Tree.headerType header
        heading = Tree.heading header
     in LTML.Flagged
            (TreeFlag header)
            $ LTML.TypedTree kind type_
            $ LTML.Tree heading
            $ treeToLtmlInputTree <$> children

treeToLtmlInputTree
    :: Tree TextElementRevision
    -> FlaggedInputTree (MetaFlag TextElementRevision)
treeToLtmlInputTree (Tree node) = nodeToLtmlInputTree node
treeToLtmlInputTree (Leaf textElementRevision@TextElementRevision {textElement, revision}) =
    let kind = LSD.KindName $ Text.unpack $ TextElement.textElementKind textElement
        type_ = LSD.TypeName $ Text.unpack $ TextElement.textElementType textElement
        content = maybe "" TextRevision.content revision -- TODO: no revison -> empty text?
     in LTML.Flagged (LeafFlag textElementRevision) $
            LTML.TypedTree kind type_ $
                LTML.Leaf content
