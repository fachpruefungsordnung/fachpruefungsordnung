{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Docs.LTML
-- Description : Utility for Working With LTML Datatypes
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains utility functions for working with LTML structures
-- in the "Docs" module.
module Docs.LTML
    ( treeToMeta
    , treeToMeta'
    , treeRevisionToLtmlInputTree
    , nodeToLtmlInputTree
    , nodeToLtmlInputTreePred
    , nodeToLtmlInputTree'
    , treeToLtmlInputTree
    , treeRevisionToMeta
    ) where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import Docs.TextElement (TextElement)
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    )
import Docs.Tree (Node (Node), NodeHeader, Tree (Leaf, Tree))

import qualified Docs.Tree as Tree

import Control.Monad (mfilter)
import Docs.MetaTree
    ( Meta (Meta)
    , MetaNode (MetaNode)
    , MetaTree (MetaLeaf, MetaTree)
    , TocEntry (TocEntry)
    , TreeRevisionWithMetaData (TreeRevisionWithMetaData)
    , TreeWithMetaData (TreeWithMetaData)
    )
import qualified Docs.MetaTree as MetaTree
import Docs.Renderable (Renderable (contentOf, kindOf, typeOf))
import qualified Docs.TextRevision as TextRevision
import Docs.TreeRevision (TreeRevision (TreeRevision))
import qualified Language.Lsd.AST.Common as LSD
import qualified Language.Ltml.Common as LTML
import Language.Ltml.Tree (FlaggedInputTree)
import qualified Language.Ltml.Tree as LTML
import qualified Language.Ltml.Tree.ToMeta as LTML

-- | Obtain a @TreeRevisionWithMetaData@ from a @TreeRevision@.
treeRevisionToMeta
    :: (Renderable r)
    => TreeRevision r
    -> Either LTML.MetaError (TreeRevisionWithMetaData r)
treeRevisionToMeta (TreeRevision header root) =
    TreeRevisionWithMetaData header <$> treeToMeta root

-- | Obtain a @TreeWithMetaData@ from the root of a tree with @TextElementRevision@s.
treeToMeta'
    :: Node TextElementRevision
    -> Either LTML.MetaError (TreeWithMetaData TextElement)
treeToMeta' input = (TextRevision.textElement <$>) <$> treeToMeta input

-- | Obtain a @TreeWithMetaData@ from the root af an arbitrary tree.
treeToMeta
    :: (Renderable r)
    => Node r
    -> Either LTML.MetaError (TreeWithMetaData r)
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
                        , MetaTree.metaMap = metaMap
                        }
            Right (Nothing, _) -> Left $ LTML.MetaBug "Wurzel ist nich da :/"

-- | Some useless boilerplate datatype needed since the ltml tree does not
-- differienciate between inner nodes and leafs.
data MetaFlag a
    = TreeFlag NodeHeader
    | LeafFlag a

instance Functor MetaFlag where
    fmap f (LeafFlag x) = LeafFlag $ f x
    fmap _ (TreeFlag x) = TreeFlag x

-- | Obtains a tree node with label and title from a @FlaggedMetaTree@.
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
            let toText =
                    either (const Nothing) Just . TE.decodeUtf8' . BL.toStrict
                label = fst tocEntry >>= toText
                title = toText <$> snd tocEntry
             in TocEntry
                    { MetaTree.label = label
                    , MetaTree.title = title
                    }

-- | Obtain an LTML @FlaggedInputTree@ from an arbitrary @TreeRevision@.
treeRevisionToLtmlInputTree
    :: (Renderable r)
    => TreeRevision r
    -> FlaggedInputTree (MetaFlag r)
treeRevisionToLtmlInputTree (TreeRevision _ node) = nodeToLtmlInputTree node

-- | Obtain an LTML @FlaggedInputTree@ from the root of @TextElementRevision@ tree.
nodeToLtmlInputTree'
    :: Node TextElementRevision
    -> FlaggedInputTree (MetaFlag TextElement)
nodeToLtmlInputTree' =
    LTML.flaggedTreeMap
        ((\(TextElementRevision te _) -> te) <$>)
        id
        id
        . nodeToLtmlInputTree

-- | Obtain an LTML @FlaggedInputTree@ where nodes are flagged accordinig to the
-- result of the given predicate functions.
nodeToLtmlInputTreePred
    :: (Renderable r)
    => (NodeHeader -> Bool)
    -> (r -> Bool)
    -> Node r
    -> FlaggedInputTree Bool
nodeToLtmlInputTreePred treePred leafPred =
    LTML.flaggedTreeMap
        pred'
        id
        id
        . nodeToLtmlInputTree
  where
    pred' (TreeFlag t) = treePred t
    pred' (LeafFlag l) = leafPred l

-- | Obtain a LTML @FlaggedInputTree@ from the root of an arbitrary tree.
nodeToLtmlInputTree
    :: (Renderable r)
    => Node r
    -> FlaggedInputTree (MetaFlag r)
nodeToLtmlInputTree (Node {Tree.header, Tree.children}) =
    let kind = LSD.KindName $ Text.unpack $ Tree.headerKind header
        type_ = LSD.TypeName $ Text.unpack $ Tree.headerType header
        heading = mfilter (/= "") $ Tree.heading header
     in LTML.Flagged
            (TreeFlag header)
            $ LTML.TypedTree kind type_
            $ LTML.Tree heading
            $ treeToLtmlInputTree <$> children

-- | Obtain a LTML @FlaggedInputTree@ from an arbitrary tree.
treeToLtmlInputTree
    :: (Renderable r)
    => Tree r
    -> FlaggedInputTree (MetaFlag r)
treeToLtmlInputTree (Tree node) = nodeToLtmlInputTree node
treeToLtmlInputTree (Leaf element) =
    LTML.Flagged (LeafFlag element) $
        LTML.TypedTree (kindOf element) (typeOf element) $
            LTML.Leaf $
                contentOf element
