{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.LTML
    ( treeRevisionToLtmlIntpuTree
    , nodeToLtmlInputTree
    , nodeToLtmlInputTree'
    , treeToLtmlInputTree
    ) where

import qualified Data.Text as Text

import Docs.TextElement (TextElement)
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision, revision, textElement)
    )
import Docs.Tree (Node (Node), Tree (Leaf, Tree))

import qualified Docs.Tree as Tree

import qualified Docs.TextElement as TextElement
import qualified Docs.TextRevision as TextRevision
import Docs.TreeRevision (TreeRevision (TreeRevision))
import qualified Language.Lsd.AST.Common as LSD
import qualified Language.Ltml.Common as LTML
import Language.Ltml.Tree (FlaggedInputTree)
import qualified Language.Ltml.Tree as LTML

treeRevisionToLtmlIntpuTree
    :: TreeRevision TextElementRevision
    -> FlaggedInputTree (Maybe TextElementRevision)
treeRevisionToLtmlIntpuTree (TreeRevision _ node) = nodeToLtmlInputTree node

nodeToLtmlInputTree'
    :: Node TextElementRevision
    -> FlaggedInputTree (Maybe TextElement)
nodeToLtmlInputTree' =
    LTML.flaggedTreeMap
        ((\(TextElementRevision te _) -> te) <$>)
        id
        id
        . nodeToLtmlInputTree

nodeToLtmlInputTree
    :: Node TextElementRevision
    -> FlaggedInputTree (Maybe TextElementRevision)
nodeToLtmlInputTree (Node {Tree.header, Tree.children}) =
    let kind = LSD.KindName $ Text.unpack $ Tree.headerKind header
        type_ = LSD.TypeName $ Text.unpack $ Tree.headerType header
        heading = Tree.heading header
        xs = Tree.content <$> children -- TODO: Docs.Tree.Edge will disappear, so this will become obsolete
     in LTML.Flagged Nothing $
            LTML.TypedTree kind type_ $
                LTML.Tree heading $
                    treeToLtmlInputTree <$> xs

treeToLtmlInputTree
    :: Tree TextElementRevision
    -> FlaggedInputTree (Maybe TextElementRevision)
treeToLtmlInputTree (Tree node) = nodeToLtmlInputTree node
treeToLtmlInputTree (Leaf textElementRevision@TextElementRevision {textElement, revision}) =
    let kind = LSD.KindName $ Text.unpack $ TextElement.textElementKind textElement
        type_ = LSD.TypeName $ Text.unpack $ TextElement.textElementType textElement
        content = maybe "" TextRevision.content revision
     in LTML.Flagged (Just textElementRevision) $
            LTML.TypedTree kind type_ $
                LTML.Leaf content
