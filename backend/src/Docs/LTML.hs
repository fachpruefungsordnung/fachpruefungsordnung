{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.LTML
    ( treeRevisionToLtmlInputTree
    , nodeToLtmlInputTree
    , nodeToLtmlInputTree'
    , treeToLtmlInputTree
    ) where

import Data.Text (Text)
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

data MetaFlag
    = MetaTree (Maybe Text)
    | MetaLeaf TextElementRevision

treeFromFlaggedMetaTree
    :: LTML.FlaggedMetaTree MetaFlag
    -> Tree TextElementRevision
treeFromFlaggedMetaTree = undefined

treeRevisionToLtmlInputTree
    :: TreeRevision TextElementRevision
    -> FlaggedInputTree (Maybe TextElementRevision)
treeRevisionToLtmlInputTree (TreeRevision _ node) = nodeToLtmlInputTree node

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
     in LTML.Flagged Nothing $
            LTML.TypedTree kind type_ $
                LTML.Tree heading $
                    treeToLtmlInputTree <$> children

treeToLtmlInputTree
    :: Tree TextElementRevision
    -> FlaggedInputTree (Maybe TextElementRevision)
treeToLtmlInputTree (Tree node) = nodeToLtmlInputTree node
treeToLtmlInputTree (Leaf textElementRevision@TextElementRevision {textElement, revision}) =
    let kind = LSD.KindName $ Text.unpack $ TextElement.textElementKind textElement
        type_ = LSD.TypeName $ Text.unpack $ TextElement.textElementType textElement
        content = maybe "" TextRevision.content revision -- TODO: no revison -> empty text?
     in LTML.Flagged (Just textElementRevision) $
            LTML.TypedTree kind type_ $
                LTML.Leaf content
