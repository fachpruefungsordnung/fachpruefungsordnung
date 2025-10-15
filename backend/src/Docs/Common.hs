-- |
-- Module      : Docs.Common
-- Description : Common Functions for the Document Management
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains some common utility functions for the
-- Document Management.
module Docs.Common (nodeWithoutText, treeWithoutText) where

import Docs.TextElement (TextElement (TextElement), TextElementID)
import Docs.TextRevision (TextElementRevision (TextElementRevision))
import Docs.Tree (Node (Node), Tree (Leaf, Tree))

-- | Takes a node of a full tree revision and throws the text element revisions away
nodeWithoutText :: Node TextElementRevision -> Node TextElementID
nodeWithoutText (Node header children) =
    Node header $ treeWithoutText <$> children

-- | Takes a full tree revision and throws the text element revision away
treeWithoutText :: Tree TextElementRevision -> Tree TextElementID
treeWithoutText (Tree node) = Tree $ nodeWithoutText node
treeWithoutText (Leaf (TextElementRevision (TextElement id_ _ _) _)) = Leaf id_
