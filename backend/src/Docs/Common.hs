module Docs.Common (nodeWithoutText, treeWithoutText) where

import Docs.TextElement (TextElement (TextElement), TextElementID)
import Docs.TextRevision (TextElementRevision (TextElementRevision))
import Docs.Tree (Node (Node), Tree (Leaf, Tree))

nodeWithoutText :: Node TextElementRevision -> Node TextElementID
nodeWithoutText (Node header children) =
    Node header $ treeWithoutText <$> children

treeWithoutText :: Tree TextElementRevision -> Tree TextElementID
treeWithoutText (Tree node) = Tree $ nodeWithoutText node
treeWithoutText (Leaf (TextElementRevision (TextElement id_ _ _) _)) = Leaf id_
