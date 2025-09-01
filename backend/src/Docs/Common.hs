module Docs.Common (nodeWithoutText, treeWithoutText, edgeWithoutText) where

import Docs.TextElement (TextElement (TextElement), TextElementID)
import Docs.TextRevision (TextElementRevision (TextElementRevision))
import Docs.Tree (Edge (content), Node (Node), Tree (Leaf, Tree))

nodeWithoutText :: Node TextElementRevision -> Node TextElementID
nodeWithoutText (Node header children) =
    Node header $ edgeWithoutText <$> children

treeWithoutText :: Tree TextElementRevision -> Tree TextElementID
treeWithoutText (Tree node) = Tree $ nodeWithoutText node
treeWithoutText (Leaf (TextElementRevision (TextElement id_ _) _)) = Leaf id_

edgeWithoutText :: Edge TextElementRevision -> Edge TextElementID
edgeWithoutText edge = edge {content = treeWithoutText $ content edge}
