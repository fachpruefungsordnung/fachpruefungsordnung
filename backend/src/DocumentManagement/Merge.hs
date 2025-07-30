module DocumentManagement.Merge () where

--     ( MergeResult (..)
--     , ConflictTree (..)
--     , ConflictTreeEdge (..)
--     ) where

-- import Data.Text (Text)
-- import DocumentManagement.Commit (CommitHeader, CreateCommit, ExistingCommit)
-- import DocumentManagement.Hash (Hashed)
-- import DocumentManagement.Tree (NodeWithRef, TreeRef)

-- TODO: implement merge :)

-- | represents the result of a merge.
-- data MergeResult
--     = NoMerge ExistingCommit
--     | AutoMerge CreateCommit
--     | MergeConflict ConflictTree

-- -- | a tree ref together with the commit it belongs to
-- data TreeRefWithOrigin
--     = TreeRefWithOrigin CommitHeader (TreeRef (Hashed NodeWithRef))

-- -- | represents conflicts of a failed merge
-- data ConflictTree
--     = DivergentTrees [TreeRefWithOrigin]
--     | ConvergentTree (TreeRef (Hashed NodeWithRef))
--     | AutoMerged [CommitHeader] NodeWithRef [ConflictTreeEdge]

-- -- | edge of a conflict tree
-- data ConflictTreeEdge = ConflictTreeEdge Text ConflictTree
