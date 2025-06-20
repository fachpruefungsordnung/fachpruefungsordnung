module VersionControl.Merge
    ( MergeResult (..)
    , ConflictTree (..)
    , ConflictTreeEdge (..)
    ) where

import Data.Text (Text)
import VersionControl.Commit (CommitHeader, CreateCommit, ExistingCommit)
import VersionControl.Hash (Hashed)
import VersionControl.Tree (NodeWithRef, TreeRef)

-- TODO: implement merge :)

-- | represents the result of a merge.
data MergeResult
    = NoMerge ExistingCommit
    | AutoMerge CreateCommit
    | MergeConflict ConflictTree

-- | a tree ref together with the commit it belongs to
data TreeRefWithOrigin
    = TreeRefWithOrigin CommitHeader (TreeRef (Hashed NodeWithRef))

-- | represents conflicts of a failed merge
data ConflictTree
    = DivergentTrees [TreeRefWithOrigin]
    | ConvergentTree (TreeRef (Hashed NodeWithRef))
    | AutoMerged [CommitHeader] NodeWithRef [ConflictTreeEdge]

-- | edge of a conflict tree
data ConflictTreeEdge = ConflictTreeEdge Text ConflictTree

module VersionControl.Merge
    ( MergeResult (..)
    , ConflictTree (..)
    , ConflictTreeEdge (..)
    ) where

import Data.Text (Text)
import VersionControl.Commit (CommitHeader, CreateCommit, ExistingCommit)
import VersionControl.Hash (Hashed)
import VersionControl.Tree (NodeWithRef, TreeRef)

data MergeResult
    = NoMerge ExistingCommit
    | AutoMerge CreateCommit
    | MergeConflict ConflictTree

data TreeRefWithOrigin
    = TreeRefWithOrigin CommitHeader (TreeRef (Hashed NodeWithRef))

data ConflictTree
    = DivergentTrees [TreeRefWithOrigin]
    | ConvergentTree (TreeRef (Hashed NodeWithRef))
    | AutoMerged [CommitHeader] NodeWithRef [ConflictTreeEdge]

data ConflictTreeEdge = ConflictTreeEdge Text ConflictTree
