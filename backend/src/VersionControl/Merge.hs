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
