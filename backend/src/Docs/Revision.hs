module Docs.Revision
    ( RevisionID (..)
    , TextOrTree (..)
    , RevisionKey (..)
    ) where

import Data.Time (UTCTime)
import Docs.TextRevision (TextRevisionRef)
import Docs.TreeRevision (TreeRevisionRef)
import GHC.Int (Int64)

newtype RevisionID = RevisionID
    { unRevisionID :: Int64
    }

data TextOrTree
    = Text TextRevisionRef
    | Tree TreeRevisionRef

data RevisionKey = RevisionKey
    { timestamp :: UTCTime
    , revision :: TextOrTree
    }
