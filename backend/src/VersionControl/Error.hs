module VersionControl.Error (VersionControlError (..)) where

-- | represents an error occured during a version control operation
data VersionControlError
    = InsufficientPrevilige
    | DatabaseError String
    deriving (Eq, Show)
