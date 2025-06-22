module VersionControl.Error
    ( VersionControlError (..)
    , DocumentError (..)
    , flattenVersionControlError
    ) where

-- | represents an error occured during a version control operation
data VersionControlError
    = InsufficientPrevilige
    | DatabaseError String
    | DocumentError DocumentError
    deriving (Eq, Show)

-- | represents an error occured during an operation on documents
data DocumentError = DocumentNewHeadCommitUnrelated
    deriving (Eq, Show)

class ToVersionControlError a where
    toVersionControlError :: a -> VersionControlError

instance ToVersionControlError DocumentError where
    toVersionControlError = DocumentError

-- | flattens a nested either where the outer error is a 'VersionControlError'
--   and the inner error is mappable to a 'VersionControlError'
flattenVersionControlError
    :: (ToVersionControlError a)
    => Either VersionControlError (Either a b)
    -> Either VersionControlError b
flattenVersionControlError (Left e) = Left e
flattenVersionControlError (Right (Left e)) = Left $ toVersionControlError e
flattenVersionControlError (Right (Right x)) = Right x
