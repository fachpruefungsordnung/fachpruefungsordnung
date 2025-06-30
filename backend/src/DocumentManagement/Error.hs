module DocumentManagement.Error
    ( DocumentManagementError (..)
    , DocumentError (..)
    , flattenDocumentManagementError
    ) where

-- | represents an error occured during a version control operation
data DocumentManagementError
    = InsufficientPrevilige
    | DatabaseError String
    | DocumentError DocumentError
    deriving (Eq, Show)

-- | represents an error occured during an operation on documents
data DocumentError = DocumentNewHeadCommitUnrelated
    deriving (Eq, Show)

class ToDocumentManagementError a where
    toDocumentManagementError :: a -> DocumentManagementError

instance ToDocumentManagementError DocumentError where
    toDocumentManagementError = DocumentError

-- | flattens a nested either where the outer error is a 'DocumentManagementError'
--   and the inner error is mappable to a 'DocumentManagementError'
flattenDocumentManagementError
    :: (ToDocumentManagementError a)
    => Either DocumentManagementError (Either a b)
    -> Either DocumentManagementError b
flattenDocumentManagementError (Left e) = Left e
flattenDocumentManagementError (Right (Left e)) = Left $ toDocumentManagementError e
flattenDocumentManagementError (Right (Right x)) = Right x
