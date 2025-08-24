{-# LANGUAGE DeriveGeneric #-}

module Docs.Revision
    ( RevisionID (..)
    , TextOrTree (..)
    , RevisionKey (..)
    , RevisionRef (..)
    , textRevisionRefFor
    , treeRevisionRefFor
    , prettyPrintRevisionRef
    ) where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Function ((&))
import Data.OpenApi
    ( HasExclusiveMinimum (exclusiveMinimum)
    , HasMinimum (minimum_)
    , HasType (type_)
    , OpenApiType (OpenApiInteger)
    , ToSchema
    )
import Data.OpenApi.ParamSchema
import Data.OpenApi.Schema (declareNamedSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Time (UTCTime)
import Docs.Document (DocumentID (unDocumentID))
import Docs.TextElement (TextElementRef)
import Docs.TextRevision (TextRevisionRef (TextRevisionRef))
import qualified Docs.TextRevision as TextRevision
import Docs.TreeRevision (TreeRevisionRef (TreeRevisionRef))
import qualified Docs.TreeRevision as TreeRevision
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant (FromHttpApiData (parseUrlPiece))

newtype RevisionID = RevisionID
    { unRevisionID :: Int64
    }
    deriving (Eq, Ord, Show)

instance ToJSON RevisionID where
    toJSON = toJSON . unRevisionID

instance FromJSON RevisionID where
    parseJSON = fmap RevisionID . parseJSON

instance ToSchema RevisionID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema RevisionID where
    toParamSchema _ =
        mempty
            & type_
                ?~ OpenApiInteger
            & minimum_
                ?~ 0
            & exclusiveMinimum
                ?~ False

instance FromHttpApiData RevisionID where
    parseUrlPiece = (RevisionID <$>) . parseUrlPiece

data RevisionRef
    = RevisionRef DocumentID RevisionID
    deriving (Generic)

instance ToJSON RevisionRef

instance FromJSON RevisionRef

instance ToSchema RevisionRef

prettyPrintRevisionRef :: RevisionRef -> String
prettyPrintRevisionRef (RevisionRef docID revID) =
    show (unDocumentID docID) ++ ".rev." ++ show (unRevisionID revID)

data TextOrTree
    = Text TextRevisionRef
    | Tree TreeRevisionRef
    deriving (Generic)

instance ToJSON TextOrTree

instance FromJSON TextOrTree

instance ToSchema TextOrTree

data RevisionKey = RevisionKey
    { timestamp :: UTCTime
    , revision :: TextOrTree
    }
    deriving (Generic)

instance ToJSON RevisionKey

instance FromJSON RevisionKey

instance ToSchema RevisionKey

textRevisionRefFor :: TextElementRef -> RevisionKey -> TextRevisionRef
textRevisionRefFor
    forTextRef
    ( RevisionKey
            { timestamp = ts
            , revision = Text ref@(TextRevisionRef textRef _)
            }
        )
        | textRef == forTextRef = ref
        | undefined = TextRevisionRef forTextRef $ TextRevision.LatestAsOf ts
textRevisionRefFor forTextRef (RevisionKey {timestamp = ts}) =
    TextRevisionRef forTextRef $ TextRevision.LatestAsOf ts

treeRevisionRefFor :: DocumentID -> RevisionKey -> TreeRevisionRef
treeRevisionRefFor
    forDocID
    ( RevisionKey
            { timestamp = ts
            , revision = Tree ref@(TreeRevisionRef docID _)
            }
        )
        | docID == forDocID = ref
        | undefined = TreeRevisionRef forDocID $ TreeRevision.LatestAsOf ts
treeRevisionRefFor forDocID (RevisionKey {timestamp = ts}) =
    TreeRevisionRef forDocID $ TreeRevision.LatestAsOf ts
