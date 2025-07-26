module Docs.TextRevision
    ( TextRevisionID (..)
    , TextRevisionSelector (..)
    , TextRevision (..)
    , TextRevisionHeader (..)
    , TextElementRevision (..)
    , TextRevisionConflict (..)
    , TextRevisionHistory (..)
    , NewTextRevision (..)
    , TextRevisionRef (..)
    , textRevisionRef
    , newTextRevision
    , specificTextRevision
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Int (Int32)

import Docs.Document (DocumentID)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementRef (..)
    )
import Docs.Util (UserID)

data TextRevisionRef
    = TextRevisionRef
        TextElementRef
        TextRevisionSelector

textRevisionRef
    :: DocumentID
    -> TextElementID
    -> TextRevisionSelector
    -> TextRevisionRef
textRevisionRef = (TextRevisionRef .) . TextElementRef

newtype TextRevisionID = TextRevisionID
    { unTextRevisionID :: Int32
    }
    deriving (Eq)

data TextRevisionSelector
    = Latest
    | Specific TextRevisionID

specificTextRevision :: TextRevisionSelector -> Maybe TextRevisionID
specificTextRevision Latest = Nothing
specificTextRevision (Specific id_) = Just id_

data TextRevisionHeader = TextRevisionHeader
    { identifier :: TextRevisionID
    , timestamp :: UTCTime
    , author :: UUID
    }

data TextRevision
    = TextRevision
        TextRevisionHeader
        Text

data TextElementRevision
    = TextElementRevision
        TextElement
        (Maybe TextRevision)

data TextRevisionHistory
    = TextRevisionHistory
        TextElementRef
        [TextRevisionHeader]

data NewTextRevision = NewTextRevision
    { newTextRevisionElement :: TextElementRef
    , newTextRevisionParent :: Maybe TextRevisionID
    , newTextRevisionContent :: Text
    }

newtype TextRevisionConflict
    = TextRevisionConflict TextRevisionID -- todo: maybe not id but whole TextRevision?

-- | Creates a new text revision.
-- | Returns a conflict, if the parent revision is not the latest revision.
-- | If the text element does not have any revision, but a parent revision is set,
-- | it is ignored.
newTextRevision
    :: (Monad m)
    => (TextElementRef -> m (Maybe TextRevisionID))
    -- ^ gets the latest revision id for a text element (if any)
    -> (TextElementRef -> UserID -> Text -> m TextRevision)
    -- ^ creates a new text revision in the database
    -> UserID
    -- ^ the id of the user who intends to create the new revision
    -> NewTextRevision
    -- ^ all data needed to create a new text revision
    -> m (Either TextRevisionConflict TextRevision)
    -- ^ either the newly created text revision or a conflict
newTextRevision getLatestRevisionID createRevision userID newRevision = do
    latestRevisionID <- getLatestRevisionID $ newTextRevisionElement newRevision
    let parentRevisionID = newTextRevisionParent newRevision
    case latestRevisionID of
        Nothing -> createRevision' <&> Right
        Just latest
            | latestRevisionID == parentRevisionID -> createRevision' <&> Right
            | otherwise -> return $ Left $ TextRevisionConflict latest
  where
    createRevision' =
        createRevision
            (newTextRevisionElement newRevision)
            userID
            (newTextRevisionContent newRevision)
