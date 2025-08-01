{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Docs.Hasql.Database
    ( HasqlSession (..)
    , HasqlTransaction (..)
    , run
    , runTransaction
    ) where

import Hasql.Connection (Connection)
import Hasql.Session (Session, SessionError)
import qualified Hasql.Session as Session
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )

import Docs.Database

import qualified UserManagement.Sessions as UserSessions

import qualified Docs.Hasql.Sessions as Sessions
import qualified Docs.Hasql.Transactions as Transactions

newtype HasqlSession a
    = HasqlSession
    { unHasqlSession :: Session a
    }
    deriving (Functor, Applicative, Monad)

run :: HasqlSession a -> Connection -> IO (Either SessionError a)
run = Session.run . unHasqlSession

-- access rights

instance HasCheckPermission HasqlSession where
    checkDocumentPermission = ((HasqlSession .) .) . Sessions.hasPermission

instance HasIsGroupAdmin HasqlSession where
    isGroupAdmin = (HasqlSession .) . Sessions.isGroupAdmin

instance HasIsSuperAdmin HasqlSession where
    isSuperAdmin = HasqlSession . UserSessions.checkSuperadmin

-- exists

instance HasExistsDocument HasqlSession where
    existsDocument = HasqlSession . Sessions.existsDocument

instance HasExistsTextElement HasqlSession where
    existsTextElement = HasqlSession . Sessions.existsTextElement

instance HasExistsTextRevision HasqlSession where
    existsTextRevision = HasqlSession . Sessions.existsTextRevision

instance HasExistsTreeRevision HasqlSession where
    existsTreeRevision = HasqlSession . Sessions.existsTreeRevision

-- get

instance HasGetDocument HasqlSession where
    getDocument = HasqlSession . Sessions.getDocument
    getDocuments = HasqlSession . Sessions.getDocuments
    getDocumentsBy = (HasqlSession .) . Sessions.getDocumentsBy

instance HasGetTreeRevision HasqlSession where
    getTreeRevision = HasqlSession . Sessions.getTreeRevision

instance HasGetTextElementRevision HasqlSession where
    getTextElementRevision = HasqlSession . Sessions.getTextElementRevision

instance HasGetTextHistory HasqlSession where
    getTextHistory = ((HasqlSession .) .) . Sessions.getTextRevisionHistory

instance HasGetTreeHistory HasqlSession where
    getTreeHistory = ((HasqlSession .) .) . Sessions.getTreeRevisionHistory

instance HasGetDocumentHistory HasqlSession where
    getDocumentHistory = ((HasqlSession .) .) . Sessions.getDocumentRevisionHistory

-- create

instance HasCreateDocument HasqlSession where
    createDocument = ((HasqlSession .) .) . Sessions.createDocument

instance HasCreateTextElement HasqlSession where
    createTextElement = (HasqlSession .) . Sessions.createTextElement

newtype HasqlTransaction a
    = HasqlTransaction
    { unHasqlTransaction :: Transaction a
    }
    deriving (Functor, Applicative, Monad)

runTransaction :: HasqlTransaction a -> Connection -> IO (Either SessionError a)
runTransaction = (Session.run . transaction Serializable Write) . unHasqlTransaction

-- access rights

instance HasCheckPermission HasqlTransaction where
    checkDocumentPermission = ((HasqlTransaction .) .) . Transactions.hasPermission

instance HasIsGroupAdmin HasqlTransaction where
    isGroupAdmin = (HasqlTransaction .) . Transactions.isGroupAdmin

-- exists

instance HasExistsDocument HasqlTransaction where
    existsDocument = HasqlTransaction . Transactions.existsDocument

instance HasExistsTextElement HasqlTransaction where
    existsTextElement = HasqlTransaction . Transactions.existsTextElement

-- create

instance HasCreateTextRevision HasqlTransaction where
    createTextRevision = ((HasqlTransaction .) .) . Transactions.createTextRevision
    getLatestTextRevisionID = HasqlTransaction . Transactions.getLatestTextRevisionID

instance HasCreateTreeRevision HasqlTransaction where
    createTreeRevision = ((HasqlTransaction .) .) . Transactions.createTreeRevision
    existsTextElementInDocument =
        HasqlTransaction . Transactions.isTextElementInDocument
