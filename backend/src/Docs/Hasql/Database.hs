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
import qualified UserManagement.Transactions as UserTransactions

import qualified Docs.Hasql.Sessions as Sessions
import qualified Docs.Hasql.Transactions as Transactions
import Logging.Logs (Severity (..))
import qualified Logging.Scope as Scope

newtype HasqlSession a
    = HasqlSession
    { unHasqlSession :: Session a
    }
    deriving (Functor, Applicative, Monad)

run :: HasqlSession a -> Connection -> IO (Either SessionError a)
run session conn = do
    result <- runUnlogged session conn
    case result of
        Left err -> do
            -- If something went wrong here, its fucked up anyway :)
            -- We could cache the result and insert it into the db as soon as the
            -- problem is solved, but i think this is currently not required.
            Right _ <-
                flip runUnlogged conn $
                    logMessage Error Nothing Scope.database $
                        show err
            return result
        Right _ -> return result
  where
    runUnlogged = Session.run . unHasqlSession

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

instance HasGetComments HasqlSession where
    getComments = HasqlSession . Sessions.getComments

instance HasGetLogs HasqlSession where
    getLogs = (HasqlSession .) . Sessions.getLogs

instance HasGetRevisionKey HasqlSession where
    getRevisionKey = HasqlSession . Sessions.getRevisionKey

-- create

instance HasCreateDocument HasqlSession where
    createDocument = ((HasqlSession .) .) . Sessions.createDocument

instance HasCreateTextElement HasqlSession where
    createTextElement = (HasqlSession .) . Sessions.createTextElement

instance HasLogMessage HasqlSession where
    logMessage = (((HasqlSession .) .) .) . Sessions.logMessage

newtype HasqlTransaction a
    = HasqlTransaction
    { unHasqlTransaction :: Transaction a
    }
    deriving (Functor, Applicative, Monad)

runTransaction :: HasqlTransaction a -> Connection -> IO (Either SessionError a)
runTransaction tx conn = do
    result <- runUnlogged tx conn
    case result of
        Left err -> do
            -- If something went wrong here, its fucked up anyway :)
            -- We could cache the result and insert it into the db as soon as the
            -- problem is solved, but i think this is currently not required.
            Right _ <-
                flip runUnlogged conn $
                    logMessage Error Nothing Scope.database $
                        show err
            return result
        Right _ -> return result
  where
    runUnlogged =
        (Session.run . transaction Serializable Write)
            . unHasqlTransaction

-- access rights

instance HasCheckPermission HasqlTransaction where
    checkDocumentPermission = ((HasqlTransaction .) .) . Transactions.hasPermission

instance HasIsGroupAdmin HasqlTransaction where
    isGroupAdmin = (HasqlTransaction .) . Transactions.isGroupAdmin

instance HasIsSuperAdmin HasqlTransaction where
    isSuperAdmin = HasqlTransaction . UserTransactions.checkSuperadmin

instance HasNow HasqlTransaction where
    now = HasqlTransaction Transactions.now

-- exists

instance HasExistsDocument HasqlTransaction where
    existsDocument = HasqlTransaction . Transactions.existsDocument

instance HasExistsTextElement HasqlTransaction where
    existsTextElement = HasqlTransaction . Transactions.existsTextElement

instance HasExistsTextRevision HasqlTransaction where
    existsTextRevision = HasqlTransaction . Transactions.existsTextRevision

instance HasExistsComment HasqlTransaction where
    existsComment = HasqlTransaction . Transactions.existsComment

-- get

instance HasGetTextElementRevision HasqlTransaction where
    getTextElementRevision = HasqlTransaction . Transactions.getTextElementRevision

instance HasGetTextElement HasqlTransaction where
    getTextElement = HasqlTransaction . Transactions.getTextElement

-- create

instance HasCreateTextRevision HasqlTransaction where
    updateTextRevision = ((HasqlTransaction .) .) . Transactions.updateTextRevision
    createTextRevision = (((HasqlTransaction .) .) .) . Transactions.createTextRevision
    getLatestTextRevisionID = HasqlTransaction . Transactions.getLatestTextRevisionID
    updateLatestTitle = (HasqlTransaction .) . Transactions.updateLatestTitle

instance HasCreateTreeRevision HasqlTransaction where
    createTreeRevision = ((HasqlTransaction .) .) . Transactions.createTreeRevision
    existsTextElementInDocument =
        HasqlTransaction . Transactions.isTextElementInDocument

instance HasCreateComment HasqlTransaction where
    createComment = ((HasqlTransaction .) .) . Transactions.createComment
    resolveComment = HasqlTransaction . Transactions.resolveComment
    createReply = ((HasqlTransaction .) .) . Transactions.createReply

instance HasDraftTextRevision HasqlTransaction where
    createDraftTextRevision = ((((HasqlTransaction .) .) .) .) . Transactions.createDraftTextRevision
    getDraftTextRevision = (HasqlTransaction .) . Transactions.getDraftTextRevision
    deleteDraftTextRevision = (HasqlTransaction .) . Transactions.deleteDraftTextRevision

instance HasLogMessage HasqlTransaction where
    logMessage = (((HasqlTransaction .) .) .) . Transactions.logMessage

instance HasCreateDocument HasqlTransaction where
    createDocument = ((HasqlTransaction .) .) . Transactions.createDocument

instance HasCreateTextElement HasqlTransaction where
    createTextElement = (HasqlTransaction .) . Transactions.createTextElement
