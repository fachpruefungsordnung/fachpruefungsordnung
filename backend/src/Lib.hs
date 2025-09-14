{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    )
where

import Data.Maybe (fromMaybe)
import Data.UUID (fromString)
import Database (getConnection, migrate)
import Docs (logMessage)
import qualified Docs
import Docs.ExampleDoc (exampleTree)
import Docs.Hasql.Database (run, runTransaction)
import Logging.Logs (Severity (Info))
import qualified Logging.Scope as Scope
import Mail (testMail)
import Server

someFunc :: IO ()
someFunc = do
    Right connection <- getConnection
    Right _ <- migrate connection
    Right _ <-
        flip run connection $
            logMessage Info Nothing Scope.server ("Starting Server..." :: String)
    -- Datenbank zumüllen :))
    testMail
    let userID = fromMaybe undefined $ fromString "7f59659a-9a46-4ba0-a911-09698107a6ea"
    let groupID = 1
    let title = "Test Document"
    Right (Right _) <-
        flip
            runTransaction
            connection
            $ Docs.newDefaultDocument
                userID
                groupID
                title
                exampleTree
    runServer
    return ()
