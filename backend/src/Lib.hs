module Lib
    ( someFunc
    )
where

import Database (getConnection, migrate)
import Docs (logMessage)
import Docs.Hasql.Database (run)
import Docs.TestDoc (createTestDocument)
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
            logMessage Info Nothing Scope.server "Starting Server..."
    -- Datenbank zumüllen :))
    testMail
    createTestDocument connection
    runServer
    return ()
