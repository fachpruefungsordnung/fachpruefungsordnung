module Lib
    ( someFunc
    )
where

import Database (getConnection, migrate)
import Docs.TestDoc (createTestDocument)
import Server

someFunc :: IO ()
someFunc = do
    Right connection <- getConnection
    Right _ <- migrate connection
    -- Datenbank zumüllen :))
    createTestDocument connection
    runServer
    return ()
