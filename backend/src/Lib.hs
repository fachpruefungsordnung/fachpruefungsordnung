{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    )
where

import Database (getConnection, migrate)
import Hasql.Connection (Connection)
import Hasql.Session (statement)
import qualified Hasql.Session as Session
import Server
import qualified UserManagement.Statements as UStatements
import VersionControl as VC
import VersionControl.Commit
import VersionControl.Tree

-- a good example document with example content and example structure
testTree :: Tree NodeWithMaybeRef
testTree =
    mkTree
        (Node "document" (Just "Hier könnten ihre Metadaten stehen."))
        [ mkEdge
            "Abschnitt 1"
            (mkTree (Node "abschnitt" (Just "Das hier ist ein Abschnitt.")) [])
        , mkEdge
            "Abschnitt 2"
            (mkTree (Node "abschnitt" (Just "Das hier ist noch ein Abschnitt.")) [])
        , mkEdge
            "Abschnitt 3"
            (mkTree (Node "abschnitt" (Just "Auch das hier ist ein Abschnitt.")) [])
        , mkEdge
            "Abschnitt 4"
            (mkTree (Node "abschnitt" (Just "Und noch ein Abschnitt.")) [])
        , mkEdge
            "Anlagen"
            ( mkTree
                (Node "anlagen" Nothing)
                [ mkEdge
                    "Anlage 1"
                    (mkTree (Node "anlage" (Just "Das hier ist eine Anlage.")) [])
                , mkEdge
                    "Anlage 2"
                    (mkTree (Node "anlage" (Just "Das hier ist auch eine Anlage.")) [])
                , mkEdge
                    "Anlage 3"
                    (mkTree (Node "anlage" (Just "Guck mal! Noch eine Anlage!")) [])
                , mkEdge
                    "Anlage 4"
                    (mkTree (Node "anlage" (Just "Guck mal! Ne vierte Anlage!")) [])
                ]
            )
        ]

testCommits :: Connection -> IO ExistingCommit
testCommits conn = do
    Right userId <- getUserID "test@test.com" conn
    let commit =
            CreateCommit
                (CommitInfo userId (Just "Test Commit") Nothing)
                (Value testTree)
    Right newCommit <- VC.createCommit commit $ VC.Context conn
    return newCommit
  where
    getUserID email = Session.run (statement email UStatements.getUserID)

someFunc :: IO ()
someFunc = do
    Right connection <- getConnection
    Right _ <- migrate connection
    -- Datenbank zumüllen :)
    commit <- testCommits connection
    print commit
    runServer
    return ()
