module Database (getConnection, getPool, migrate) where

import Data.ByteString.Char8 (pack)
import Data.Functor
import qualified Hasql.Connection as Conn
import qualified Hasql.Connection.Setting.Connection as Connection
import Hasql.Migration
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Session
import Hasql.Transaction.Sessions
import System.Environment

migrate
    :: Conn.Connection -> IO (Either Session.SessionError [Maybe MigrationError])
migrate conn = do
    path <- getEnv "MIGRATIONS_DIR"
    migrations <- loadMigrationsFromDirectory path
    let tx = mapM runMigration (MigrationInitialization : migrations)
    let session = transaction Serializable Write tx
    Session.run session conn

getPool :: IO Pool.Pool
getPool = do
    s <- envSettings
    Pool.acquire $ PoolConfig.settings [PoolConfig.staticConnectionSettings s]

getConnection :: IO (Either Conn.ConnectionError Conn.Connection)
getConnection = envSettings >>= Conn.acquire

envSettings :: IO Conn.Setting.Connection
envSettings = do
    host <- getEnv "POSTGRES_HOST" <&> pack
    port <- getEnv "POSTGRES_PORT" <&> read
    user <- getEnv "POSTGRES_USER" <&> pack
    password <- getEnv "POSTGRES_PASSWORD" <&> pack
    database <- getEnv "POSTGRES_DB" <&> pack
    return $
        Conn.Setting.connection
            ( Connection.params
                [ Connection.Param.host host
                , Connection.Param.port port
                , Connection.Param.user user
                , Connection.Param.password password
                , Connection.Param.dbname database
                ]
            )
