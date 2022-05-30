module Db ( 
    migrationsUp, 
    withConnFromPool, 
    DBCreds(..),
    CacheCreds(..), 
    newConn, 
    makeDbConfig,
    makeCacheConfig,
    newCacheConnPool) where

import Database.PostgreSQL.Simple as PG
    ( Connection, close, connectPostgreSQL )
import Database.PostgreSQL.Simple.Migration
    ( runMigration,
      MigrationCommand(MigrationDirectory, MigrationCommands,
                       MigrationInitialization),
      MigrationContext(MigrationContext) )
import Database.Redis as R
    ( checkedConnect,
      defaultConnectInfo,
      ConnectInfo(connectHost, connectPort, connectAuth,
                  connectMaxConnections, connectDatabase),
      Connection,
      PortID(PortNumber) ) 
import Control.Monad ( (<=<) )
import Control.Applicative ()
import Data.ByteString.UTF8 as BSU ( fromString, ByteString )
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

newtype DBCreds = DBCreds {dsn :: String}

data CacheCreds = CacheCreds {
  host :: String,
  port :: String,
  password :: Maybe BSU.ByteString,
  connectDb :: Integer,
  maxConnections  :: Int
}

makeCacheConfig :: C.Config -> IO (Maybe CacheCreds)
makeCacheConfig conf = do
  host <- C.lookup conf "redis.host" :: IO (Maybe String)
  port <- C.lookup conf "redis.port" :: IO (Maybe String)
  password <- C.lookup conf "redis.password" :: IO (Maybe BSU.ByteString)
  db <- C.lookup conf "redis.db" :: IO (Maybe Integer)
  maxConn <- C.lookup conf "redis.maxConn" :: IO (Maybe Int)
  return $ CacheCreds <$> host
                      <*> port
                      <*> Just password
                      <*> db
                      <*> maxConn

newCacheConnPool :: CacheCreds -> IO R.Connection
newCacheConnPool conf = do
  let redisConnInfo = defaultConnectInfo { 
    connectHost = host conf, 
    connectPort = PortNumber $ read $ port conf, 
    connectAuth = password conf,
    connectMaxConnections = maxConnections conf,
    connectDatabase = connectDb conf}
  checkedConnect redisConnInfo

makeDbConfig :: C.Config -> IO (Maybe DBCreds)
makeDbConfig conf = do
  dsn <- C.lookup conf "pg.dsn" :: IO (Maybe String)
  return $ DBCreds <$> dsn

newConn :: DBCreds -> IO PG.Connection
newConn creds = connectPostgreSQL $ BSU.fromString $ dsn creds

migrationsUp :: PG.Connection -> IO ()
migrationsUp conn = do
    let ctx = MigrationContext (MigrationCommands [MigrationInitialization, MigrationDirectory "./sql"]) True conn
    runMigration ctx
    pure ()

withConnFromPool :: Pool PG.Connection -> (PG.Connection -> IO ()) -> IO ()
withConnFromPool = withResource
