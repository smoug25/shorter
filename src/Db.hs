module Db (conn, migrationsUp, withConn) where 

import Database.PostgreSQL.Simple
    ( Connection, close, connectPostgreSQL )
import Database.PostgreSQL.Simple.Migration
    ( runMigration,
      MigrationCommand(MigrationDirectory, MigrationCommands,
                       MigrationInitialization),
      MigrationContext(MigrationContext) )
import Control.Monad ( (<=<) )
import Control.Applicative ()
import Env ( (<=<), parse, header, help, nonempty, str, var )
import Data.ByteString.Lazy.UTF8 as BLU ( ByteString, fromString )
import Data.ByteString.Lazy as BL ( toStrict, ByteString )


dbCreds :: IO ByteString
dbCreds = do
    envVar <- Env.parse (header "Postgres creds") $
            var (str <=< nonempty) "PG_DSN"  (help "Postgres dsn string postgresql://user:secret@localhost:5432/dbname")
    let dsn = BLU.fromString envVar
    pure dsn       
              
conn :: IO Connection
conn = do
    creds <- dbCreds
    connectPostgreSQL $ BL.toStrict creds

migrationsUp :: Connection -> IO ()
migrationsUp conn = do
    let ctx = MigrationContext (MigrationCommands [MigrationInitialization, MigrationDirectory "./sql"]) True conn
    runMigration ctx
    pure ()

withConn :: (Connection -> IO ()) -> IO ()
withConn action = do
    creds <- dbCreds
    conn  <- connectPostgreSQL $ BL.toStrict creds
    action conn
    close conn

