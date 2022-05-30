--{-# LANGUAGE OverloadedStrings #-}
module Main where

import Command as C
    ( promptAndDeleteUrl,
      promptAndDeleteUser,
      promptAndGetUrl,
      promptAndGetUserUrls,
      promptAndAddUrl,
      promptAndAddUser )
import Db ( migrationsUp, withConnFromPool, makeDbConfig, makeCacheConfig, newConn, newCacheConnPool, DBCreds(..), CacheCreds(..))
import Handler ( Email, AuthToken(..), auth, confirm, shorten, redirectToOrigin, SMTPConf(..), ServerConf(..), makeSMTPConf, makeServerConfig)

import Database.PostgreSQL.Simple as PG ( Connection, close )
import Data.Pool (Pool, createPool, withResource)
import Database.Redis as R (Connection)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Web.Scotty as S
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Cookie
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import qualified Data.Text.Lazy as TL
import Control.Applicative
import Control.Monad.IO.Class
import GHC.Conc (reportHeapOverflow)
import Data.Aeson
import Middleware
import System.Environment
import Control.Monad
import System.Exit

performCommand :: Pool PG.Connection -> String -> IO ()
performCommand p "addUser"     = C.promptAndAddUser     p
performCommand p "addUrl"      = C.promptAndAddUrl      p
performCommand p "getUserUrls" = C.promptAndGetUserUrls p
performCommand p "getUrl"      = C.promptAndGetUrl      p
performCommand p "deleteUser"  = C.promptAndDeleteUser  p
performCommand p "deleteUrl"   = C.promptAndDeleteUrl   p
performCommand _ _             = putStrLn "Commant isn't found"

data AppConf = AppConf {
  serverConf :: ServerConf,
  dbCreds :: DBCreds,
  cacheCreds :: CacheCreds,
  smtpConf :: SMTPConf
}

makeAppConf :: C.Config -> IO (Maybe AppConf)
makeAppConf conf = do
  serverConf <- makeServerConfig conf
  cacheConf <- makeCacheConfig conf
  dbConf <- makeDbConfig conf
  smtpConf <- makeSMTPConf conf
  return $ AppConf <$> serverConf <*> dbConf <*> cacheConf <*> smtpConf

main :: IO ()
main = do
    args <- getArgs
    loadedConf <- C.load [C.Required "app.conf"]
    appConf <- makeAppConf loadedConf

    case appConf of
      Nothing -> putStrLn "No app configuration found, terminating..."
      Just conf -> do
        pool <- createPool (Db.newConn $ dbCreds conf) close 1 64 10
        rPool <- newCacheConnPool $ cacheCreds conf
        case args of
          ["upDb"  ] -> withConnFromPool pool migrationsUp
          ["cli"   ] -> do
            forever $ do
              putStrLn "Enter comand:"
              command <- getLine
              if command == "quit"
              then do
                putStrLn "Bay!"
                exitSuccess
              else performCommand pool command
          ["server"] -> do
            scotty (Handler.port $ serverConf conf) $ do
              middleware logStdout             -- log all requests; for production use logStdout
              middleware $ loadSession rPool   -- save session data in headers

              post "/auth" $ auth rPool $ smtpConf conf
              post "/confirm" $ confirm rPool pool
              post "/shorten" $ shorten rPool pool (domen $ serverConf conf)

              get (regex "^/([a-zA-Z0-9]{10})$") $ redirectToOrigin rPool pool

          _other -> error $ mconcat ["\nUnrecognized command, please use: \n", 
                                     "upDb   - apply db migrations \n", 
                                     "cli    - start cli app \n",
                                     "server - start web server"]

