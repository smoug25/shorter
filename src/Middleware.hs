module Middleware (loadSession) where

import Control.Arrow (Arrow (second))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bifunctor
import Data.ByteString (ByteString, putStrLn)
import Data.ByteString.UTF8 as BSU (fromString, toString)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack, unpack)
import Database.Redis as R (Connection, get, runRedis)
import Handler (Email (..), SessionToken (SessionToken), Session(..))
import Network.HTTP.Types as H (Header, RequestHeaders, hCookie)
import Network.Wai (Middleware, Request (requestHeaders))
import Web.Cookie (parseCookiesText)
import Web.Scotty as S ()
import Web.Scotty.Cookie ()
import Web.Scotty.Internal.Types (ActionT)
import qualified Data.List.Split as SP
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

loadSession :: R.Connection -> Middleware
loadSession conn app req sendResponse = do
  let token = getSessionToken $ fromMaybe "" $ lookup H.hCookie $ requestHeaders req
  sessionData <- getSessionData token conn
  newReq <- addHeaders (prepareSessinHeaders sessionData) req
  app newReq sendResponse

getCookieVal :: ByteString -> String -> Maybe T.Text
getCookieVal b m = Map.lookup (T.pack m) $ Map.fromList . maybe [] parse $ Just b
  where
    parse = parseCookiesText

getSessionToken :: ByteString -> Maybe SessionToken
getSessionToken b =
  let c = getCookieVal b "session_token"
   in case c of
        Nothing -> Nothing
        Just t -> Just $ SessionToken $ T.unpack t

getSessionData :: Maybe SessionToken -> R.Connection -> IO (Maybe Session)
getSessionData Nothing _ = pure Nothing
getSessionData (Just (SessionToken sessionToken)) conn = do
  res <- liftIO (runRedis conn $ R.get (BSU.fromString sessionToken))
  let res' = case res of
        Right b -> parseSessionData b
        Left _ -> Nothing     
  pure res'

parseSessionData :: Maybe ByteString -> Maybe Session
parseSessionData Nothing = Nothing
parseSessionData (Just b) =
    decode (BSL.fromStrict b) :: Maybe Session
    where
        makeSession s = ""


prepareSessinHeaders :: Maybe Session -> [(ByteString, ByteString)]
prepareSessinHeaders Nothing = [("x-session-email", "default@short.me"), ("x-session-auth", "False"), ("x-session-user-id", "0")]
prepareSessinHeaders (Just (Session userEmail authorised userId))
  | email userEmail == "default@short.me" = [("x-session-email", "default@short.me"), ("x-session-auth", "False"), ("x-session-user-id", "0")]
  | email userEmail == "" = [("x-session-email", "default@short.me"), ("x-session-auth", "False"), ("x-session-user-id", "0")]
  | otherwise = [("x-session-email", BSU.fromString $ email userEmail), ("x-session-auth", "True"), ("x-session-user-id", BSU.fromString $ show userId)]

addHeaders :: [(ByteString, ByteString)] -> Request -> IO Request
addHeaders h req = do
  let headers = appendHeaders h $ requestHeaders req
  pure req {requestHeaders = headers}

appendHeaders :: [(ByteString, ByteString)] -> RequestHeaders -> RequestHeaders
appendHeaders xs h = Prelude.foldr ((:) . makeHeader) h xs

makeHeader :: (ByteString, ByteString) -> Header
makeHeader = Data.Bifunctor.first CI.mk
