module Cache (Cache.selectUrl, Cache.addUrl, Cache.deleteUrl) where

import qualified Data.ByteString.UTF8 as BSU
import Database.Redis as R (Connection, get, runRedis, setex, del)
import Storage ( Url(..), UrlHash )
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

selectUrl :: R.Connection -> UrlHash -> IO (Maybe Url)
selectUrl conn uhash = do
  res <- runRedis conn $ R.get (BSU.fromString uhash)
  let res' = case res of
        Right b -> parseUrl b
        Left _ -> Nothing
  pure res'

parseUrl :: Maybe ByteString -> Maybe Url
parseUrl Nothing = Nothing
parseUrl (Just b) = decode (BSL.fromStrict b) :: Maybe Url

addUrl :: R.Connection -> Url -> IO ()
addUrl conn url = do
  runRedis conn $ setex (BSU.fromString $ urlHash url) 604800 (BSL.toStrict $ encode url)
  pure ()

deleteUrl :: R.Connection -> UrlHash -> IO ()
deleteUrl conn urlHash = do
  runRedis conn $ del [BSU.fromString urlHash]
  pure ()