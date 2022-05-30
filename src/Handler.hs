module Handler (
    Email(..),
    AuthToken (..),
    auth,
    confirm,
    shorten,
    redirectToOrigin,
    makeSMTPConf,
    makeServerConfig,
    SMTPConf (..),
    ServerConf(..),
    SessionToken(..),
    Session(..)) where

import Control.Applicative
import Control.Monad.IO.Class
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Aeson
import Data.ByteString.UTF8 as BSU (ByteString, fromString, toString, fromChar)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool (Pool, createPool, withResource)
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Database.PostgreSQL.Simple as PG (Connection, close)
import Database.Redis as R (Connection, get, runRedis, setex)
import Network.HTTP.Types.Status
import Network.Mail.Mime as MI
import Network.Mail.SMTP as M
import System.Random
import Web.Scotty as S
import Web.Scotty.Cookie (getCookie, setSimpleCookie)
import Web.Scotty.Internal.Types (ActionT)
import Data.List.Split as SP
import Storage
import Db
import Data.Time
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BSL
import Cache

data SMTPConf = SMTPConf
  { hostSmtp :: String,
    loginSmtp :: String,
    passwordSmtp :: String,
    from :: String
  }

data ServerConf = ServerConf
  {
      domen :: String,
      port  :: Int
  }

makeSMTPConf :: C.Config -> IO (Maybe SMTPConf)
makeSMTPConf conf = do
  host <- C.lookup conf "smtp.host" :: IO (Maybe String)
  login <- C.lookup conf "smtp.login" :: IO (Maybe String)
  passwd <- C.lookup conf "smtp.password" :: IO (Maybe String)
  from <- C.lookup conf "smtp.from" :: IO (Maybe String)
  return $
    SMTPConf <$> host
      <*> login
      <*> passwd
      <*> from

makeServerConfig :: C.Config -> IO (Maybe ServerConf)
makeServerConfig conf = do
  domen <- C.lookup conf "server.domen" :: IO (Maybe String)
  port  <- C.lookup conf "server.port" :: IO (Maybe Int)
  return $ ServerConf <$> domen <*> port

newtype AuthToken = AuthToken {authToken :: String}
  deriving (Show)

newtype Email = Email {email :: String}
  deriving (Show)

newtype ConfirmCode = ConfirmCode {confirmCode :: String}
  deriving (Show)

newtype SessionToken = SessionToken {sessionToken :: String}
  deriving (Show)

data ShortenReq = ShortenReq {
    url :: UrlOrigin,
    allowEmails :: [Email],
    private :: Bool
} deriving (Show)

newtype ShortenRsp = ShortenRsp {
    shortUrl :: String
} deriving (Show)

data Session = Session {
    userEmail :: Email,
    authorised :: Bool,
    userId :: Int
} deriving (Show)

getSession :: ActionT TL.Text IO Session
getSession = do
    e <- header "x-session-email"
    a <- header "x-session-auth"
    id <-  header "x-session-user-id"
    pure $ Session {
        userEmail = parseEmail e,
        authorised = parseAuth a,
        Handler.userId = parseUserId id}
    where
        parseEmail Nothing = Email "default@short.me"
        parseEmail (Just t) = Email $ TL.unpack t

        parseAuth  Nothing = False
        parseAuth (Just t)  | TL.unpack t == "true" = True
                            | otherwise = False

        parseUserId Nothing = 0
        parseUserId (Just t)  = read $ TL.unpack t

instance FromJSON Session where
  parseJSON = withObject "session" $ \v -> Session <$> v .: "userEmail" <*> v .: "authorised" <*> v .: "userId"

instance ToJSON Session where
  toJSON (Session userEmail authorised userId) = object ["userEmail" .= userEmail, "authorised" .= authorised, "userId" .= userId]

instance FromJSON Email where
  parseJSON = withObject "email" $ \v -> Email
        <$> v .: "email"

instance ToJSON Email where
  toJSON (Email email) = object ["email" .= email]

instance FromJSON ConfirmCode where
  parseJSON = withObject "confirmCode" $ \v -> ConfirmCode <$> v .: "code"

instance ToJSON ConfirmCode where
  toJSON (ConfirmCode confirmCode) = object ["code" .= confirmCode]

instance FromJSON ShortenReq where
    parseJSON = withObject "shortenReq" $ \v -> ShortenReq <$> v .: "url"  <*> v .: "allowEmails" <*> v .: "private"

instance ToJSON ShortenReq where
    toJSON (ShortenReq url allowEmails private) =  object ["url" .= url, "allowEmails" .= allowEmails, "private" .= private]

instance FromJSON ShortenRsp where
  parseJSON = withObject "shortenRsp" $ \v -> ShortenRsp <$> v .: "shortUrl"

instance ToJSON ShortenRsp where
  toJSON (ShortenRsp shortUrl) = object ["shortUrl" .= shortUrl]

-- TODO implement email validation
validateEmail :: Email -> Bool
validateEmail e = True

-- TODO implement url validation
validateUrl :: UrlOrigin -> Bool
validateUrl u = True

-- Auth handler
auth :: R.Connection -> SMTPConf -> ActionM ()
auth pool smptConf = do
  email <- getEmailParam
  token <- sendCode email pool smptConf
  sendAuthToken token

getEmailParam :: ActionT TL.Text IO (Maybe Email)
getEmailParam = do
  b <- body
  return (decode b :: Maybe Email)
  where
    makeEmail s = ""

sendCode :: Maybe Email -> R.Connection -> SMTPConf -> ActionT TL.Text IO (Maybe AuthToken)
sendCode Nothing _ _ = pure Nothing
sendCode (Just (Email email)) conn conf = do
  code <- genCode
  sendEmail (Email email) code conf
  let token = genToken email code
  liftIO $ runRedis conn $ setex (BSU.fromString token) 300 (BSU.fromString $ mconcat [email, ":", code])
  pure $ Just $ AuthToken {authToken = token}

genCode :: ActionT TL.Text IO String
genCode = do
  g <- newStdGen
  pure . Prelude.take 4 $ randomRs ('0', '9') g

genToken :: String -> String -> String
genToken base sol =
  let bs = BSU.fromString $ mconcat [base, sol]
   in show (hash bs :: Digest SHA3_512)

sendEmail :: Email -> String -> SMTPConf -> ActionT TL.Text IO ()
sendEmail e code conf = do
  let fromAddr = Address Nothing $ T.pack (from conf)
      to = Address Nothing $ T.pack (Handler.email e)
      subject = "Shorter service email confirmation"
      body = MI.plainPart $ TL.pack ("Code for email confirm: " ++ code)
      html = MI.htmlPart $ TL.pack ("<h1>Code for email confirm: " ++ code ++ "</h1>")
      mail = M.simpleMail fromAddr [to] [] [] subject [body, html]
  --liftIO $ sendMailWithLogin (hostSmtp conf) (loginSmtp conf) (passwordSmtp conf) mail
  liftIO $ sendMail (hostSmtp conf) mail
  pure ()

sendAuthToken :: Maybe AuthToken -> ActionM ()
sendAuthToken Nothing = raise "Auth token is not definde"
sendAuthToken (Just (AuthToken authToken)) = do
  setSimpleCookie (T.pack "auth_token") (T.pack authToken)
  S.json ()

-- Auth handler end

-- Confirm handler

confirm :: R.Connection -> Pool PG.Connection -> ActionM ()
confirm rConn pgConn = do
  code <- getCodeParam
  token <- getAuthToken
  email <- checkEmail code token rConn
  sToken <- startSession email rConn pgConn
  sendSessionToken sToken

getCodeParam :: ActionT TL.Text IO (Maybe ConfirmCode)
getCodeParam = do
  b <- body
  return (decode b :: Maybe ConfirmCode)
  where
    makeConfirmCode s = ""

getAuthToken :: ActionT TL.Text IO (Maybe AuthToken)
getAuthToken = do
  token <- getCookie "auth_token"
  case token of
    Nothing -> pure Nothing
    Just t -> pure $ Just $ AuthToken {authToken = T.unpack t}

checkEmail :: Maybe ConfirmCode -> Maybe AuthToken -> R.Connection -> ActionT TL.Text IO (Maybe Email)
checkEmail Nothing _ _ = pure Nothing
checkEmail _ Nothing _ = pure Nothing
checkEmail (Just (ConfirmCode confirmCode)) (Just (AuthToken authToken)) conn = do
  res <- liftIO (runRedis conn $ R.get (BSU.fromString authToken))
  let res' = case res of
        Right b -> parseEmailFromResult b $ ConfirmCode confirmCode
        Left _ -> Nothing
  pure res'

parseEmailFromResult :: Maybe ByteString -> ConfirmCode -> Maybe Email
parseEmailFromResult Nothing _ = Nothing
parseEmailFromResult (Just b) code =
    let l = SP.splitOn ":" (BSU.toString b)
    in gerEmailFromList l code
    where
        gerEmailFromList :: [String] -> ConfirmCode -> Maybe Email
        gerEmailFromList l code
             | Prelude.null l = Nothing
             | Prelude.tail l == [confirmCode code] = Just $ Email $ Prelude.head l
             | otherwise = Nothing

startSession :: Maybe Email -> R.Connection -> Pool PG.Connection -> ActionT TL.Text IO (Maybe SessionToken)
startSession Nothing _ _ = pure Nothing
startSession (Just (Email email)) rConn pgConn = do
   user <- liftIO (withResource pgConn $ \conn -> do selectUser conn email)
   liftIO (withConnFromPool pgConn $ \conn -> do updateUserOrCreateTable conn user email)
   user <- liftIO (withResource pgConn $ \conn -> do selectUser conn email)
   timeStr <- liftIO $ show <$> getCurrentTime
   let token = genToken email timeStr
   liftIO $ runRedis rConn $ setex (BSU.fromString token) 2592000 (BSL.toStrict $ encode $ getSessionData user)
   pure $ Just (SessionToken token)

getSessionData :: Maybe User -> Maybe Session
getSessionData Nothing = Nothing
getSessionData (Just u) = Just Session {userEmail=Email{Handler.email= Storage.email u}, authorised=True, Handler.userId=Storage.userId u}

sendSessionToken :: Maybe SessionToken -> ActionM ()
sendSessionToken Nothing = status status401
sendSessionToken (Just (SessionToken sessionToken)) = do
  setSimpleCookie (T.pack "session_token") (T.pack sessionToken)
  S.json ()

-- Confirm handler end  

-- Shorten handler start

shorten :: R.Connection -> Pool PG.Connection -> String -> ActionM ()
shorten rConn pgConn domen = do
    session <- getSession
    req <- getShortenReqParam
    urlHash <- liftIO $ getUrlHash req session
    url <- liftIO $ checkUrlInCache rConn urlHash
    url <- liftIO $ checkUrlInDbAndAddInCacheIfNeeded rConn pgConn url urlHash
    url <- liftIO $ saveUrlIfNeeded rConn pgConn req session url
    sendShortenRsp url domen

getShortenReqParam :: ActionT TL.Text IO (Maybe ShortenReq)
getShortenReqParam = do
  b <- body
  return (decode b :: Maybe ShortenReq)
  where
    makeConfirmCode s = ""

getUrlHash :: Maybe ShortenReq -> Session -> IO (Maybe UrlHash)
getUrlHash Nothing _ = do pure Nothing
getUrlHash (Just (ShortenReq url allowEmails private)) s = do
    let userId = Handler.userId s
    pure $ Just $ getHash url userId private

checkUrlInCache :: R.Connection -> Maybe UrlHash -> IO (Maybe Url)
checkUrlInCache _ Nothing = do pure Nothing
checkUrlInCache conn (Just h) = do Cache.selectUrl conn h

checkUrlInDbAndAddInCacheIfNeeded :: R.Connection -> Pool PG.Connection -> Maybe Url -> Maybe UrlHash -> IO (Maybe Url)
checkUrlInDbAndAddInCacheIfNeeded _ _ (Just u) _ = do pure $ Just u
checkUrlInDbAndAddInCacheIfNeeded _ _ Nothing Nothing = do  pure Nothing
checkUrlInDbAndAddInCacheIfNeeded rConn pgConn Nothing (Just h) = do
    url <- withResource pgConn $ \conn -> do Storage.selectUrl conn h
    case url of
        Nothing -> pure Nothing
        Just u  -> do
            Cache.addUrl rConn u
            pure $ Just u

saveUrlIfNeeded :: R.Connection -> Pool PG.Connection -> Maybe ShortenReq -> Session -> Maybe Url -> IO (Maybe Url)
saveUrlIfNeeded _ _ Nothing _ Nothing = do pure Nothing
saveUrlIfNeeded _ _ _ _ (Just u) = do pure $ Just u
saveUrlIfNeeded rConn pgConn (Just (ShortenReq url allowEmails private)) s Nothing = do
    let userId = Handler.userId s
        hash = getHash url userId private
    currentDay <- utctDay <$> getCurrentTime
    withResource pgConn $ \conn -> Storage.addUrl conn url userId private (addDays 31 currentDay)
    checkUrlInDbAndAddInCacheIfNeeded rConn pgConn Nothing (Just hash)

sendShortenRsp :: Maybe Url -> String -> ActionM ()
sendShortenRsp Nothing _ = status status500
sendShortenRsp (Just u) domen = do
    S.json ShortenRsp {shortUrl=  domen ++ "/" ++  urlHash  u}

-- Shorten handler end

-- RedirectToOrigin handler start

redirectToOrigin :: R.Connection -> Pool PG.Connection -> ActionM ()
redirectToOrigin rConn pgConn = do
    session <- getSession
    hash <- S.param "1" :: ActionT TL.Text IO UrlHash
    url <- liftIO $ checkUrlInCache rConn (Just hash)
    url <- liftIO $ checkUrlInDbAndAddInCacheIfNeeded rConn pgConn Nothing (Just hash)
    case url of
        Nothing -> status status404
        Just u -> do
            if isAllowed u session
            then
                do    
                    status status301
                    setHeader (TL.pack "Location") $ TL.pack $ urlOrigin u
            else
                status status401    
            S.json ()

isAllowed :: Url -> Session -> Bool
isAllowed u s | not (Storage.private u) = True
              | urlUserId u == Handler.userId s = True
              | otherwise = False

-- RedirectToOrigin handler end