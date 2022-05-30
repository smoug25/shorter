module Storage
  ( addUser,
    selectUser,
    deleteUser,
    addUrl,
    selectUrl,
    deleteUrl,
    getHash,
    getUserUrls,
    updateUserOrCreateTable,
    User (..),
    Url (..),
    UrlUserId,
    UrlOrigin(..),
    UserEmail,
    UrlHash,
  )
where

import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Aeson
import Data.ByteString.UTF8 as BSU (fromString, toString)
import Data.Time (Day, UTCTime (utctDay), addDays, getCurrentTime)
import Data.Time.Format.ISO8601 (durationDaysFormat)
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    Only (Only),
    execute,
    query,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Db

type UserName = String

type UserEmail = String

type UrlHash = String

type UrlOrigin = String

type UrlUserId = Int

data User = User
  { userId :: Int,
    email :: UserEmail,
    createDate :: Day,
    lastLogin :: Day
  }

data Url = Url
  { urlId :: Int,
    urlHash :: UrlHash,
    urlOrigin :: UrlOrigin,
    private :: Bool,
    urlUserId :: UrlUserId,
    creationDate :: Day,
    expirationDate :: Day
  }

instance Show User where
  show user =
    mconcat
      [ show $ userId user,
        ".) ",
        email user,
        "\nlast login: ",
        show $ lastLogin user,
        "\n"
      ]

instance Show Url where
  show url =
    mconcat
      [ show $ urlId url,
        ".) ",
        "\nhash: ",
        urlHash url,
        "\norigin url: ",
        urlOrigin url,
        "\nprivate: ",
        show $ private url,
        "\ncreation date: ",
        show $ creationDate url,
        "\nexpiration date: ",
        show $ expirationDate url,
        "\nuser id: ",
        show $ urlUserId url,
        "\n"
      ]

instance FromRow User where
  fromRow =
    User <$> field
      <*> field
      <*> field
      <*> field

instance FromRow Url where
  fromRow =
    Url <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance FromJSON Url where
  parseJSON = withObject "url" $ \v -> Url <$> v .: "urlId" 
                                               <*> v .: "urlHash" 
                                               <*> v .: "urlOrigin"
                                               <*> v .: "privat"
                                               <*> v .: "urlUserId"
                                               <*> v .: "creationDate"
                                               <*> v .: "expirationDate"

instance ToJSON Url where
  toJSON (Url urlId urlHash urlOrigin privat urlUserId creationDate expirationDate) =
    object
      [ "urlId"          .= urlId,
        "urlHash"        .= urlHash,
        "urlOrigin"      .= urlOrigin,
        "privat"         .= privat,
        "urlUserId"      .= urlUserId,
        "creationDate"   .= creationDate,
        "expirationDate" .= expirationDate
      ]

urlExpTimeAddition :: Integer
urlExpTimeAddition = 7

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : _) = Just x

addUser :: Connection -> UserEmail -> IO ()
addUser conn userEmail = do
  execute conn "INSERT INTO users (email) VALUES (?)" (Only userEmail)
  pure ()

selectUser :: Connection -> UserEmail -> IO (Maybe User)
selectUser conn userEmail = do
  resp <- query conn "SELECT * FROM users WHERE email = ?" (Only userEmail) :: IO [User]
  return $ firstOrNothing resp

updateUser :: User -> Day -> User
updateUser user date = user {lastLogin = date}

updateUserOrWarn :: Connection -> Maybe User -> IO ()
updateUserOrWarn _ Nothing = Prelude.putStrLn "id is not found"
updateUserOrWarn conn (Just user) = do
  let q =
        mconcat
          [ "UPDATE users SET ",
            "last_login = ?",
            "WHERE user_id = ?;"
          ]
  execute conn q (lastLogin user, userId user)
  pure ()

updateUserTable :: Connection -> UserEmail -> IO ()
updateUserTable conn userEmail = do
  user <- selectUser conn userEmail
  currentDay <- utctDay <$> getCurrentTime
  let updatedUser = updateUser <$> user <*> pure currentDay
  updateUserOrWarn conn updatedUser

updateUserOrCreateTable :: Connection -> Maybe User -> UserEmail -> IO ()
updateUserOrCreateTable conn Nothing userEmail = do addUser conn userEmail
updateUserOrCreateTable conn (Just u) userEmail = do
  currentDay <- utctDay <$> getCurrentTime
  let updatedUser = Just $ updateUser u currentDay
  updateUserOrWarn conn updatedUser

deleteUser :: Connection -> UserEmail -> IO ()
deleteUser conn userEmail = do
  execute conn "DELETE FROM users WHERE email =  ?" (Only userEmail)
  pure ()

addUrl :: Connection -> UrlOrigin -> UrlUserId -> Bool -> Day -> IO ()
addUrl conn url user private exp = do
  execute
    conn
    "INSERT INTO urls (origin_url, hash, private, user_id, expiration_date) VALUES (?, ?, ?, ?, ?)"
    (url, getHash url user private, private, user, exp)
  pure ()

getHash :: UrlOrigin -> UrlUserId -> Bool -> UrlHash
getHash url user private =
  let bs = BSU.fromString $ mconcat [url, show user, show private]
   in take 10 $ show (hash bs :: Digest SHA3_512)

selectUrl :: Connection -> UrlHash -> IO (Maybe Url)
selectUrl conn uhash = do
  resp <- query conn "SELECT * FROM urls WHERE hash = ?" (Only uhash) :: IO [Url]
  return $ firstOrNothing resp

updateUrl :: Url -> Day -> Url
updateUrl url date = url {expirationDate = date}

updateUrlOrWarn :: Connection -> Maybe Url -> IO ()
updateUrlOrWarn _ Nothing = Prelude.putStrLn "hash is not found"
updateUrlOrWarn conn (Just url) = do
  let q =
        mconcat
          [ "UPDATE urls SET ",
            "expiration_date = ?",
            "WHERE url_id = ?;"
          ]
  execute conn q (expirationDate url, urlId url)
  pure ()

updateUrlTable :: Connection -> UrlHash -> IO ()
updateUrlTable conn urlHash = do
  url <- selectUrl conn urlHash
  currentDay <- utctDay <$> getCurrentTime
  let updatedUrl = updateUrl <$> url <*> pure (addDays urlExpTimeAddition currentDay)
  updateUrlOrWarn conn updatedUrl

deleteUrl :: Connection -> UrlHash -> IO ()
deleteUrl conn urlHash = do
  execute conn "DELETE FROM urls WHERE hash =  ?" (Only urlHash)
  pure ()

getUserUrls :: Connection -> UserEmail -> IO [Url]
getUserUrls conn userEmail =
  query
    conn
    "SELECT * FROM urls WHERE user_id = (SELECT user_id from users where email = ?)"
    (Only userEmail) ::
    IO [Url]