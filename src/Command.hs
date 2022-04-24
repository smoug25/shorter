module Command where
import Database.PostgreSQL.Simple (Connection)
import Storage
    ( addUser,
      selectUser,
      deleteUser,
      addUrl,
      getHash,
      UrlUserId,
      selectUrl,
      deleteUrl,
      getUserUrls ) 
import Data.Time ( addDays, getCurrentTime, UTCTime(utctDay) )
import Db (withConn)
import Data.Maybe ( fromJust )

printSelectedDataOrError :: Show a => Maybe a -> IO ()
printSelectedDataOrError Nothing = putStr "record does not exist"
printSelectedDataOrError a = putStr (show (fromJust a))

promptAndAddUser :: IO ()
promptAndAddUser = 
    withConn $
        \conn -> do
            putStrLn "Enter user email:"
            userEmail <- getLine
            addUser conn userEmail
            user <- selectUser conn userEmail
            printSelectedDataOrError user

promptAndAddUrl :: IO ()
promptAndAddUrl =
    withConn $
        \conn -> do
            putStrLn "Enter url:"
            originUrl <- getLine
            putStrLn "Is url private? y/n:"
            privateStr <- getLine
            putStrLn "Enter user id:"
            userIdStr <- getLine
            currentDay <- utctDay <$> getCurrentTime
            addUrl conn originUrl (userIdFromPrompt userIdStr) (isPromptPrivate  privateStr) (addDays 31 currentDay) 
            url <- selectUrl conn (getHash originUrl (userIdFromPrompt userIdStr) (isPromptPrivate  privateStr))
            printSelectedDataOrError url

isPromptPrivate :: String -> Bool 
isPromptPrivate s 
    | s == "y" = True 
    | otherwise = False

userIdFromPrompt :: String -> UrlUserId
userIdFromPrompt str =
    let id = read str
    in if id > 999 
       then id 
       else 0

promptAndGetUrl :: IO ()
promptAndGetUrl =
    withConn $
        \conn -> do
           putStrLn "Enter url hash:"
           urlHash <- getLine
           url <- selectUrl conn urlHash
           printSelectedDataOrError url

promptAndGetUserUrls :: IO ()
promptAndGetUserUrls =
    withConn $
        \conn -> do
           putStrLn "Enter user email:"
           email <- getLine
           urls  <- getUserUrls conn email
           mapM_ print urls

promptAndDeleteUser :: IO ()
promptAndDeleteUser =
    withConn $
        \conn -> do
            putStrLn "Enter user email:"
            email <- getLine
            deleteUser conn email

promptAndDeleteUrl :: IO ()
promptAndDeleteUrl =
    withConn $
        \conn -> do
            putStrLn "Enter url hash:"
            hash <- getLine
            deleteUrl conn hash
