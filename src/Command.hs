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
import Db (withConnFromPool)
import Data.Maybe ( fromJust )
import Data.Pool(Pool, createPool, withResource)

printSelectedDataOrError :: Show a => Maybe a -> IO ()
printSelectedDataOrError Nothing = putStr "record does not exist"
printSelectedDataOrError a = putStr (show (fromJust a))

promptAndAddUser :: Pool Connection -> IO ()
promptAndAddUser pool = 
    withConnFromPool pool $
        \conn -> do
            putStrLn "Enter user email:"
            userEmail <- getLine
            addUser conn userEmail
            user <- selectUser conn userEmail
            printSelectedDataOrError user

promptAndAddUrl :: Pool Connection -> IO ()
promptAndAddUrl pool =
    withConnFromPool pool $
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

promptAndGetUrl :: Pool Connection -> IO ()
promptAndGetUrl pool =
    withConnFromPool pool $
        \conn -> do
           putStrLn "Enter url hash:"
           urlHash <- getLine
           url <- selectUrl conn urlHash
           printSelectedDataOrError url

promptAndGetUserUrls :: Pool Connection -> IO ()
promptAndGetUserUrls pool =
    withConnFromPool pool $
        \conn -> do
           putStrLn "Enter user email:"
           email <- getLine
           urls  <- getUserUrls conn email
           mapM_ print urls

promptAndDeleteUser :: Pool Connection -> IO ()
promptAndDeleteUser pool =
    withConnFromPool pool $
        \conn -> do
            putStrLn "Enter user email:"
            email <- getLine
            deleteUser conn email

promptAndDeleteUrl :: Pool Connection -> IO ()
promptAndDeleteUrl pool =
    withConnFromPool pool $
        \conn -> do
            putStrLn "Enter url hash:"
            hash <- getLine
            deleteUrl conn hash
