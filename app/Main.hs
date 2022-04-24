
module Main where

import Command as C
import Db

performCommand :: String -> IO ()
performCommand "upDb"        = withConn migrationsUp          >> main
performCommand "addUser"     = C.promptAndAddUser             >> main
performCommand "addUrl"      = C.promptAndAddUrl              >> main
performCommand "getUserUrls" = C.promptAndGetUserUrls         >> main
performCommand "getUrl"      = C.promptAndGetUrl              >> main
performCommand "deleteUser"  = C.promptAndDeleteUser          >> main
performCommand "deleteUrl"   = C.promptAndDeleteUrl           >> main
performCommand "quit"        = putStrLn "Bay!"
performCommand _             = putStrLn "Commant isn't found" >> main 

main :: IO ()
main = do
    putStrLn "Enter comand:"
    command <- getLine
    performCommand command
