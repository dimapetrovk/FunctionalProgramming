module Main where

import System.IO
import Data.Map
import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Type
import ConsoleInterface

-- execAction ("Type", "create") conn = putStrLn $ show $ createTypeFromInput conn
-- execAction ("Type", "update") conn = putStrLn $ show $ updateTypeFromInput conn
-- execAction ("Type", "read") conn = putStrLn $ show $ getTypeFromInput conn
-- execAction ("Type", "list") conn = putStrLn $ show $ listTypeFromInput conn
--
-- execAction ("User", "create") conn = putStrLn $ show $ createUserFromInput conn
-- execAction ("User", "update") conn = putStrLn $ show $ updateUserFromInput conn
-- execAction ("User", "read") conn = putStrLn $ show $ getUserFromInput conn
-- execAction ("User", "list") conn = putStrLn $ show $ listUsersFromInput conn
--
-- execAction ("Author", "create") conn = putStrLn $ show $ createAuthorFromInput conn
-- execAction ("Author", "update") conn = putStrLn $ show $ updateAuthorFromInput conn
-- execAction ("Author", "read") conn = putStrLn $ show $ getAuthorFromInput conn
-- execAction ("Author", "list") conn = putStrLn $ show $ listAuthorsFromInput conn
--
-- execAction ("Usage", "create") conn = putStrLn $ show $ createUsageFromInput conn
-- execAction ("Usage", "update") conn = putStrLn $ show $ updateUsageFromInput conn
-- execAction ("Usage", "read") conn = putStrLn $ show $ getUsageFromInput conn
-- execAction ("Usage", "list") conn = putStrLn $ show $ listUsagesFromInput conn
--
-- execAction ("Program", "create") conn = putStrLn $ show $ createProgramFromInput conn
-- execAction ("Program", "update") conn = putStrLn $ show $ updateProgramFromInput conn
-- execAction ("Program", "read") conn = putStrLn $ show $ getProgramFromInput conn
-- execAction ("Program", "list") conn = putStrLn $ show $ listProgramsFromInput conn
-- execAction (_,_) conn = putStrLn "Wrong params"


printResTypeList conn = do
    res <- listTypeFromInput conn
    putStrLn $ show res

printResTypeCreate conn = do
    res <- createTypeFromInput conn
    putStrLn $ show res

printResTypeRead conn = do
    res <- getTypeFromInput conn
    putStrLn $ show res

printResTypeUpdate conn = do
    res <- updateTypeFromInput conn
    putStrLn $ show res


printUserList conn = do
    res <- listUsersFromInput conn
    putStrLn $ show res

printUserCreate conn = do
    res <- createUserFromInput conn
    putStrLn $ show res

printUserRead conn = do
    res <- getUserFromInput conn
    putStrLn $ show res

printUserUpdate conn = do
    res <- updateUserFromInput conn
    putStrLn $ show res


printAuthorList conn = do
    res <- listAuthorsFromInput conn
    putStrLn $ show res

printAuthorCreate conn = do
    res <- createAuthorFromInput conn
    putStrLn $ show res

printAuthorRead conn = do
    res <- getAuthorFromInput conn
    putStrLn $ show res

printAuthorUpdate conn = do
    res <- updateAuthorFromInput conn
    putStrLn $ show res


printUsageList conn = do
    res <- listUsagesFromInput conn
    putStrLn $ show res

printUsageCreate conn = do
    res <- createUsageFromInput conn
    putStrLn $ show res

printUsageRead conn = do
    res <- getUsageFromInput conn
    putStrLn $ show res

printUsageUpdate conn = do
    res <- updateUsageFromInput conn
    putStrLn $ show res


printProgramList conn = do
    res <- listProgramsFromInput conn
    putStrLn $ show res

printProgramCreate conn = do
    res <- createProgramFromInput conn
    putStrLn $ show res

printProgramRead conn = do
    res <- getProgramFromInput conn
    putStrLn $ show res

printProgramUpdate conn = do
    res <- updateProgramFromInput conn
    putStrLn $ show res


printTermsOfUseList conn = do
    res <- listTermsOfUseFromInput conn
    putStrLn $ show res

printTermsOfUseCreate conn = do
    res <- createTermsOfUseFromInput conn
    putStrLn $ show res

printTermsOfUseRead conn = do
    res <- getTermsOfUseFromInput conn
    putStrLn $ show res

printTermsOfUseUpdate conn = do
    res <- updateTermsOfUseFromInput conn
    putStrLn $ show res

promptLine :: String -> IO String
promptLine prompt = do
    putStrLn prompt
    getLine

handleInput :: IConnection a => a -> IO()
handleInput conn = do
    model <- promptLine "Choose the model: "
    operation <- promptLine "Choose the operation: "
    let action = (model, operation)
    case action of ("Type", "list") -> printResTypeList conn
                   ("Type", "create") -> printResTypeCreate conn
                   ("Type", "read") -> printResTypeRead conn
                   ("Type", "update") -> printResTypeUpdate conn

                   ("User", "list") -> printUserList conn
                   ("User", "create") -> printUserCreate conn
                   ("User", "read") -> printUserRead conn
                   ("User", "update") -> printUserUpdate conn

                   ("Author", "list") -> printAuthorList conn
                   ("Author", "create") -> printAuthorCreate conn
                   ("Author", "read") -> printAuthorRead conn
                   ("Author", "update") -> printAuthorUpdate conn

                   ("Usage", "list") -> printUsageList conn
                   ("Usage", "create") -> printUsageCreate conn
                   ("Usage", "read") -> printUsageRead conn
                   ("Usage", "update") -> printUsageUpdate conn

                   ("Program", "list") -> printProgramList conn
                   ("Program", "create") -> printProgramCreate conn
                   ("Program", "read") -> printProgramRead conn
                   ("Program", "update") -> printProgramUpdate conn

                   ("TermsOfUse", "list") -> printTermsOfUseList conn
                   ("TermsOfUse", "create") -> printTermsOfUseCreate conn
                   ("TermsOfUse", "read") -> printTermsOfUseRead conn
                   ("TermsOfUse", "update") -> printTermsOfUseUpdate conn
                   (_) -> putStrLn "Wrong params"

    putStrLn "exit(y/[n])"
    isExit <- getLine
    case isExit of
        "y" -> putStr "program closed"
        _  -> handleInput conn


main :: IO ()
main = do
    conn <- connectPostgreSQL "host=localhost dbname=univsoftsystem"-- user=postgres password=postgres"

    putStrLn "Available operations: read, list, create, update "
    putStrLn "Available models: Type, User, Author, Program, Usage, TermsOfUse "
    putStrLn "exit - close the app"

    handleInput conn
