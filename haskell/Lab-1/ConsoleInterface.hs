module ConsoleInterface where

import Data.Time
import Database.HDBC

import Type
import User
import Author
import Usage
import Program
import TermsOfUse

type ResTypeArray = [ResType]
type UserArray = [User]
type UsageArray = [Usage]
type AuthorArray = [Author]
type ProgramArray = [Program]

data DaoRes = ResTypeArray | UserArray | UsageArray | AuthorArray | ProgramArray | Bool

parseDate :: String -> LocalTime
parseDate dateStr = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" dateStr


listTypeFromInput :: IConnection a => a -> IO [ResType]
listTypeFromInput conn = readAllType conn

listUsersFromInput :: IConnection a => a -> IO [User]
listUsersFromInput conn = readAllUser conn

listAuthorsFromInput :: IConnection a => a -> IO [Author]
listAuthorsFromInput conn = readAllAuthor conn

listUsagesFromInput :: IConnection a => a -> IO [Usage]
listUsagesFromInput conn = readAllUsage conn

listProgramsFromInput :: IConnection a => a -> IO [Program]
listProgramsFromInput conn = readAllProgram conn

listTermsOfUseFromInput :: IConnection a => a -> IO [TermsOfUse]
listTermsOfUseFromInput conn = readAllTermsOfUse conn


getTypeFromInput :: IConnection a => a -> IO [ResType]
getTypeFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    readType (read id) conn

getUserFromInput :: IConnection a => a -> IO [User]
getUserFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    readUser (read id) conn

getAuthorFromInput :: IConnection a => a -> IO [Author]
getAuthorFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    readAuthor (read id) conn

getUsageFromInput :: IConnection a => a -> IO [Usage]
getUsageFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    readUsage (read id) conn

getProgramFromInput :: IConnection a => a -> IO [Program]
getProgramFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    readProgram (read id) conn

getTermsOfUseFromInput :: IConnection a => a -> IO [TermsOfUse]
getTermsOfUseFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    readTermsOfUse (read id) conn

createTypeFromInput :: IConnection a => a -> IO Bool
createTypeFromInput conn = do
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set description: "
    description <- getLine
    createType name description conn

createUserFromInput :: IConnection a => a -> IO Bool
createUserFromInput conn = do
    putStrLn "Set first name: "
    firstName <- getLine
    putStrLn "Set last name: "
    lastName <- getLine
    putStrLn "Set username: "
    username <- getLine
    putStrLn "Set password: "
    password <- getLine
    putStrLn "Set additional info: "
    info <- getLine
    createUser firstName lastName username password info conn

createAuthorFromInput :: IConnection a => a -> IO Bool
createAuthorFromInput conn = do
    putStrLn "Set first name: "
    firstName <- getLine
    putStrLn "Set last name: "
    lastName <- getLine
    putStrLn "Set company: "
    company <- getLine
    putStrLn "Set position: "
    position <- getLine
    putStrLn "Set additional info: "
    info <- getLine
    createAuthor firstName lastName company position info conn

createProgramFromInput :: IConnection a => a -> IO Bool
createProgramFromInput conn = do
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set annotation: "
    annotation <- getLine
    putStrLn "Set version: "
    version <- getLine
    putStrLn "Set author id: "
    authorId <- getLine
    putStrLn "Set type id: "
    typeId <- getLine
    putStrLn "Set terms of use id"
    termsOfUseId <- getLine
    putStrLn "Set distribution url: "
    distUrl <- getLine
    createProgram name annotation version (read authorId) (read typeId) (read termsOfUseId) distUrl conn


createUsageFromInput :: IConnection a => a -> IO Bool
createUsageFromInput conn = do
    putStrLn "Set user id: "
    userId <- getLine
    putStrLn "Set program id: "
    programId <- getLine
    putStrLn "Set time. Format YYYY-MM-DD HH:mm "
    time <- getLine
    putStrLn "Set action:"
    action <- getLine
    putStrLn "Set info:"
    info <- getLine
    createUsage (read userId) (read programId) (parseDate time) action info conn


createTermsOfUseFromInput :: IConnection a => a -> IO Bool
createTermsOfUseFromInput conn = do
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set condition: "
    condition <- getLine
    putStrLn "Set start date. Format YYYY-MM-DD HH:mm "
    startDate <- getLine
    putStrLn "Set period (in days)"
    period <- getLine
    createTermsOfUse name condition (parseDate startDate) (read period) conn

updateTypeFromInput :: IConnection a => a -> IO Bool
updateTypeFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set description: "
    description <- getLine
    updateType (ResType (read id) name description) conn

updateUserFromInput :: IConnection a => a -> IO Bool
updateUserFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set first name: "
    firstName <- getLine
    putStrLn "Set last name: "
    lastName <- getLine
    putStrLn "Set username: "
    username <- getLine
    putStrLn "Set password: "
    password <- getLine
    putStrLn "Set additional info: "
    info <- getLine
    updateUser (User (read id) firstName lastName username password info) conn

updateAuthorFromInput :: IConnection a => a -> IO Bool
updateAuthorFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set first name: "
    firstName <- getLine
    putStrLn "Set last name: "
    lastName <- getLine
    putStrLn "Set company: "
    company <- getLine
    putStrLn "Set position: "
    position <- getLine
    putStrLn "Set additional info: "
    info <- getLine
    updateAuthor (Author (read id) firstName lastName company position info) conn


updateProgramFromInput :: IConnection a => a -> IO Bool
updateProgramFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set annotation: "
    annotation <- getLine
    putStrLn "Set version: "
    version <- getLine
    putStrLn "Set author id: "
    authorId <- getLine
    putStrLn "Set type id: "
    typeId <- getLine
    putStrLn "Set terms of use id"
    termsOfUseId <- getLine
    putStrLn "Set distribution url: "
    distUrl <- getLine
    updateProgram (Program (read id) name annotation version (read authorId) (read typeId) (read termsOfUseId) distUrl) conn

updateUsageFromInput :: IConnection a => a -> IO Bool
updateUsageFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set user id: "
    userId <- getLine
    putStrLn "Set program id: "
    programId <- getLine
    putStrLn "Set time. Format YYYY-MM-DD HH:mm "
    time <- getLine
    putStrLn "Set action:"
    action <- getLine
    putStrLn "Set info:"
    info <- getLine
    updateUsage (Usage (read id) (read userId) (read programId) (parseDate time) action info) conn

updateTermsOfUseFromInput :: IConnection a => a -> IO Bool
updateTermsOfUseFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set condition: "
    condition <- getLine
    putStrLn "Set start date. Format YYYY-MM-DD HH:mm "
    startDate <- getLine
    putStrLn "Set period (in days)"
    period <- getLine
    updateTermsOfUse (TermsOfUse (read id) name condition (parseDate startDate) (read period)) conn