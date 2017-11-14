module Author where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BS


data Author = Author {id :: Integer, firstName, lastName, company, position, info::String} deriving (Show)

createAuthor :: IConnection a => String -> String -> String -> String -> String -> a -> IO Bool
createAuthor firstName lastName company position info conn =
    withTransaction conn (createSc' firstName lastName company position info)

createSc' firstName lastName company position info conn = do
    changed <- run conn query [SqlString firstName, SqlString lastName, SqlString company, SqlString position, SqlString info]
    return $ changed == 1
    where
        query = "insert into author (firstname, lastname, company, position, info)" ++
            " values (?, ?, ?, ?, ?)"

readAuthor :: IConnection a => Integer -> a -> IO [Author]
readAuthor id conn = do
  rslt <- quickQuery' conn query [SqlInteger id]
  return $ map toDataObject rslt
  where
      query = "select id, firstname, lastname, company, position, info from author where id = ?"
      toDataObject [SqlInteger uid, SqlByteString firstName, SqlByteString lastName, SqlByteString company, SqlByteString position, SqlByteString info] =
        Author uid (BS.unpack firstName) (BS.unpack lastName) (BS.unpack company) (BS.unpack position) (BS.unpack info)
      toDataObject x = error $ "Unexpected result: " ++ show x


readAllAuthor :: IConnection a => a -> IO [Author]
readAllAuthor conn = do
  rslt <- quickQuery' conn query []
  return $ map toDataObject rslt
  where
    query = "select id, firstname, lastname, company, position, info from author order by id"
    toDataObject [SqlInteger uid, SqlByteString firstName, SqlByteString lastName, SqlByteString company, SqlByteString position, SqlByteString info] =
            Author uid (BS.unpack firstName) (BS.unpack lastName) (BS.unpack company) (BS.unpack position) (BS.unpack info)
    toDataObject x = error $ "Unexpected result: " ++ show x

updateAuthor :: IConnection a => Author -> a -> IO Bool
updateAuthor (Author uid firstName lastName company position info) conn =
    withTransaction conn (updateSd' uid firstName lastName company position info)

updateSd' uid firstName lastName company position info conn = do
  changed <- run conn query
                 [SqlString firstName, SqlString lastName,SqlString company,  SqlString position, SqlString info, SqlInteger uid]
  return $ changed == 1
  where
    query = "update author set first_name=?, last_name=?, company=?, position=?, info=? where id = ?"