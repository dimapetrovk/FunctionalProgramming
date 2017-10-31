module TermsOfUse where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BS

data TermsOfUse = TermsOfUse {id :: Integer, name, condition :: String, startDate :: LocalTime, period :: Integer} deriving (Show)

createTermsOfUse :: IConnection a => String -> String -> LocalTime -> Integer -> a -> IO Bool
createTermsOfUse name condition startDate period conn =
    withTransaction conn (createSc' name condition startDate period)

createSc' name condition startDate period conn = do
    changed <- run conn query [SqlString name, SqlString condition, SqlLocalTime startDate, SqlInteger period]
    return $ changed == 1
    where
        query = "insert into terms_of_use (name, condition, start_date, period)" ++
            " values (?, ?, ?, ?)"

readTermsOfUse :: IConnection a => Integer -> a -> IO [TermsOfUse]
readTermsOfUse id conn = do
  rslt <- quickQuery' conn query [SqlInteger id]
  return $ map toDataObject rslt
  where
      query = "select id, name, condition, start_date, period from terms_of_use where id = ?"
      toDataObject [SqlInteger uid, SqlByteString name, SqlByteString condition, SqlLocalTime startDate, SqlInteger period] =
        TermsOfUse uid (BS.unpack name) (BS.unpack condition) startDate period
      toDataObject x = error $ "Unexpected result: " ++ show x


readAllTermsOfUse :: IConnection a => a -> IO [TermsOfUse]
readAllTermsOfUse conn = do
  rslt <- quickQuery' conn query []
  return $ map toDataObject rslt
  where
    query = "select id, name, condition, start_date, period from terms_of_use order by id"
    toDataObject [SqlInteger uid, SqlByteString name, SqlByteString condition, SqlLocalTime startDate, SqlInteger period] =
        TermsOfUse uid (BS.unpack name) (BS.unpack condition) startDate period
    toDataObject x = error $ "Unexpected result: " ++ show x

updateTermsOfUse :: IConnection a => TermsOfUse -> a -> IO Bool
updateTermsOfUse (TermsOfUse uid name condition startDate period) conn =
    withTransaction conn (updateSd' uid name condition startDate period)

updateSd' uid name condition startDate period conn = do
  changed <- run conn query
                    [SqlString name, SqlString condition, SqlLocalTime startDate, SqlInteger period, SqlInteger uid]
  return $ changed == 1
  where
    query = "update terms_of_use set name=?, condition=?, startDate=?, period=? where id = ?"