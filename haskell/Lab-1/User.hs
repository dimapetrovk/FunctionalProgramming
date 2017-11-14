module User where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BS

data User = User {id :: Integer, firstName, lastName, email, password, info :: String} deriving (Show)

createUser :: IConnection a => String -> String -> String -> String -> String -> a -> IO Bool
createUser firstName lastName email password info conn =
    withTransaction conn (createSc' firstName lastName email password info)

createSc' firstName lastName email password info conn = do
    changed <- run conn query [SqlString firstName, SqlString lastName, SqlString email, SqlString password, SqlString info]
    return $ changed == 1
    where
        query = "insert into users (firstname, lastname, email, password, info) values (?, ?, ?, ?, ?)"

readUser :: IConnection a => Integer -> a -> IO [User]
readUser id conn = do
  rslt <- quickQuery' conn query [SqlInteger id]
  return $ map toDataObject rslt
  where
      query = "select id, firstname, lastname, email, password, info from users where id = ?"
      toDataObject [SqlInteger uid, SqlByteString firstName, SqlByteString lastName, SqlByteString email, SqlByteString password, SqlByteString info] =
        User uid (BS.unpack firstName) (BS.unpack lastName) (BS.unpack email) (BS.unpack password) (BS.unpack info)
      toDataObject x = error $ "Unexpected result: " ++ show x


readAllUser :: IConnection a => a -> IO [User]
readAllUser conn = do
  rslt <- quickQuery' conn query []
  return $ map toDataObject rslt
  where
    query = "select id, firstname, lastname, email, password, info from users order by id"
    toDataObject [SqlInteger uid, SqlByteString firstName, SqlByteString lastName, SqlByteString email, SqlByteString password, SqlByteString info] =
        User uid (BS.unpack firstName) (BS.unpack lastName) (BS.unpack email) (BS.unpack password) (BS.unpack info)
    toDataObject x = error $ "Unexpected result: " ++ show x

updateUser :: IConnection a => User -> a -> IO Bool
updateUser (User uid firstName lastName email password info) conn =
    withTransaction conn (updateSd' uid firstName lastName email password info)

updateSd' uid firstName lastName email password info conn = do
  changed <- run conn query
                 [SqlString firstName, SqlString lastName, SqlString email, SqlString password, SqlString info, SqlInteger uid]
  return $ changed == 1
  where
    query = "update users set firstname=?, lastname=?, email=?, password=?, info=? where id = ?"